//! Parser that converts scanned lines into an event stream.
const std = @import("std");

const Error = @import("Error.zig");
const EventModel = @import("Event.zig");
const Options = @import("Options.zig");
const Scanner = @import("Scanner.zig");
const Token = @import("Token.zig");
const Composer = @import("Composer.zig");

pub const Parser = @This();

const EmitSink = enum {
    /// Normal path: buffer in `events` or feed `stream` directly.
    direct,
    /// Flow collections may `insert` events; buffer locally then flush to `stream`.
    flow_scratch,
};

allocator: std.mem.Allocator,
scanned: Scanner.ScannedDocument,
options: Options.Parse,
/// Allocator for rebuilt scalar buffers (defaults to `allocator`).
scalar_allocator: std.mem.Allocator,
scalar_pool_backed: bool = false,
stream: ?*Composer.Stream = null,
emit_sink: EmitSink = .direct,
flow_scratch: std.ArrayListUnmanaged(EventModel.Event) = .empty,
pending_scalar_tag: ?[]const u8 = null,
events: std.ArrayListUnmanaged(EventModel.Event) = .empty,
index: usize = 0,

pub fn init(allocator: std.mem.Allocator, scanned: Scanner.ScannedDocument, options: Options.Parse) Parser {
    return initWithScalarPool(allocator, scanned, options, null, null);
}

pub fn initWithScalarPool(
    allocator: std.mem.Allocator,
    scanned: Scanner.ScannedDocument,
    options: Options.Parse,
    scalar_pool: ?*@import("Document.zig").OwnedScalarPool,
    stream: ?*Composer.Stream,
) Parser {
    return .{
        .allocator = allocator,
        .scanned = scanned,
        .options = options,
        .scalar_allocator = if (scalar_pool) |p| p.arena.allocator() else allocator,
        .scalar_pool_backed = scalar_pool != null,
        .stream = stream,
    };
}

pub fn deinit(self: *Parser) void {
    self.freeEventSlice(self.events.items);
    self.events.deinit(self.allocator);
    self.freeEventSlice(self.flow_scratch.items);
    self.flow_scratch.deinit(self.allocator);
    self.scanned.deinit(self.allocator);
    self.* = undefined;
}

fn freeEventSlice(self: *Parser, slice: []EventModel.Event) void {
    for (slice) |ev| {
        switch (ev.kind) {
            .scalar => {
                if (ev.data.scalar.value_owned and !ev.data.scalar.pool_backed) {
                    self.allocator.free(ev.data.scalar.value);
                }
                if (ev.data.scalar.anchor) |anchor| self.allocator.free(anchor);
                if (ev.data.scalar.tag) |tag| self.allocator.free(tag);
            },
            .alias => self.allocator.free(ev.data.alias.name),
            .sequence_start => {
                if (ev.data.sequence_start.anchor) |a| self.allocator.free(a);
                if (ev.data.sequence_start.tag) |t| self.allocator.free(t);
            },
            .mapping_start => {
                if (ev.data.mapping_start.anchor) |a| self.allocator.free(a);
                if (ev.data.mapping_start.tag) |t| self.allocator.free(t);
            },
            else => {},
        }
    }
}

pub fn parse(self: *Parser) ![]EventModel.Event {
    if (self.stream != null) return Error.Parse.UnexpectedToken;
    try self.runParseLoop();
    const owned = try self.events.toOwnedSlice(self.allocator);
    self.events = .empty;
    return owned;
}

pub fn parseComposed(self: *Parser) !void {
    if (self.stream == null) return Error.Parse.UnexpectedToken;
    try self.runParseLoop();
}

fn runParseLoop(self: *Parser) !void {
    if (self.stream == null) {
        const line_count = self.scanned.lines.items.len;
        if (line_count > 0) {
            try self.events.ensureTotalCapacity(self.allocator, line_count * 4 + 32);
        }
    }

    try self.pushSimple(.stream_start, .{});

    while (self.index < self.scanned.lines.items.len) {
        const line = self.scanned.lines.items[self.index];
        if (line.kind == .document_end) {
            self.index += 1;
            continue;
        }

        try self.pushDocumentStart(line.started_explicit or line.kind == .empty_document);
        if (line.kind == .empty_document) {
            try self.pushScalar("", .plain, null, .{});
            self.index += 1;
        } else {
            try self.parseBlockValue(line.indent, true);
            if (self.index < self.scanned.lines.items.len and
                self.scanned.lines.items[self.index].kind != .document_end)
            {
                return Error.Parse.UnexpectedToken;
            }
        }
        try self.pushSimple(.document_end, .{});
        if (self.index < self.scanned.lines.items.len and
            self.scanned.lines.items[self.index].kind == .document_end)
        {
            self.index += 1;
        }
    }

    try self.pushSimple(.stream_end, .{});
}

fn usesStreamFeed(self: *Parser) bool {
    return self.stream != null and self.emit_sink == .direct;
}

fn activeEventList(self: *Parser) *std.ArrayListUnmanaged(EventModel.Event) {
    if (self.emit_sink == .flow_scratch) return &self.flow_scratch;
    return &self.events;
}

fn flushFlowScratch(self: *Parser) !void {
    if (self.flow_scratch.items.len == 0) return;
    if (self.stream) |s| {
        for (self.flow_scratch.items) |ev| {
            try s.feed(self.allocator, ev);
        }
    } else {
        try self.events.appendSlice(self.allocator, self.flow_scratch.items);
    }
    self.flow_scratch.clearRetainingCapacity();
}

fn commitFlowScratch(self: *Parser) !void {
    if (self.stream != null) try self.flushFlowScratch();
}

fn eventInsert(self: *Parser, index: usize, ev: EventModel.Event) !void {
    try self.activeEventList().insert(self.allocator, index, ev);
}

fn emitEvent(self: *Parser, ev_in: EventModel.Event) !void {
    var ev = ev_in;
    if (ev.kind == .scalar and self.pending_scalar_tag != null) {
        ev.data.scalar.tag = try self.allocator.dupe(u8, self.pending_scalar_tag.?);
        self.pending_scalar_tag = null;
    }
    if (self.usesStreamFeed()) {
        try self.stream.?.feed(self.allocator, ev);
        return;
    }
    try self.activeEventList().append(self.allocator, ev);
}

fn parseBlockValue(self: *Parser, indent: usize, is_root: bool) anyerror!void {
    if (self.index >= self.scanned.lines.items.len) return;
    const line = self.scanned.lines.items[self.index];
    if (line.kind == .document_end or line.kind == .empty_document or line.kind == .explicit_value) return;
    if (line.indent < indent) return;

    if (line.indent == indent and line.kind == .sequence_item) {
        try self.parseBlockSequence(indent);
        return;
    }
    if (line.indent == indent and line.kind == .mapping_entry) {
        try self.parseBlockMapping(indent);
        return;
    }

    if (line.kind == .scalar and (line.style == .literal or line.style == .folded)) {
        const block = try self.collectBlockScalar(line.indent, line.style, line.value, line.line_no);
        try self.pushBlockScalar(block, line.style, null, line.span);
        return;
    }

    if (line.kind == .scalar and (line.style == .single_quoted or line.style == .double_quoted)) {
        const multiline = try self.collectMultilineQuotedScalar(indent, line.style);
        defer self.allocator.free(multiline);
        try self.parseScalarLikeValue(multiline, line.style, line.line_no, line.indent);
        return;
    }

    // Plain scalar with multiline continuation
    if (line.kind == .scalar and line.style == .plain) {
        const min_cont = if (is_root) @as(usize, 0) else indent;
        const joined = try self.collectPlainContinuation(min_cont, line.value);
        defer self.allocator.free(joined);

        // Check if value is just tags/anchors — look ahead for block structure
        const stripped = stripTagPrefix(std.mem.trim(u8, joined, " "));
        const anc = extractLeadingAnchor(stripped);
        // Tags may follow the anchor (`&a4 !!map`), so strip both before deciding
        // that the line is only node properties.
        const rest_after = std.mem.trim(u8, stripTagPrefix(stripAnchorPrefix(stripped)), " ");
        if (rest_after.len == 0 and self.index < self.scanned.lines.items.len) {
            const next = self.scanned.lines.items[self.index];
            const anchor_to_attach = if (anc.name.len > 0) anc.name else null;
            if (next.kind == .sequence_item and next.indent >= indent) {
                if (anchor_to_attach) |a| {
                    try self.emitEvent( .{
                        .kind = .sequence_start,
                        .data = .{ .sequence_start = .{
                            .style = .block,
                            .anchor = try self.allocator.dupe(u8, a),
                        } },
                    });
                    while (self.index < self.scanned.lines.items.len) {
                        const sl = self.scanned.lines.items[self.index];
                        if (sl.indent != next.indent or sl.kind != .sequence_item) break;
                        self.index += 1;
                        if (sl.value.len == 0) {
                            if (self.index < self.scanned.lines.items.len and self.scanned.lines.items[self.index].indent > next.indent) {
                                try self.parseBlockValue(self.scanned.lines.items[self.index].indent, false);
                            } else {
                                try self.pushScalar("", .plain, null, sl.span);
                            }
                        } else {
                            try self.parseScalarLikeValue(sl.value, sl.style, sl.line_no, sl.indent + 2);
                        }
                    }
                    try self.pushSimple(.sequence_end, .{});
                } else {
                    try self.parseBlockSequence(next.indent);
                }
                return;
            }
            if (next.kind == .mapping_entry and next.indent >= indent) {
                if (anchor_to_attach) |a| {
                    try self.emitEvent( .{
                        .kind = .mapping_start,
                        .data = .{ .mapping_start = .{
                            .style = .block,
                            .anchor = try self.allocator.dupe(u8, a),
                        } },
                    });
                    while (self.index < self.scanned.lines.items.len) {
                        const ml = self.scanned.lines.items[self.index];
                        if (ml.indent != next.indent or ml.kind != .mapping_entry) break;
                        try self.emitSingleMappingEntry(ml, next.indent);
                    }
                    try self.pushSimple(.mapping_end, .{});
                } else {
                    try self.parseBlockMapping(next.indent);
                }
                return;
            }
        }

        try self.parseScalarLikeValue(joined, .plain, line.line_no, line.indent);
        return;
    }

    try self.parseScalarLikeValue(line.value, line.style, line.line_no, line.indent);
    self.index += 1;
}

fn parseBlockSequence(self: *Parser, indent: usize) anyerror!void {
    try self.emitEvent( .{
        .kind = .sequence_start,
        .data = .{ .sequence_start = .{ .style = .block } },
    });

    while (self.index < self.scanned.lines.items.len) {
        const line = self.scanned.lines.items[self.index];
        if (line.indent != indent or line.kind != .sequence_item) break;

        // Empty value — look ahead for nested block
        if (line.value.len == 0) {
            self.index += 1;
            if (self.index < self.scanned.lines.items.len and self.scanned.lines.items[self.index].indent > indent) {
                try self.parseBlockValue(self.scanned.lines.items[self.index].indent, false);
            } else {
                try self.pushScalar("", .plain, null, line.span);
            }
            continue;
        }

        // Check for virtual-empty value (only anchor/tag, no real content)
        if (line.style == .plain and isVirtualEmptyValue(line.value, .plain)) {
            const raw_trimmed = std.mem.trim(u8, line.value, " ");
            const vs = stripTagPrefix(raw_trimmed);
            const vs_anc = extractLeadingAnchor(vs);
            const seq_anc: ?[]const u8 = if (vs_anc.name.len > 0) vs_anc.name else null;
            self.index += 1;
            if (self.index < self.scanned.lines.items.len) {
                const nx = self.scanned.lines.items[self.index];
                if (nx.indent > indent) {
                    try self.emitVirtualEmptyBlock(nx, seq_anc);
                    continue;
                }
            }
            try self.prepareScalarTag(canonicalTag(raw_trimmed));
            try self.pushScalar("", .plain, seq_anc, line.span);
            continue;
        }

        // Nested sequence: value starts with "- "
        const trimmed_val = std.mem.trimStart(u8, line.value, " ");
        if (trimmed_val.len >= 2 and trimmed_val[0] == '-' and (trimmed_val[1] == ' ' or trimmed_val[1] == '\t')) {
            const nested_indent = indent + 2;
            try self.emitEvent( .{
                .kind = .sequence_start,
                .data = .{ .sequence_start = .{ .style = .block } },
            });
            // Emit inline first item
            const inner_item = std.mem.trimStart(u8, trimmed_val[2..], " \t");
            if (inner_item.len == 0) {
                self.index += 1;
                if (self.index < self.scanned.lines.items.len and self.scanned.lines.items[self.index].indent > nested_indent) {
                    try self.parseBlockValue(self.scanned.lines.items[self.index].indent, false);
                } else {
                    try self.pushScalar("", .plain, null, line.span);
                }
            } else if (inner_item.len >= 2 and inner_item[0] == '-' and (inner_item[1] == ' ' or inner_item[1] == '\t')) {
                try self.emitNestedSequenceValue(inner_item, nested_indent, line.line_no, line.span);
                self.index += 1;
            } else {
                const is = detectInlineStyle(inner_item);
                if (is == .plain and Scanner.findInlineMappingColon(stripAnchorPrefix(stripTagPrefix(inner_item))) != null) {
                    try self.emitInlineMappingValue(inner_item, nested_indent, line.line_no);
                    self.index += 1;
                } else {
                    try self.parseScalarLikeValue(inner_item, is, line.line_no, nested_indent);
                    self.index += 1;
                }
            }
            // Consume continuation sequence items at nested_indent
            while (self.index < self.scanned.lines.items.len) {
                const nl = self.scanned.lines.items[self.index];
                if (nl.indent != nested_indent or nl.kind != .sequence_item) break;
                self.index += 1;
                if (nl.value.len == 0) {
                    if (self.index < self.scanned.lines.items.len and self.scanned.lines.items[self.index].indent > nested_indent) {
                        try self.parseBlockValue(self.scanned.lines.items[self.index].indent, false);
                    } else {
                        try self.pushScalar("", .plain, null, nl.span);
                    }
                } else {
                    try self.parseScalarLikeValue(nl.value, nl.style, nl.line_no, nested_indent + 2);
                }
            }
            try self.pushSimple(.sequence_end, .{});
            continue;
        }

        // Block scalar value
        if (line.style == .literal or line.style == .folded) {
            const block = try self.collectBlockScalar(line.indent, line.style, line.value, line.line_no);
            try self.pushBlockScalar(block, line.style, null, line.span);
            continue;
        }

        // `- ? : x` / `- ? earth: blue` is an explicit-key mapping, not a scalar.
        if (line.style == .plain and trimmed_val.len >= 1 and trimmed_val[0] == '?' and
            (trimmed_val.len == 1 or trimmed_val[1] == ' ' or trimmed_val[1] == '\t'))
        {
            try self.parseCompactExplicit(line, indent);
            continue;
        }

        // Check for inline mapping colon in the value
        if (line.style == .plain and line.value.len > 0 and line.value[0] != '[' and line.value[0] != '{') {
            const stripped_val = stripTagPrefix(std.mem.trim(u8, line.value, " "));
            const no_anchor = stripAnchorPrefix(stripped_val);
            if (Scanner.findInlineMappingColon(no_anchor)) |_| {
                try self.emitSequenceItemMapping(line, indent);
                continue;
            }
        }

        // For plain scalars, collect multiline continuation
        if (line.style == .plain) {
            var final_text = try self.collectPlainContinuation(indent + 1, line.value);
            defer self.allocator.free(final_text);

            // Absorb sequence_item lines at indent > seq_indent as plain text (AB8U).
            // A comment ends that plain scalar, so later dashes stay structure.
            if (!line.ends_with_comment) {
                while (self.index < self.scanned.lines.items.len) {
                    const nl = self.scanned.lines.items[self.index];
                    if (nl.after_comment) break;
                    if (nl.kind != .sequence_item or nl.indent <= indent) break;
                    const trimmed = std.mem.trim(u8, nl.value, " ");
                    const new_buf = try self.allocator.alloc(u8, final_text.len + 3 + trimmed.len);
                    @memcpy(new_buf[0..final_text.len], final_text);
                    new_buf[final_text.len] = ' ';
                    new_buf[final_text.len + 1] = '-';
                    new_buf[final_text.len + 2] = ' ';
                    @memcpy(new_buf[final_text.len + 3 ..][0..trimmed.len], trimmed);
                    self.allocator.free(final_text);
                    final_text = new_buf[0 .. final_text.len + 3 + trimmed.len];
                    self.index += 1;
                }
            }
            try self.parseScalarLikeValue(final_text, .plain, line.line_no, line.indent + 2);
        } else {
            try self.parseScalarLikeValue(line.value, line.style, line.line_no, line.indent + 2);
            self.index += 1;
        }
    }

    try self.pushSimple(.sequence_end, .{});
}

fn parseCompactExplicit(self: *Parser, line: Scanner.ScannedLine, seq_indent: usize) anyerror!void {
    const raw = std.mem.trim(u8, line.value, " \t");
    const rest = std.mem.trim(u8, if (raw.len <= 1) "" else raw[1..], " \t");
    try self.emitEvent( .{
        .kind = .mapping_start,
        .data = .{ .mapping_start = .{ .style = .block } },
    });
    try self.parseExplicitKeyNode(rest, line, seq_indent);
    try self.parseExplicitValueLine(seq_indent, line.span);
    try self.pushSimple(.mapping_end, .{});
}

fn parseExplicitKeyNode(self: *Parser, rest: []const u8, line: Scanner.ScannedLine, seq_indent: usize) anyerror!void {
    if (rest.len == 0) {
        self.index += 1;
        if (self.index < self.scanned.lines.items.len and self.scanned.lines.items[self.index].indent > seq_indent) {
            try self.parseBlockValue(self.scanned.lines.items[self.index].indent, false);
        } else {
            try self.pushScalar("", .plain, null, line.span);
        }
        return;
    }

    self.index += 1;
    if (rest[0] == ':' and (rest.len == 1 or rest[1] == ' ' or rest[1] == '\t')) {
        const inner = if (rest.len <= 1) "" else std.mem.trimStart(u8, rest[2..], " \t");
        try self.emitEvent( .{
            .kind = .mapping_start,
            .data = .{ .mapping_start = .{ .style = .block } },
        });
        try self.pushScalar("", .plain, null, line.span);
        if (inner.len == 0) {
            try self.pushScalar("", .plain, null, line.span);
        } else if (Scanner.findInlineMappingColon(inner) != null) {
            try self.emitInlineMappingValue(inner, seq_indent + 2, line.line_no);
        } else {
            try self.parseScalarLikeValue(inner, detectInlineStyle(inner), line.line_no, seq_indent + 2);
        }
        try self.pushSimple(.mapping_end, .{});
        return;
    }
    if (isCompactSequence(rest)) {
        try self.emitCompactSequence(seq_indent + 2, rest, line.span, line.line_no);
        return;
    }
    if (Scanner.findInlineMappingColon(rest) != null and (rest[0] != '[' and rest[0] != '{')) {
        try self.emitInlineMappingValue(rest, seq_indent + 2, line.line_no);
        return;
    }
    try self.parseScalarLikeValue(rest, detectInlineStyle(rest), line.line_no, seq_indent + 2);
}

fn parseExplicitValueLine(self: *Parser, seq_indent: usize, span: @import("Span.zig")) anyerror!void {
    const col = seq_indent + 2;
    if (self.index >= self.scanned.lines.items.len) {
        try self.pushScalar("", .plain, null, span);
        return;
    }
    const next = self.scanned.lines.items[self.index];
    const value_line = next.indent == col and (next.kind == .explicit_value or
        (next.kind == .mapping_entry and next.key.len == 0 and !next.explicit_key));
    if (!value_line) {
        try self.pushScalar("", .plain, null, span);
        return;
    }
    const text = std.mem.trim(u8, next.value, " \t");
    self.index += 1;
    if (text.len == 0) {
        if (self.index < self.scanned.lines.items.len and self.scanned.lines.items[self.index].indent > col) {
            try self.parseBlockValue(self.scanned.lines.items[self.index].indent, false);
        } else {
            try self.pushScalar("", .plain, null, next.span);
        }
        return;
    }
    if (isCompactSequence(text)) {
        try self.emitCompactSequence(col, text, next.span, next.line_no);
        return;
    }
    if ((text[0] == '[' or text[0] == '{') or Scanner.findInlineMappingColon(text) != null) {
        if (text[0] == '[' or text[0] == '{') {
            try self.parseFlow(text, next.line_no, col);
            try self.commitFlowScratch();
        } else {
            try self.emitInlineMappingValue(text, col, next.line_no);
        }
        return;
    }
    try self.parseScalarLikeValue(text, next.style, next.line_no, col);
}

fn emitSequenceItemMapping(self: *Parser, line: Scanner.ScannedLine, seq_indent: usize) anyerror!void {
    const original = std.mem.trim(u8, line.value, " ");
    const col_idx = Scanner.findInlineMappingColon(original) orelse unreachable;
    const key = std.mem.trimEnd(u8, original[0..col_idx], " \t");
    const raw_val = std.mem.trimStart(u8, original[col_idx + 1 ..], " \t");

    try self.emitEvent( .{
        .kind = .mapping_start,
        .data = .{ .mapping_start = .{ .style = .block } },
    });

    try self.parseScalarLikeValue(key, detectInlineStyle(key), line.line_no, line.indent);

    // Emit first value
    if (raw_val.len == 0) {
        self.index += 1;
        if (self.index < self.scanned.lines.items.len and self.scanned.lines.items[self.index].indent > seq_indent) {
            try self.parseBlockValue(self.scanned.lines.items[self.index].indent, false);
        } else {
            try self.pushScalar("", .plain, null, .{});
        }
    } else {
        const vs = detectInlineStyle(raw_val);
        if (vs == .literal or vs == .folded) {
            const block = try self.collectBlockScalar(seq_indent + 2, vs, raw_val, line.line_no);
            try self.pushBlockScalar(block, vs, null, .{});
        } else if (vs == .plain) {
            // Fold indented continuation lines into the plain value
            // (`- message: …\n    specified`).
            const joined = try self.collectPlainContinuation(seq_indent + 1, raw_val);
            defer self.allocator.free(joined);
            try self.parseScalarLikeValue(joined, .plain, line.line_no, seq_indent + 2);
        } else {
            try self.parseScalarLikeValue(raw_val, vs, line.line_no, seq_indent + 2);
            self.index += 1;
        }
    }

    // Consume continuation mapping entries at indent > seq_indent
    while (self.index < self.scanned.lines.items.len) {
        const next = self.scanned.lines.items[self.index];
        if (next.kind != .mapping_entry or next.indent <= seq_indent) break;
        try self.emitSingleMappingEntry(next, next.indent);
    }

    try self.pushSimple(.mapping_end, .{});
}

fn emitVirtualEmptyBlock(self: *Parser, next_line: Scanner.ScannedLine, anchor: ?[]const u8) anyerror!void {
    if (next_line.kind == .sequence_item) {
        try self.emitEvent( .{
            .kind = .sequence_start,
            .data = .{ .sequence_start = .{
                .style = .block,
                .anchor = if (anchor) |a| try self.allocator.dupe(u8, a) else null,
            } },
        });
        const seq_indent = next_line.indent;
        while (self.index < self.scanned.lines.items.len) {
            const sl = self.scanned.lines.items[self.index];
            if (sl.indent != seq_indent or sl.kind != .sequence_item) break;
            self.index += 1;
            if (sl.value.len == 0) {
                if (self.index < self.scanned.lines.items.len and self.scanned.lines.items[self.index].indent > seq_indent) {
                    try self.parseBlockValue(self.scanned.lines.items[self.index].indent, false);
                } else {
                    try self.pushScalar("", .plain, null, sl.span);
                }
            } else {
                try self.parseScalarLikeValue(sl.value, sl.style, sl.line_no, sl.indent + 2);
            }
        }
        try self.pushSimple(.sequence_end, .{});
    } else if (next_line.kind == .mapping_entry) {
        try self.emitEvent( .{
            .kind = .mapping_start,
            .data = .{ .mapping_start = .{
                .style = .block,
                .anchor = if (anchor) |a| try self.allocator.dupe(u8, a) else null,
            } },
        });
        const map_indent = next_line.indent;
        while (self.index < self.scanned.lines.items.len) {
            const ml = self.scanned.lines.items[self.index];
            if (ml.indent != map_indent or ml.kind != .mapping_entry) break;
            try self.emitSingleMappingEntry(ml, map_indent);
        }
        try self.pushSimple(.mapping_end, .{});
    } else {
        try self.parseBlockValue(next_line.indent, false);
    }
}

fn emitInlineMappingValue(self: *Parser, text: []const u8, parent_indent: usize, line_no: usize) anyerror!void {
    var working = stripTagPrefix(std.mem.trim(u8, text, " "));
    const anchor_info = extractLeadingAnchor(working);
    if (anchor_info.name.len > 0) working = anchor_info.rest;
    const col_idx = Scanner.findInlineMappingColon(working) orelse return;
    const key = std.mem.trimEnd(u8, working[0..col_idx], " \t");
    const raw_val = std.mem.trimStart(u8, working[col_idx + 1 ..], " \t");

    try self.emitEvent( .{
        .kind = .mapping_start,
        .data = .{ .mapping_start = .{ .style = .block } },
    });
    const ks = detectInlineStyle(key);
    try self.pushScalarNormalized(key, ks, null, .{});
    if (raw_val.len == 0) {
        try self.pushScalar("", .plain, null, .{});
    } else {
        try self.parseScalarLikeValue(raw_val, detectInlineStyle(raw_val), line_no, parent_indent + 2);
    }
    try self.pushSimple(.mapping_end, .{});
}

fn emitNestedSequenceValue(self: *Parser, value: []const u8, parent_indent: usize, line_no: usize, span: @import("Span.zig")) anyerror!void {
    const trimmed = std.mem.trimStart(u8, value, " ");
    if (trimmed.len >= 2 and trimmed[0] == '-' and (trimmed[1] == ' ' or trimmed[1] == '\t')) {
        try self.emitEvent( .{
            .kind = .sequence_start,
            .data = .{ .sequence_start = .{ .style = .block } },
        });
        const inner = std.mem.trimStart(u8, trimmed[2..], " \t");
        if (inner.len == 0) {
            try self.pushScalar("", .plain, null, span);
        } else if (inner.len >= 2 and inner[0] == '-' and (inner[1] == ' ' or inner[1] == '\t')) {
            try self.emitNestedSequenceValue(inner, parent_indent + 2, line_no, span);
        } else {
            try self.parseScalarLikeValue(inner, detectInlineStyle(inner), line_no, parent_indent + 2);
        }
        try self.pushSimple(.sequence_end, span);
    } else {
        try self.parseScalarLikeValue(trimmed, detectInlineStyle(trimmed), line_no, parent_indent + 2);
    }
}

fn parseBlockMapping(self: *Parser, indent: usize) anyerror!void {
    try self.emitEvent( .{
        .kind = .mapping_start,
        .data = .{ .mapping_start = .{ .style = .block } },
    });

    while (self.index < self.scanned.lines.items.len) {
        const line = self.scanned.lines.items[self.index];
        if (line.indent != indent or line.kind != .mapping_entry) break;
        try self.emitSingleMappingEntry(line, indent);
    }

    try self.pushSimple(.mapping_end, .{});
}

fn emitSingleMappingEntry(self: *Parser, line: Scanner.ScannedLine, parent_indent: usize) anyerror!void {
    if (line.explicit_key) {
        try self.emitExplicitMappingEntry(line, parent_indent);
        return;
    }
    // Emit key
    const key_without_tag = stripTagPrefix(std.mem.trim(u8, line.key, " "));
    const key_anchor_info = extractLeadingAnchor(key_without_tag);
    const key_anchor: ?[]const u8 = if (key_anchor_info.name.len > 0) key_anchor_info.name else null;
    const key_without_anchor = stripTagPrefix(std.mem.trim(u8, if (key_anchor != null) key_anchor_info.rest else stripAnchorPrefix(key_without_tag), " "));
    if (line.key_style == .plain) {
        const raw_key = std.mem.trim(u8, line.key, " ");
        if (mappingKeyIsNode(raw_key)) {
            try self.parseScalarLikeValue(raw_key, .plain, line.line_no, line.indent);
        } else if (key_anchor != null and isAliasToken(key_without_anchor) != null) {
            return Error.Parse.UnexpectedToken;
        } else if (isAliasToken(key_without_anchor)) |alias_name| {
            try self.emitEvent( .{
                .kind = .alias,
                .data = .{ .alias = .{ .name = try self.allocator.dupe(u8, alias_name), .span = line.span } },
            });
        } else {
            const effective_key_style = detectInlineStyle(key_without_anchor);
            try self.pushScalarNormalized(key_without_anchor, effective_key_style, key_anchor, line.span);
        }
    } else {
        try self.pushScalarNormalized(key_without_anchor, line.key_style, key_anchor, line.span);
    }

    // Emit value
    const value_is_virtual_empty = isVirtualEmptyValue(line.value, line.style);
    if (value_is_virtual_empty) {
        const val_stripped = if (line.style == .plain)
            stripTagPrefix(std.mem.trim(u8, line.value, " "))
        else
            line.value;
        const val_anc_info = extractLeadingAnchor(val_stripped);
        const maybe_anchor: ?[]const u8 = if (val_anc_info.name.len > 0) val_anc_info.name else null;
        self.index += 1;
        if (self.index < self.scanned.lines.items.len) {
            const next = self.scanned.lines.items[self.index];
            if (next.indent > parent_indent) {
                if (maybe_anchor) |anch| {
                    if (next.kind == .sequence_item) {
                        try self.emitEvent( .{
                            .kind = .sequence_start,
                            .data = .{ .sequence_start = .{
                                .style = .block,
                                .anchor = try self.allocator.dupe(u8, anch),
                            } },
                        });
                        while (self.index < self.scanned.lines.items.len) {
                            const sl = self.scanned.lines.items[self.index];
                            if (sl.indent != next.indent or sl.kind != .sequence_item) break;
                            self.index += 1;
                            if (sl.value.len == 0) {
                                if (self.index < self.scanned.lines.items.len and self.scanned.lines.items[self.index].indent > next.indent) {
                                    try self.parseBlockValue(self.scanned.lines.items[self.index].indent, false);
                                } else {
                                    try self.pushScalar("", .plain, null, sl.span);
                                }
                            } else {
                                try self.parseScalarLikeValue(sl.value, sl.style, sl.line_no, sl.indent + 2);
                            }
                        }
                        try self.pushSimple(.sequence_end, .{});
                        return;
                    } else if (next.kind == .mapping_entry) {
                        try self.emitEvent( .{
                            .kind = .mapping_start,
                            .data = .{ .mapping_start = .{
                                .style = .block,
                                .anchor = try self.allocator.dupe(u8, anch),
                            } },
                        });
                        while (self.index < self.scanned.lines.items.len) {
                            const ml = self.scanned.lines.items[self.index];
                            if (ml.indent != next.indent or ml.kind != .mapping_entry) break;
                            try self.emitSingleMappingEntry(ml, next.indent);
                        }
                        try self.pushSimple(.mapping_end, .{});
                        return;
                    } else {
                        if (next.kind == .scalar) {
                            const nv = stripTagPrefix(std.mem.trim(u8, next.value, " "));
                            if (extractLeadingAnchor(nv).name.len > 0) return Error.Parse.UnexpectedToken;
                        }
                        try self.parseBlockValue(next.indent, false);
                    }
                } else {
                    // Check if next is tag/anchor-only scalar followed by a structure or block scalar
                    if (next.kind == .scalar and next.style == .plain) {
                        const nv = stripTagPrefix(std.mem.trim(u8, next.value, " "));
                        const nv_anc = extractLeadingAnchor(nv);
                        const nv_rest = std.mem.trim(u8, stripAnchorPrefix(nv), " ");
                        const nv_anchor: ?[]const u8 = if (nv_anc.name.len > 0) nv_anc.name else null;
                        if (nv_rest.len == 0 and (nv_anchor != null or !std.mem.eql(u8, std.mem.trim(u8, next.value, " "), nv))) {
                            const saved_idx = self.index;
                            self.index += 1;
                            if (self.index < self.scanned.lines.items.len) {
                                const nn = self.scanned.lines.items[self.index];
                                if (nn.kind == .scalar and (nn.style == .literal or nn.style == .folded)) {
                                    const block = try self.collectBlockScalar(parent_indent, nn.style, nn.value, nn.line_no);
                                    try self.pushBlockScalar(block, nn.style, nv_anchor, line.span);
                                    return;
                                }
                                if (nv_anchor) |anch| {
                                    if (nn.kind == .sequence_item and nn.indent >= parent_indent) {
                                        try self.emitEvent( .{
                                            .kind = .sequence_start,
                                            .data = .{ .sequence_start = .{
                                                .style = .block,
                                                .anchor = try self.allocator.dupe(u8, anch),
                                            } },
                                        });
                                        while (self.index < self.scanned.lines.items.len) {
                                            const sl = self.scanned.lines.items[self.index];
                                            if (sl.indent != nn.indent or sl.kind != .sequence_item) break;
                                            self.index += 1;
                                            if (sl.value.len == 0) {
                                                if (self.index < self.scanned.lines.items.len and self.scanned.lines.items[self.index].indent > nn.indent) {
                                                    try self.parseBlockValue(self.scanned.lines.items[self.index].indent, false);
                                                } else {
                                                    try self.pushScalar("", .plain, null, sl.span);
                                                }
                                            } else {
                                                try self.parseScalarLikeValue(sl.value, sl.style, sl.line_no, sl.indent + 2);
                                            }
                                        }
                                        try self.pushSimple(.sequence_end, .{});
                                        return;
                                    } else if (nn.kind == .mapping_entry and nn.indent >= parent_indent) {
                                        try self.emitEvent( .{
                                            .kind = .mapping_start,
                                            .data = .{ .mapping_start = .{
                                                .style = .block,
                                                .anchor = try self.allocator.dupe(u8, anch),
                                            } },
                                        });
                                        while (self.index < self.scanned.lines.items.len) {
                                            const ml = self.scanned.lines.items[self.index];
                                            if (ml.indent != nn.indent or ml.kind != .mapping_entry) break;
                                            try self.emitSingleMappingEntry(ml, nn.indent);
                                        }
                                        try self.pushSimple(.mapping_end, .{});
                                        return;
                                    }
                                }
                            }
                            self.index = saved_idx;
                        }
                    }
                    // 2SXE: "foo:\n  *a:" - single mapping entry with alias key and empty value
                    // is the alias as value, not a nested mapping
                    if (next.kind == .mapping_entry and next.indent > parent_indent) {
                        if (isAliasToken(next.key)) |alias_name| {
                            if (isVirtualEmptyValue(next.value, next.style)) {
                                const following = self.index + 1;
                                const sole_entry = following >= self.scanned.lines.items.len or
                                    self.scanned.lines.items[following].indent != next.indent or
                                    self.scanned.lines.items[following].kind != .mapping_entry;
                                if (sole_entry) {
                                    self.index += 1;
                                    try self.emitEvent( .{
                                        .kind = .alias,
                                        .data = .{ .alias = .{ .name = try self.allocator.dupe(u8, alias_name), .span = next.span } },
                                    });
                                    return;
                                }
                            }
                        }
                    }
                    try self.parseBlockValue(next.indent, false);
                }
            } else if (next.indent == parent_indent and next.kind == .sequence_item) {
                if (maybe_anchor) |anch| {
                    try self.emitEvent( .{
                        .kind = .sequence_start,
                        .data = .{ .sequence_start = .{
                            .style = .block,
                            .anchor = try self.allocator.dupe(u8, anch),
                        } },
                    });
                    while (self.index < self.scanned.lines.items.len) {
                        const sl = self.scanned.lines.items[self.index];
                        if (sl.indent != parent_indent or sl.kind != .sequence_item) break;
                        self.index += 1;
                        if (sl.value.len == 0) {
                            if (self.index < self.scanned.lines.items.len and self.scanned.lines.items[self.index].indent > parent_indent) {
                                try self.parseBlockValue(self.scanned.lines.items[self.index].indent, false);
                            } else {
                                try self.pushScalar("", .plain, null, sl.span);
                            }
                        } else {
                            try self.parseScalarLikeValue(sl.value, sl.style, sl.line_no, sl.indent + 2);
                        }
                    }
                    try self.pushSimple(.sequence_end, .{});
                } else {
                    try self.parseBlockSequence(parent_indent);
                }
            } else {
                try self.prepareScalarTag(canonicalTag(line.value));
                try self.pushScalar("", .plain, maybe_anchor, line.span);
            }
        } else {
            try self.prepareScalarTag(canonicalTag(line.value));
            try self.pushScalar("", .plain, maybe_anchor, line.span);
        }
        return;
    }

    if (line.style == .literal or line.style == .folded) {
        const block = try self.collectBlockScalar(line.indent, line.style, line.value, line.line_no);
        try self.pushBlockScalar(block, line.style, null, line.span);
        return;
    }

    // Check for tag + block scalar indicator (e.g., "!foo >1" or "!!str |")
    if (line.style == .plain and line.value.len > 0) {
        const after_tag = stripTagPrefix(std.mem.trim(u8, line.value, " "));
        const after_anchor = stripAnchorPrefix(after_tag);
        const trimmed_after = std.mem.trimStart(u8, after_anchor, " ");
        if (trimmed_after.len > 0 and (trimmed_after[0] == '|' or trimmed_after[0] == '>')) {
            const bs_style: Token.ScalarStyle = if (trimmed_after[0] == '|') .literal else .folded;
            const block = try self.collectBlockScalar(line.indent, bs_style, trimmed_after, line.line_no);
            try self.pushBlockScalar(block, bs_style, null, line.span);
            return;
        }
    }

    // Check for tag prefix + unclosed quoted scalar (e.g., !!binary "\...)
    if (line.style == .plain and line.value.len > 0) {
        const after_tag2 = stripTagPrefix(std.mem.trim(u8, line.value, " "));
        const after_anchor2 = stripAnchorPrefix(after_tag2);
        const trimmed_after2 = std.mem.trimStart(u8, after_anchor2, " ");
        if (trimmed_after2.len > 0 and (trimmed_after2[0] == '"' or trimmed_after2[0] == '\'')) {
            const qs: Token.ScalarStyle = if (trimmed_after2[0] == '"') .double_quoted else .single_quoted;
            const quote2: u8 = trimmed_after2[0];
            if (!hasClosingQuote(trimmed_after2, quote2)) {
                const multiline = try self.collectMultilineQuotedValue(line.indent, trimmed_after2, qs);
                defer self.allocator.free(multiline);
                try self.parseScalarLikeValue(multiline, qs, line.line_no, line.indent);
                return;
            }
        }
    }

    if (line.style == .single_quoted or line.style == .double_quoted) {
        const quote: u8 = if (line.style == .double_quoted) '"' else '\'';
        if (!hasClosingQuote(line.value, quote)) {
            const multiline = try self.collectMultilineQuotedValue(line.indent, line.value, line.style);
            defer self.allocator.free(multiline);
            try self.parseScalarLikeValue(multiline, line.style, line.line_no, line.indent);
            return;
        }
    }

    if (line.style == .plain and line.value.len > 0) {
        const joined = try self.collectPlainContinuation(line.indent + 1, line.value);
        defer self.allocator.free(joined);
        try self.parseScalarLikeValue(joined, .plain, line.line_no, line.indent + line.key.len + 1);
        return;
    }

    try self.parseScalarLikeValue(line.value, line.style, line.line_no, line.indent + line.key.len + 1);
    self.index += 1;
}

fn emitExplicitMappingEntry(self: *Parser, line: Scanner.ScannedLine, parent_indent: usize) anyerror!void {
    const key_raw = std.mem.trim(u8, if (std.mem.eql(u8, line.key, "~")) "" else line.key, " \t");
    if (isCompactSequence(key_raw)) {
        self.index += 1;
        try self.emitCompactSequence(parent_indent, key_raw, line.span, line.line_no);
    } else if (key_raw.len > 0 and (key_raw[0] == '[' or key_raw[0] == '{' or key_raw[0] == '&' or key_raw[0] == '!')) {
        self.index += 1;
        try self.parseScalarLikeValue(key_raw, .plain, line.line_no, line.indent);
    } else if (key_raw.len == 0) {
        self.index += 1;
        if (self.index < self.scanned.lines.items.len) {
            const next = self.scanned.lines.items[self.index];
            if (next.kind == .sequence_item and next.indent >= parent_indent) {
                try self.parseBlockSequence(next.indent);
            } else if (next.kind == .mapping_entry and !next.explicit_key and next.indent >= parent_indent) {
                try self.parseBlockMapping(next.indent);
            } else {
                try self.pushScalar("", .plain, null, line.span);
            }
        } else {
            try self.pushScalar("", .plain, null, line.span);
        }
    } else {
        const key_text = try self.collectExplicitKey(line, parent_indent);
        const key_style = line.key_style;
        if (key_style == .literal or key_style == .folded) {
            try self.pushBlockScalar(key_text, key_style, null, line.span);
        } else {
            defer self.allocator.free(key_text);
            if (key_style == .plain) {
                const trimmed = std.mem.trim(u8, key_text, " \t");
                if (trimmed.len == 0 or isSubslice(self.scanned.source, trimmed)) {
                    try self.pushScalar(trimmed, key_style, null, line.span);
                } else {
                    const owned_key = try self.scalar_allocator.dupe(u8, trimmed);
                    try self.pushScalarOwned(owned_key, key_style, null, line.span);
                }
            } else {
                try self.pushScalarNormalized(key_text, key_style, null, line.span);
            }
        }
    }

    if (self.index >= self.scanned.lines.items.len) {
        try self.pushScalar("", .plain, null, line.span);
        return;
    }
    const next = self.scanned.lines.items[self.index];
    if (next.kind != .explicit_value or next.indent != parent_indent) {
        try self.pushScalar("", .plain, null, line.span);
        return;
    }

    const value_text = next.value;
    const value_style = next.style;
    const value_span = next.span;
    const value_line_no = next.line_no;
    self.index += 1;

    if (value_style == .literal or value_style == .folded) {
        self.index -= 1;
        const block = try self.collectBlockScalar(parent_indent, value_style, value_text, value_line_no);
        try self.pushBlockScalar(block, value_style, null, value_span);
        return;
    }

    const trimmed = std.mem.trimStart(u8, value_text, " \t");
    if (trimmed.len >= 1 and trimmed[0] == '-' and (trimmed.len == 1 or trimmed[1] == ' ' or trimmed[1] == '\t')) {
        try self.emitCompactSequence(parent_indent, trimmed, value_span, value_line_no);
        return;
    }
    if (trimmed.len > 0 and (trimmed[0] == '[' or trimmed[0] == '{')) {
        try self.parseFlow(trimmed, value_line_no, parent_indent);
        try self.commitFlowScratch();
        return;
    }
    if (trimmed.len == 0) {
        if (self.index < self.scanned.lines.items.len and self.scanned.lines.items[self.index].kind != .document_end) {
            const nested = self.scanned.lines.items[self.index];
            if (nested.kind == .sequence_item and nested.indent >= parent_indent) {
                try self.parseBlockSequence(nested.indent);
                return;
            }
            if (nested.indent > parent_indent) {
                try self.parseBlockValue(nested.indent, false);
                return;
            }
        }
        try self.pushScalar("", .plain, null, value_span);
        return;
    }

    const joined = try self.collectExplicitPlainValue(parent_indent, trimmed);
    defer self.allocator.free(joined);
    try self.parseScalarLikeValue(joined, detectInlineStyle(trimmed), value_line_no, parent_indent);
}

fn collectExplicitKey(self: *Parser, line: Scanner.ScannedLine, parent_indent: usize) ![]u8 {
    if (line.key_style == .literal or line.key_style == .folded) {
        return self.collectBlockScalar(parent_indent, line.key_style, line.key, line.line_no);
    }

    var out: std.ArrayListUnmanaged(u8) = .empty;
    errdefer out.deinit(self.allocator);
    const stripped = stripTagPrefix(std.mem.trim(u8, line.key, " \t"));
    try out.appendSlice(self.allocator, std.mem.trim(u8, stripped, " \t"));
    self.index += 1;
    while (self.index < self.scanned.lines.items.len) {
        const next = self.scanned.lines.items[self.index];
        if (next.indent <= parent_indent or next.kind != .scalar) break;
        if (out.items.len > 0) try out.append(self.allocator, ' ');
        try out.appendSlice(self.allocator, std.mem.trim(u8, stripTagPrefix(next.value), " \t"));
        self.index += 1;
    }
    return out.toOwnedSlice(self.allocator);
}

fn collectExplicitPlainValue(self: *Parser, parent_indent: usize, initial: []const u8) ![]u8 {
    var out: std.ArrayListUnmanaged(u8) = .empty;
    errdefer out.deinit(self.allocator);
    try out.appendSlice(self.allocator, std.mem.trim(u8, initial, " \t"));
    while (self.index < self.scanned.lines.items.len) {
        const next = self.scanned.lines.items[self.index];
        if (next.indent <= parent_indent or next.kind != .scalar) break;
        if (out.items.len > 0) try out.append(self.allocator, ' ');
        try out.appendSlice(self.allocator, std.mem.trim(u8, next.value, " \t"));
        self.index += 1;
    }
    return out.toOwnedSlice(self.allocator);
}

fn emitCompactSequence(self: *Parser, parent_indent: usize, first: []const u8, span: @import("Span.zig"), line_no: usize) anyerror!void {
    var nested_indent = parent_indent + 2;
    if (self.index < self.scanned.lines.items.len) {
        const nxt = self.scanned.lines.items[self.index];
        if (nxt.kind == .sequence_item and nxt.indent > parent_indent) nested_indent = nxt.indent;
    }
    try self.emitEvent( .{
        .kind = .sequence_start,
        .data = .{ .sequence_start = .{ .style = .block } },
    });

    const inner = if (first.len <= 1) "" else std.mem.trimStart(u8, first[2..], " \t");
    if (inner.len == 0) {
        try self.pushScalar("", .plain, null, span);
    } else if (isCompactSequence(inner)) {
        try self.emitCompactSequence(nested_indent, inner, span, line_no);
    } else {
        try self.parseScalarLikeValue(inner, detectInlineStyle(inner), line_no, nested_indent);
    }

    while (self.index < self.scanned.lines.items.len) {
        const nl = self.scanned.lines.items[self.index];
        if (nl.indent != nested_indent or nl.kind != .sequence_item) break;
        const trimmed = std.mem.trimStart(u8, nl.value, " \t");
        self.index += 1;
        if (isCompactSequence(trimmed)) {
            try self.emitCompactSequence(nl.indent, trimmed, nl.span, nl.line_no);
        } else if (trimmed.len == 0) {
            try self.pushScalar("", .plain, null, nl.span);
        } else {
            try self.parseScalarLikeValue(trimmed, nl.style, nl.line_no, nested_indent);
        }
    }
    try self.pushSimple(.sequence_end, .{});
}

fn isCompactSequence(text: []const u8) bool {
    return text.len >= 1 and text[0] == '-' and (text.len == 1 or text[1] == ' ' or text[1] == '\t');
}

fn mutablePinBytes(self: *Parser) ?[]u8 {
    if (!self.options.retain_input) return null;
    const st = self.stream orelse return null;
    return st.mutablePinnedBytes();
}

fn sourceOffset(source: []const u8, slice: []const u8) usize {
    return @intFromPtr(slice.ptr) - @intFromPtr(source.ptr);
}

fn appendLiteralBlockLine(
    pin: ?[]u8,
    pin_write: *usize,
    pool: ?*@import("Document.zig").OwnedScalarPool,
    rline: []const u8,
    li: usize,
    bi: usize,
    has_content: bool,
) !void {
    if (li >= rline.len) {
        if (has_content and li > bi) {
            const slice = rline[bi..];
            if (pin) |p| {
                @memcpy(p[pin_write.* .. pin_write.* + slice.len], slice);
                pin_write.* += slice.len;
                p[pin_write.*] = '\n';
                pin_write.* += 1;
            } else if (pool) |pl| {
                try pl.scratch.appendSlice(pl.parent, slice);
                try pl.scratch.append(pl.parent, '\n');
            }
        } else if (pin) |p| {
            p[pin_write.*] = '\n';
            pin_write.* += 1;
        } else if (pool) |pl| {
            try pl.scratch.append(pl.parent, '\n');
        }
        return;
    }
    if (li >= bi) {
        const slice = rline[bi..];
        if (pin) |p| {
            @memcpy(p[pin_write.* .. pin_write.* + slice.len], slice);
            pin_write.* += slice.len;
            p[pin_write.*] = '\n';
            pin_write.* += 1;
        } else if (pool) |pl| {
            try pl.scratch.appendSlice(pl.parent, slice);
            try pl.scratch.append(pl.parent, '\n');
        }
    } else if (pin) |p| {
        p[pin_write.*] = '\n';
        pin_write.* += 1;
    } else if (pool) |pl| {
        try pl.scratch.append(pl.parent, '\n');
    }
}

fn finishLiteralBlockChomp(
    buf: []u8,
    start: usize,
    end: usize,
    has_content: bool,
    chomp: ChompMode,
) []const u8 {
    var len = end - start;
    switch (chomp) {
        .strip => {
            while (len > 0 and buf[start + len - 1] == '\n') len -= 1;
        },
        .clip => {
            while (len > 0 and buf[start + len - 1] == '\n') len -= 1;
            if (has_content) {
                buf[start + len] = '\n';
                len += 1;
            }
        },
        .keep => {},
    }
    return buf[start .. start + len];
}

fn collectBlockScalar(
    self: *Parser,
    parent_indent: usize,
    style: Token.ScalarStyle,
    header_value: []const u8,
    header_line_no: usize,
) anyerror![]u8 {
    const chomp = detectChompMode(header_value);
    var explicit_indent: ?usize = null;
    for (header_value) |c| {
        if (c >= '1' and c <= '9') {
            explicit_indent = @as(usize, c - '0');
            break;
        }
    }

    self.index += 1;

    const raw_lines = self.scanned.physical.items;

    var base_indent: ?usize = if (explicit_indent) |ei| parent_indent + ei else null;
    var block_end: usize = header_line_no + 1;
    var has_content = false;
    var leading_spaces: usize = 0;
    var in_leading = base_indent == null;
    var est: usize = 0;
    // Track sole content line for `|-` single-line fast path (CRD descriptions).
    var sole_content: ?[]const u8 = null;
    var sole_content_ri: usize = 0;
    var content_lines: usize = 0;
    var leading_blanks: usize = 0;

    const literal_pool: ?*@import("Document.zig").OwnedScalarPool = if (style == .literal and self.stream != null)
        self.stream.?.scalar_pool
    else
        null;
    var literal_pool_start: usize = 0;
    const pin_buf: ?[]u8 = if (style == .literal) self.mutablePinBytes() else null;
    var pin_write_start: usize = 0;
    var pin_write: usize = 0;

    {
        var ri = header_line_no + 1;
        while (ri < raw_lines.len) : (ri += 1) {
            const rline = raw_lines[ri].text;
            const li = raw_lines[ri].spaces;
            const is_blank = (li >= rline.len);

            // A document marker at column 0 ends the scalar. An indented "..." is content.
            if (!is_blank and li == 0 and rline.len >= 3 and (std.mem.startsWith(u8, rline, "---") or std.mem.startsWith(u8, rline, "..."))) {
                if (rline.len == 3 or rline[3] == ' ' or rline[3] == '\t') break;
            }

            // A tab in the parent's column is indentation, which YAML forbids.
            // A tab past that column is block-scalar content (` \t`, `  \t bar`).
            if (tabAtOrBefore(rline, parent_indent)) return Error.Parse.InvalidIndentation;

            if (is_blank) {
                if (in_leading and li > leading_spaces) leading_spaces = li;
                if (!has_content) leading_blanks += 1;
                block_end = ri + 1;
                continue;
            }

            if (base_indent == null) {
                if (li <= parent_indent and looksLikeStructure(rline[li..])) break;
                if (li <= parent_indent and parent_indent > 0) break;
                // Leading empty lines must not be indented past the first content line.
                if (leading_spaces > li) return Error.Parse.InvalidIndentation;
                base_indent = li;
                in_leading = false;
            }
            if (li < base_indent.?) break;

            has_content = true;
            block_end = ri + 1;
            content_lines += 1;
            const content = if (rline.len > base_indent.?) rline[base_indent.?..] else rline[rline.len..];
            if (content_lines == 1) {
                sole_content = content;
                sole_content_ri = ri;
            } else sole_content = null;
            if (rline.len > base_indent.?) est += rline.len - base_indent.? + 1 else est += 1;
        }
    }

    // Advance parser index past consumed scanned lines
    while (self.index < self.scanned.lines.items.len and
        self.scanned.lines.items[self.index].line_no < block_end)
    {
        self.index += 1;
    }

    const bi = base_indent orelse (parent_indent + 1);

    var literal_filled = false;
    if (style == .literal and leading_blanks == 0 and (pin_buf != null or literal_pool != null)) {
        if (literal_pool) |pl| literal_pool_start = pl.startScalar();
        var ri = header_line_no + 1;
        while (ri < block_end) : (ri += 1) {
            const rline = raw_lines[ri].text;
            const li = raw_lines[ri].spaces;
            if (pin_buf != null and pin_write == pin_write_start and li < rline.len and li >= bi) {
                const slice = rline[bi..];
                pin_write_start = sourceOffset(self.scanned.source, slice);
                pin_write = pin_write_start;
            }
            try appendLiteralBlockLine(
                pin_buf,
                &pin_write,
                literal_pool,
                rline,
                li,
                bi,
                has_content,
            );
        }
        literal_filled = true;
    }

    // `|-` with exactly one content line and nothing after it (no trailing blanks that
    // strip would still partially keep when more-indented). Leading blanks disallowed.
    if (style == .literal and chomp == .strip and content_lines == 1 and leading_blanks == 0 and
        block_end == sole_content_ri + 1)
    {
        if (sole_content) |content| {
            if (literal_pool) |pl| pl.truncateScalar(literal_pool_start);
            if (self.pinBorrowSlice(content)) |borrowed| return @constCast(borrowed);
            if (literal_pool) |pl| {
                const s = pl.startScalar();
                try pl.scratch.appendSlice(pl.parent, content);
                return @constCast(pl.scalarSlice(s));
            }
            return try self.scalar_allocator.dupe(u8, content);
        }
    }

    if (style == .literal) {
        if (literal_filled) {
            if (pin_buf) |pin| {
                return @constCast(finishLiteralBlockChomp(pin, pin_write_start, pin_write, has_content, chomp));
            }
            if (literal_pool) |pl| {
                const start = literal_pool_start;
                var len = pl.scratch.items.len - start;
                const buf = pl.scratch.items;
                switch (chomp) {
                    .strip => {
                        while (len > 0 and buf[start + len - 1] == '\n') len -= 1;
                    },
                    .clip => {
                        while (len > 0 and buf[start + len - 1] == '\n') len -= 1;
                        pl.truncateScalar(start + len);
                        if (has_content) try pl.scratch.append(pl.parent, '\n');
                        return @constCast(pl.scalarSlice(start));
                    },
                    .keep => {},
                }
                pl.truncateScalar(start + len);
                return @constCast(pl.scalarSlice(start));
            }
        } else if (literal_pool) |pl| {
            pl.truncateScalar(literal_pool_start);
        }
        return try collectLiteralBlock(
            self.scalar_allocator,
            raw_lines,
            header_line_no + 1,
            block_end,
            bi,
            has_content,
            chomp,
            est,
        );
    }

    var out: std.ArrayListUnmanaged(u8) = .empty;
    defer out.deinit(self.scalar_allocator);
    try out.ensureTotalCapacity(self.scalar_allocator, est + 8);

    // Folded: accumulate blank lines and emit correct newline counts
    var blank_count: usize = 0;
    var first_content = true;
    var prev_was_more = false;

    var ri = header_line_no + 1;
    while (ri < block_end) : (ri += 1) {
        const rline = raw_lines[ri].text;
        const li = raw_lines[ri].spaces;
        const is_blank = (li >= rline.len);

        if (is_blank) {
            blank_count += 1;
            continue;
        }

        if (li < bi) break;

        // A tab is not indentation, so a line whose content starts with a tab is
        // more-indented and is not folded into the previous line.
        const more_indented = li > bi or (li < rline.len and rline[li] == '\t');
        const content = rline[bi..];

        if (first_content) {
            var b: usize = 0;
            while (b < blank_count) : (b += 1) try out.append(self.scalar_allocator, '\n');
        } else {
            if (blank_count > 0) {
                const extra: usize = if (prev_was_more or more_indented) 1 else 0;
                var b: usize = 0;
                while (b < blank_count + extra) : (b += 1) try out.append(self.scalar_allocator, '\n');
            } else if (prev_was_more or more_indented) {
                try out.append(self.scalar_allocator, '\n');
            } else {
                try out.append(self.scalar_allocator, ' ');
            }
        }

        try out.appendSlice(self.scalar_allocator, content);
        blank_count = 0;
        prev_was_more = more_indented;
        first_content = false;
    }

    // Trailing blank lines for keep chomping
    if (!first_content) {
        try out.append(self.scalar_allocator, '\n');
    }
    var b: usize = 0;
    while (b < blank_count) : (b += 1) try out.append(self.scalar_allocator, '\n');

    // Apply chomping
    switch (chomp) {
        .strip => {
            while (out.items.len > 0 and out.items[out.items.len - 1] == '\n') {
                _ = out.pop();
            }
        },
        .clip => {
            while (out.items.len > 0 and out.items[out.items.len - 1] == '\n') {
                _ = out.pop();
            }
            if (has_content) try out.append(self.scalar_allocator, '\n');
        },
        .keep => {},
    }

    return out.toOwnedSlice(self.scalar_allocator);
}

/// Literal `|` / `|-` / `|+` body → owned buffer (no ArrayList churn).
/// Hot path for CRD `description: |-` blocks (~6k in prometheus-operator bundle).
fn collectLiteralBlock(
    allocator: std.mem.Allocator,
    raw_lines: []const Scanner.PhysicalLine,
    start: usize,
    block_end: usize,
    bi: usize,
    has_content: bool,
    chomp: ChompMode,
    est: usize,
) ![]u8 {
    var buf = try allocator.alloc(u8, est + 8);
    errdefer allocator.free(buf);
    var len: usize = 0;

    var ri = start;
    while (ri < block_end) : (ri += 1) {
        const rline = raw_lines[ri].text;
        const li = raw_lines[ri].spaces;

        if (li >= rline.len) {
            if (has_content and li > bi) {
                const slice = rline[bi..];
                @memcpy(buf[len .. len + slice.len], slice);
                len += slice.len;
                buf[len] = '\n';
                len += 1;
            } else {
                buf[len] = '\n';
                len += 1;
            }
        } else if (li >= bi) {
            const slice = rline[bi..];
            @memcpy(buf[len .. len + slice.len], slice);
            len += slice.len;
            buf[len] = '\n';
            len += 1;
        } else {
            buf[len] = '\n';
            len += 1;
        }
    }

    switch (chomp) {
        .strip => {
            while (len > 0 and buf[len - 1] == '\n') len -= 1;
        },
        .clip => {
            while (len > 0 and buf[len - 1] == '\n') len -= 1;
            if (has_content) {
                buf[len] = '\n';
                len += 1;
            }
        },
        .keep => {},
    }

    return buf[0..len];
}

fn parseScalarLikeValue(
    self: *Parser,
    value: []const u8,
    style: Token.ScalarStyle,
    line_no: usize,
    col: usize,
) anyerror!void {
    var working = std.mem.trim(u8, value, " ");
    var working_style = style;
    var anchor_name: ?[]const u8 = null;

    if (working_style == .plain) {
        working = stripTagPrefix(working);
        const extracted = extractLeadingAnchor(working);
        if (extracted.name.len > 0) {
            anchor_name = extracted.name;
            working = extracted.rest;
        }
        working = stripTagPrefix(working);
        working_style = detectInlineStyle(working);
    }

    if (std.mem.eql(u8, std.mem.trim(u8, working, " \t"), "!")) working = "";
    if (working.len == 0) {
        try self.prepareScalarTag(canonicalTag(value));
        try self.pushScalar("", .plain, anchor_name, .{});
        return;
    }

    if (working_style == .plain and working.len > 0 and working[0] == '*') {
        if (anchor_name != null) return Error.Parse.UnexpectedToken;
        var end: usize = 1;
        while (end < working.len and Scanner.isBlockAnchorChar(working[end])) : (end += 1) {}
        const alias_name = working[1..end];
        const rest = std.mem.trim(u8, working[end..], " \t");
        if (alias_name.len == 0 or rest.len != 0) return Error.Parse.InvalidAlias;
        try self.emitEvent( .{
            .kind = .alias,
            .data = .{ .alias = .{ .name = try self.allocator.dupe(u8, alias_name), .span = .{} } },
        });
        return;
    }

    if (working[0] == '[' or working[0] == '{') {
        const pre_flow_count = if (self.stream != null)
            self.flow_scratch.items.len
        else
            self.events.items.len;
        try self.parseFlow(working, line_no, col);
        if (anchor_name) |aname| {
            const list = if (self.stream != null) &self.flow_scratch else &self.events;
            if (list.items.len > pre_flow_count) {
                const first = &list.items[pre_flow_count];
                switch (first.kind) {
                    .sequence_start => first.data.sequence_start.anchor = try self.allocator.dupe(u8, aname),
                    .mapping_start => first.data.mapping_start.anchor = try self.allocator.dupe(u8, aname),
                    else => {},
                }
            }
        }
        try self.commitFlowScratch();
        return;
    }

    if (working_style == .plain) {
        if (Scanner.findInlineMappingColon(working)) |idx| {
            const key = std.mem.trimEnd(u8, working[0..idx], " ");
            const raw_val = std.mem.trimStart(u8, working[idx + 1 ..], " ");
            if (key.len > 0) {
                try self.emitEvent( .{
                    .kind = .mapping_start,
                    .data = .{ .mapping_start = .{ .style = .block } },
                });
                try self.pushScalar(key, .plain, null, .{});
                try self.parseScalarLikeValue(raw_val, detectInlineStyle(raw_val), line_no, col + idx + 1);
                try self.pushSimple(.mapping_end, .{});
                return;
            }
        }
    }

    const final_style: Token.ScalarStyle = working_style;
    try self.prepareScalarTag(canonicalTag(value));
    try self.pushScalarNormalized(working, final_style, anchor_name, .{});
}

fn detectInlineStyle(value: []const u8) Token.ScalarStyle {
    if (value.len == 0) return .plain;
    if (value[0] == '\'') return .single_quoted;
    if (value[0] == '"') return .double_quoted;
    if (value[0] == '|') return .literal;
    if (value[0] == '>') return .folded;
    return .plain;
}

fn parseFlow(self: *Parser, text: []const u8, line_no: usize, col: usize) anyerror!void {
    const saved_sink = self.emit_sink;
    if (self.stream != null) self.emit_sink = .flow_scratch;
    defer {
        if (self.stream != null) self.emit_sink = saved_sink;
    }

    var folded: std.ArrayListUnmanaged([]u8) = .empty;
    defer {
        for (folded.items) |buf| self.allocator.free(buf);
        folded.deinit(self.allocator);
    }
    const tokens = try Scanner.tokenizeFlow(self.allocator, text, line_no, col, &folded);
    defer self.allocator.free(tokens);
    var cursor: usize = 0;
    const at = self.activeEventList().items.len;
    try self.parseFlowValue(tokens, &cursor);
    // `[]: x` in block context is a mapping whose key is the flow node.
    if (cursor < tokens.len and tokens[cursor].kind == .colon) {
        if (tokens[cursor].indent != 0) return Error.Parse.UnexpectedToken;
        try self.eventInsert(at, .{
            .kind = .mapping_start,
            .data = .{ .mapping_start = .{ .style = .block } },
        });
        cursor += 1;
        try self.parseFlowPairValue(tokens, &cursor, tokens[cursor - 1].span);
        try self.pushSimple(.mapping_end, .{});
    }
    if (cursor < tokens.len and tokens[cursor].kind != .eof) return Error.Parse.UnexpectedToken;
}

fn parseFlowPairValue(self: *Parser, tokens: []const Token.Token, cursor: *usize, span: @import("Span.zig")) anyerror!void {
    if (cursor.* >= tokens.len or tokens[cursor.*].kind == .comma or tokens[cursor.*].kind == .rbrace or tokens[cursor.*].kind == .rbracket or tokens[cursor.*].kind == .eof) {
        try self.pushScalar("", .plain, null, span);
    } else {
        try self.parseFlowValue(tokens, cursor);
    }
}

fn noteFlowAnchor(self: *Parser, anchor: []const u8) !void {
    if (anchor.len == 0) return;
    const list = self.activeEventList();
    if (list.items.len == 0) return;
    const ev = &list.items[list.items.len - 1];
    const name = try self.allocator.dupe(u8, anchor);
    switch (ev.kind) {
        .sequence_start => ev.data.sequence_start.anchor = name,
        .mapping_start => ev.data.mapping_start.anchor = name,
        else => self.allocator.free(name),
    }
}

fn parseFlowValue(self: *Parser, tokens: []const Token.Token, cursor: *usize) anyerror!void {
    if (cursor.* >= tokens.len) return Error.Parse.UnexpectedToken;
    const tok = tokens[cursor.*];
    switch (tok.kind) {
        .lbracket => {
            cursor.* += 1;
            try self.emitEvent( .{
                .kind = .sequence_start,
                .data = .{ .sequence_start = .{ .style = .flow } },
            });
            try self.noteFlowAnchor(tok.anchor);
            while (cursor.* < tokens.len and tokens[cursor.*].kind != .rbracket and tokens[cursor.*].kind != .eof) {
                // `key: value` inside a flow sequence is a single-entry mapping.
                const entry_at = self.activeEventList().items.len;
                if (tokens[cursor.*].kind == .colon) {
                    if (tokens[cursor.*].indent != 0) return Error.Parse.UnexpectedToken;
                    try self.pushScalar("", .plain, null, tokens[cursor.*].span);
                } else {
                    try self.parseFlowValue(tokens, cursor);
                }
                if (cursor.* < tokens.len and tokens[cursor.*].kind == .colon) {
                    if (tokens[cursor.*].indent != 0) return Error.Parse.UnexpectedToken;
                    try self.eventInsert(entry_at, .{
                        .kind = .mapping_start,
                        .data = .{ .mapping_start = .{ .style = .flow } },
                    });
                    cursor.* += 1;
                    if (cursor.* >= tokens.len or tokens[cursor.*].kind == .comma or tokens[cursor.*].kind == .rbracket or tokens[cursor.*].kind == .eof) {
                        try self.pushScalar("", .plain, null, tok.span);
                    } else {
                        try self.parseFlowValue(tokens, cursor);
                    }
                    try self.pushSimple(.mapping_end, tok.span);
                }
                if (cursor.* < tokens.len and tokens[cursor.*].kind == .comma) cursor.* += 1;
            }
            if (cursor.* >= tokens.len or tokens[cursor.*].kind != .rbracket) return Error.Parse.UnterminatedFlowCollection;
            cursor.* += 1;
            try self.pushSimple(.sequence_end, tok.span);
        },
        .lbrace => {
            cursor.* += 1;
            try self.emitEvent( .{
                .kind = .mapping_start,
                .data = .{ .mapping_start = .{ .style = .flow } },
            });
            try self.noteFlowAnchor(tok.anchor);
            while (cursor.* < tokens.len and tokens[cursor.*].kind != .rbrace and tokens[cursor.*].kind != .eof) {
                const key = tokens[cursor.*];
                if (key.kind == .colon) {
                    try self.pushScalar("", .plain, null, key.span);
                    cursor.* += 1;
                    try self.parseFlowPairValue(tokens, cursor, key.span);
                } else if (key.kind == .alias) {
                    try self.emitEvent( .{
                        .kind = .alias,
                        .data = .{ .alias = .{ .name = try self.allocator.dupe(u8, key.lexeme), .span = key.span } },
                    });
                    cursor.* += 1;
                    if (cursor.* < tokens.len and tokens[cursor.*].kind == .colon) {
                        cursor.* += 1;
                        try self.parseFlowPairValue(tokens, cursor, key.span);
                    } else {
                        try self.pushScalar("", .plain, null, key.span);
                    }
                } else if (key.kind == .lbracket or key.kind == .lbrace) {
                    try self.parseFlowValue(tokens, cursor);
                    if (cursor.* < tokens.len and tokens[cursor.*].kind == .colon) {
                        cursor.* += 1;
                        try self.parseFlowPairValue(tokens, cursor, key.span);
                    } else {
                        try self.pushScalar("", .plain, null, key.span);
                    }
                } else if (key.kind == .scalar) {
                    const flow_anchor: ?[]const u8 = if (key.anchor.len > 0) key.anchor else null;
                    try self.pushScalarNormalized(key.lexeme, key.scalar_style, flow_anchor, key.span);
                    cursor.* += 1;
                    if (cursor.* >= tokens.len) return Error.Parse.UnexpectedToken;
                    if (tokens[cursor.*].kind == .colon) {
                        cursor.* += 1;
                        try self.parseFlowPairValue(tokens, cursor, key.span);
                    } else {
                        try self.pushScalar("", .plain, null, key.span);
                    }
                } else return Error.Parse.UnexpectedToken;
                // Flow mapping entries are separated by commas. A newline is not enough.
                if (cursor.* < tokens.len and tokens[cursor.*].kind == .comma) {
                    cursor.* += 1;
                } else if (cursor.* < tokens.len and tokens[cursor.*].kind != .rbrace and tokens[cursor.*].kind != .eof) {
                    return Error.Parse.UnexpectedToken;
                }
            }
            if (cursor.* >= tokens.len or tokens[cursor.*].kind != .rbrace) return Error.Parse.UnterminatedFlowCollection;
            cursor.* += 1;
            try self.pushSimple(.mapping_end, tok.span);
        },
        .alias => {
            try self.emitEvent( .{
                .kind = .alias,
                .data = .{ .alias = .{ .name = try self.allocator.dupe(u8, tok.lexeme), .span = tok.span } },
            });
            cursor.* += 1;
        },
        .scalar => {
            const flow_anchor: ?[]const u8 = if (tok.anchor.len > 0) tok.anchor else null;
            try self.pushScalarNormalized(tok.lexeme, tok.scalar_style, flow_anchor, tok.span);
            cursor.* += 1;
        },
        .eof => {},
        else => return Error.Parse.UnexpectedToken,
    }
}

fn normalizeScalar(allocator: std.mem.Allocator, raw: []const u8, style: Token.ScalarStyle) anyerror![]u8 {
    switch (style) {
        .single_quoted => {
            const inner = stripOuterQuotes(raw, '\'');
            if (std.mem.indexOf(u8, inner, "''") == null) {
                return allocator.dupe(u8, inner);
            }
            var out: std.ArrayListUnmanaged(u8) = .empty;
            defer out.deinit(allocator);
            try out.ensureTotalCapacity(allocator, inner.len);
            var i: usize = 0;
            while (i < inner.len) : (i += 1) {
                if (inner[i] == '\'' and i + 1 < inner.len and inner[i + 1] == '\'') {
                    try out.append(allocator, '\'');
                    i += 1;
                } else {
                    try out.append(allocator, inner[i]);
                }
            }
            return out.toOwnedSlice(allocator);
        },
        .double_quoted => {
            const inner = stripOuterQuotes(raw, '"');
            if (std.mem.indexOfScalar(u8, inner, '\\') == null) {
                return allocator.dupe(u8, inner);
            }
            var out: std.ArrayListUnmanaged(u8) = .empty;
            defer out.deinit(allocator);
            try out.ensureTotalCapacity(allocator, inner.len);
            var i: usize = 0;
            while (i < inner.len) {
                if (inner[i] == '\\' and i + 1 < inner.len) {
                    i += 1;
                    switch (inner[i]) {
                        'n' => try out.append(allocator, '\n'),
                        'r' => try out.append(allocator, '\r'),
                        't', '\t' => try out.append(allocator, '\t'),
                        '"' => try out.append(allocator, '"'),
                        '\\' => try out.append(allocator, '\\'),
                        '/' => try out.append(allocator, '/'),
                        '0' => try out.append(allocator, 0),
                        'a' => try out.append(allocator, 0x07),
                        'b' => try out.append(allocator, 0x08),
                        'e' => try out.append(allocator, 0x1B),
                        'v' => try out.append(allocator, 0x0B),
                        ' ' => try out.append(allocator, ' '),
                        'x' => {
                            if (i + 2 >= inner.len) return Error.Parse.InvalidEscapeSequence;
                            const cp = std.fmt.parseInt(u21, inner[i + 1 .. i + 3], 16) catch return Error.Parse.InvalidEscapeSequence;
                            try appendUtf8(&out, allocator, cp);
                            i += 2;
                        },
                        'u' => {
                            if (i + 4 >= inner.len) return Error.Parse.InvalidEscapeSequence;
                            const cp = std.fmt.parseInt(u21, inner[i + 1 .. i + 5], 16) catch return Error.Parse.InvalidEscapeSequence;
                            try appendUtf8(&out, allocator, cp);
                            i += 4;
                        },
                        'U' => {
                            if (i + 8 >= inner.len) return Error.Parse.InvalidEscapeSequence;
                            const cp = std.fmt.parseInt(u21, inner[i + 1 .. i + 9], 16) catch return Error.Parse.InvalidEscapeSequence;
                            try appendUtf8(&out, allocator, cp);
                            i += 8;
                        },
                        'N' => try out.appendSlice(allocator, "\xC2\x85"),
                        '_' => try out.appendSlice(allocator, "\xC2\xA0"),
                        'L' => try out.appendSlice(allocator, "\xE2\x80\xA8"),
                        'P' => try out.appendSlice(allocator, "\xE2\x80\xA9"),
                        else => return Error.Parse.InvalidEscapeSequence,
                    }
                    i += 1;
                } else {
                    const start = i;
                    i += 1;
                    while (i < inner.len and inner[i] != '\\') : (i += 1) {}
                    try out.appendSlice(allocator, inner[start..i]);
                }
            }
            return out.toOwnedSlice(allocator);
        },
        else => return allocator.dupe(u8, std.mem.trim(u8, raw, " \t")),
    }
}

fn appendUtf8(out: *std.ArrayListUnmanaged(u8), allocator: std.mem.Allocator, codepoint: u21) !void {
    if (codepoint <= 0x7F) {
        try out.append(allocator, @intCast(codepoint));
    } else if (codepoint <= 0x7FF) {
        try out.append(allocator, @intCast(0xC0 | (codepoint >> 6)));
        try out.append(allocator, @intCast(0x80 | (codepoint & 0x3F)));
    } else if (codepoint <= 0xFFFF) {
        try out.append(allocator, @intCast(0xE0 | (codepoint >> 12)));
        try out.append(allocator, @intCast(0x80 | ((codepoint >> 6) & 0x3F)));
        try out.append(allocator, @intCast(0x80 | (codepoint & 0x3F)));
    } else {
        try out.append(allocator, @intCast(0xF0 | (codepoint >> 18)));
        try out.append(allocator, @intCast(0x80 | ((codepoint >> 12) & 0x3F)));
        try out.append(allocator, @intCast(0x80 | ((codepoint >> 6) & 0x3F)));
        try out.append(allocator, @intCast(0x80 | (codepoint & 0x3F)));
    }
}

fn stripOuterQuotes(raw: []const u8, quote: u8) []const u8 {
    if (raw.len >= 2 and raw[0] == quote and raw[raw.len - 1] == quote) {
        return raw[1 .. raw.len - 1];
    }
    return raw;
}

fn stripTagPrefix(raw: []const u8) []const u8 {
    var value = std.mem.trimStart(u8, raw, " ");
    while (value.len > 1 and value[0] == '!' and !std.mem.startsWith(u8, value, "!=")) {
        var i: usize = 1;
        if (i < value.len and value[i] == '<') {
            i += 1;
            while (i < value.len and value[i] != '>') : (i += 1) {}
            if (i < value.len and value[i] == '>') i += 1;
        } else {
            while (i < value.len and !isSpace(value[i])) : (i += 1) {}
        }
        value = std.mem.trimStart(u8, value[i..], " ");
    }
    return value;
}

fn stripAnchorPrefix(raw: []const u8) []const u8 {
    var value = std.mem.trimStart(u8, raw, " ");
    while (value.len > 1 and value[0] == '&') {
        var i: usize = 1;
        while (i < value.len) : (i += 1) {
            if (!Scanner.isBlockAnchorChar(value[i])) break;
        }
        if (i <= 1) break;
        value = std.mem.trimStart(u8, value[i..], " \t");
    }
    return value;
}

fn extractLeadingAnchor(raw: []const u8) struct { name: []const u8, rest: []const u8 } {
    var value = std.mem.trimStart(u8, raw, " ");
    if (value.len <= 1 or value[0] != '&') return .{ .name = "", .rest = raw };

    var i: usize = 1;
    while (i < value.len) : (i += 1) {
        if (!Scanner.isBlockAnchorChar(value[i])) break;
    }
    if (i <= 1) return .{ .name = "", .rest = raw };

    const name = value[1..i];
    value = std.mem.trimStart(u8, value[i..], " \t");
    return .{ .name = name, .rest = value };
}

fn isVirtualEmptyValue(value: []const u8, style: Token.ScalarStyle) bool {
    if (std.mem.trim(u8, value, " ").len == 0) return true;
    if (style != .plain) return false;
    const no_tag = stripTagPrefix(std.mem.trim(u8, value, " "));
    const no_anchor = stripAnchorPrefix(no_tag);
    return std.mem.trim(u8, no_anchor, " ").len == 0;
}

fn collectPlainContinuation(self: *Parser, min_indent: usize, initial: []const u8) ![]u8 {
    var out: std.ArrayListUnmanaged(u8) = .empty;
    defer out.deinit(self.allocator);

    try out.appendSlice(self.allocator, std.mem.trim(u8, initial, " "));

    const start = self.scanned.lines.items[self.index];
    var prev_line_no = start.line_no;
    self.index += 1;
    // A trailing comment ends the plain scalar (`word1 # comment` / `word2`).
    if (start.ends_with_comment) return out.toOwnedSlice(self.allocator);
    // A finished quoted or flow node does not continue onto the next line
    // (`!foo "bar"` then `%TAG`).
    if (isClosedNode(initial)) return out.toOwnedSlice(self.allocator);

    while (self.index < self.scanned.lines.items.len) {
        const line = self.scanned.lines.items[self.index];
        if (line.indent < min_indent or line.kind != .scalar) break;
        // A comment-only line between plain lines ends the scalar.
        if (line.after_comment) break;

        const gap = if (line.line_no > prev_line_no) line.line_no - prev_line_no - 1 else 0;
        if (gap > 0) {
            var g: usize = 0;
            while (g < gap) : (g += 1) try out.append(self.allocator, '\n');
        } else if (out.items.len > 0) {
            try out.append(self.allocator, ' ');
        }

        try out.appendSlice(self.allocator, std.mem.trim(u8, line.value, " "));
        prev_line_no = line.line_no;
        self.index += 1;
        if (line.ends_with_comment) break;
    }

    return out.toOwnedSlice(self.allocator);
}

fn collectMultilineQuotedScalar(self: *Parser, base_indent: usize, style: Token.ScalarStyle) ![]u8 {
    var out: std.ArrayListUnmanaged(u8) = .empty;
    defer out.deinit(self.allocator);

    const quote: u8 = if (style == .double_quoted) '"' else '\'';

    const first = self.scanned.lines.items[self.index];
    try out.appendSlice(self.allocator, first.value);
    var prev_line_no = first.line_no;
    self.index += 1;

    while (self.index < self.scanned.lines.items.len and !hasClosingQuote(out.items, quote)) {
        const line = self.scanned.lines.items[self.index];
        if (line.kind == .document_end or line.kind == .empty_document) break;
        if (line.indent < base_indent) break;

        const gap = if (line.line_no > prev_line_no) line.line_no - prev_line_no - 1 else 0;

        stripTrailingQuoteWs(&out, style);

        if (style == .double_quoted and hasTrailingEscapedNewline(out.items)) {
            _ = out.pop();
            const trimmed = std.mem.trimStart(u8, line.value, " \t");
            try out.appendSlice(self.allocator, trimmed);
        } else {
            const trimmed = std.mem.trimStart(u8, std.mem.trim(u8, line.value, " \t"), " \t");
            const doc_end = trimDocEndLine(trimmed, style);
            if (gap > 0) {
                var g: usize = 0;
                while (g < gap) : (g += 1) try out.append(self.allocator, '\n');
            } else {
                try out.append(self.allocator, ' ');
            }
            if (doc_end.prefix.len > 0) {
                try out.appendSlice(self.allocator, doc_end.prefix);
                try out.appendSlice(self.allocator, doc_end.rest);
            } else {
                try out.appendSlice(self.allocator, doc_end.rest);
            }
        }
        prev_line_no = line.line_no;
        self.index += 1;
        if (truncateAtClosingQuote(&out, quote)) break;
    }

    if (!hasClosingQuote(out.items, quote)) return Error.Parse.UnterminatedString;
    return out.toOwnedSlice(self.allocator);
}

fn collectMultilineQuotedValue(self: *Parser, base_indent: usize, initial: []const u8, style: Token.ScalarStyle) ![]u8 {
    var out: std.ArrayListUnmanaged(u8) = .empty;
    defer out.deinit(self.allocator);

    const quote: u8 = if (style == .double_quoted) '"' else '\'';
    try out.appendSlice(self.allocator, initial);
    var prev_line_no = self.scanned.lines.items[self.index].line_no;
    self.index += 1;

    while (self.index < self.scanned.lines.items.len and !hasClosingQuote(out.items, quote)) {
        const line = self.scanned.lines.items[self.index];
        if (line.kind == .document_end or line.kind == .empty_document) break;
        // A mapping or sequence value must continue past the parent's column
        // (`quoted: "a\nb\nc"`, `"bar\n\tbaz"`).
        if (line.indent <= base_indent) return Error.Parse.InvalidIndentation;

        const gap = if (line.line_no > prev_line_no) line.line_no - prev_line_no - 1 else 0;

        stripTrailingQuoteWs(&out, style);

        if (style == .double_quoted and hasTrailingEscapedNewline(out.items)) {
            _ = out.pop();
            const trimmed = std.mem.trimStart(u8, line.value, " \t");
            try out.appendSlice(self.allocator, trimmed);
        } else {
            const trimmed = std.mem.trimStart(u8, std.mem.trim(u8, line.value, " \t"), " \t");
            const doc_end = trimDocEndLine(trimmed, style);
            if (gap > 0) {
                var g: usize = 0;
                while (g < gap) : (g += 1) try out.append(self.allocator, '\n');
            } else {
                try out.append(self.allocator, ' ');
            }
            if (doc_end.prefix.len > 0) {
                try out.appendSlice(self.allocator, doc_end.prefix);
                try out.appendSlice(self.allocator, doc_end.rest);
            } else {
                try out.appendSlice(self.allocator, doc_end.rest);
            }
        }
        prev_line_no = line.line_no;
        self.index += 1;
        if (truncateAtClosingQuote(&out, quote)) break;
    }

    if (!hasClosingQuote(out.items, quote)) return Error.Parse.UnterminatedString;
    return out.toOwnedSlice(self.allocator);
}

/// A quoted scalar may close before a trailing comment (`quotes" # lala`).
fn truncateAtClosingQuote(out: *std.ArrayListUnmanaged(u8), quote: u8) bool {
    const end = closingQuoteIndex(out.items, quote) orelse return false;
    if (end + 1 < out.items.len) out.shrinkRetainingCapacity(end + 1);
    return true;
}

fn closingQuoteIndex(text: []const u8, quote: u8) ?usize {
    if (text.len == 0 or text[0] != quote) return null;
    var i: usize = 1;
    if (quote == '\'') {
        while (i < text.len) : (i += 1) {
            if (text[i] != '\'') continue;
            if (i + 1 < text.len and text[i + 1] == '\'') {
                i += 1;
                continue;
            }
            return i;
        }
        return null;
    }
    while (i < text.len) : (i += 1) {
        if (text[i] == '\\' and i + 1 < text.len) {
            i += 1;
            continue;
        }
        if (text[i] == '"') return i;
    }
    return null;
}

/// 9MQT/01: In double-quoted, "... x" -> "...x" (strip space after "..." at line start)
fn trimDocEndLine(content: []const u8, style: Token.ScalarStyle) struct { prefix: []const u8, rest: []const u8 } {
    if (style != .double_quoted or content.len <= 3) return .{ .prefix = "", .rest = content };
    if (!std.mem.eql(u8, content[0..3], "...")) return .{ .prefix = "", .rest = content };
    if (content.len == 3) return .{ .prefix = "", .rest = content };
    if (content[3] != ' ') return .{ .prefix = "", .rest = content };
    return .{ .prefix = "...", .rest = std.mem.trimStart(u8, content[3..], " ") };
}

fn stripTrailingQuoteWs(out: *std.ArrayListUnmanaged(u8), style: Token.ScalarStyle) void {
    while (out.items.len > 0 and (out.items[out.items.len - 1] == ' ' or out.items[out.items.len - 1] == '\t')) {
        if (style == .double_quoted and out.items.len >= 2) {
            var bs_count: usize = 0;
            var j = out.items.len - 2;
            while (true) {
                if (out.items[j] != '\\') break;
                bs_count += 1;
                if (j == 0) break;
                j -= 1;
            }
            if (bs_count % 2 == 1) break;
        }
        _ = out.pop();
    }
}

fn hasTrailingEscapedNewline(content: []const u8) bool {
    var count: usize = 0;
    var i = content.len;
    while (i > 0) {
        i -= 1;
        if (content[i] == '\\') {
            count += 1;
        } else break;
    }
    return count % 2 == 1;
}

fn hasClosingQuote(text: []const u8, quote: u8) bool {
    // Use the escape-aware scan (`''` in single-quoted, `\"` in double-quoted).
    // A naive "starts and ends with quote" check falsely closes on content like
    // `'… ''..''` before the real terminator.
    return closingQuoteIndex(text, quote) != null;
}

const ChompMode = enum { clip, strip, keep };

fn detectChompMode(header_value: []const u8) ChompMode {
    const trimmed = std.mem.trim(u8, header_value, " ");
    if (trimmed.len == 0) return .clip;
    for (trimmed) |ch| {
        if (ch == '+') return .keep;
        if (ch == '-') return .strip;
    }
    return .clip;
}

fn tabAtOrBefore(line: []const u8, column: usize) bool {
    var i: usize = 0;
    while (i < line.len and i <= column) : (i += 1) {
        if (line[i] == '\t') return true;
        if (line[i] != ' ') return false;
    }
    return false;
}

fn isClosedNode(text: []const u8) bool {
    var value = stripTagPrefix(std.mem.trim(u8, text, " "));
    value = stripAnchorPrefix(value);
    value = std.mem.trimStart(u8, value, " \t");
    if (value.len == 0) return false;
    if (value[0] == '"' or value[0] == '\'') return hasClosingQuote(value, value[0]);
    if (value[0] == '[' or value[0] == '{') return !Scanner.flowUnclosed(value);
    return false;
}

fn countSpaces(line: []const u8) usize {
    return @import("CharClass.zig").countLeadingSpaces(line);
}

fn stripCR(line: []const u8) []const u8 {
    if (line.len > 0 and line[line.len - 1] == '\r') return line[0 .. line.len - 1];
    return line;
}

fn mappingKeyIsNode(raw: []const u8) bool {
    if (raw.len == 0) return false;
    if (raw[0] == '[' or raw[0] == '{' or raw[0] == '!') return true;
    if (raw[0] == '&') {
        const rest = extractLeadingAnchor(raw).rest;
        return rest.len > 0 and (rest[0] == '[' or rest[0] == '{');
    }
    return false;
}

fn canonicalTag(raw: []const u8) ?[]const u8 {
    const text = std.mem.trimStart(u8, raw, " \t");
    if (std.mem.startsWith(u8, text, "!!str")) return "tag:yaml.org,2002:str";
    if (std.mem.startsWith(u8, text, "!!null")) return "tag:yaml.org,2002:null";
    if (std.mem.startsWith(u8, text, "!!map")) return "tag:yaml.org,2002:map";
    if (std.mem.startsWith(u8, text, "!!seq")) return "tag:yaml.org,2002:seq";
    if (std.mem.eql(u8, text, "!") or std.mem.startsWith(u8, text, "! ")) return "!";
    return null;
}

fn prepareScalarTag(self: *Parser, tag: ?[]const u8) !void {
    if (tag) |t| self.pending_scalar_tag = t;
}

fn looksLikeStructure(content: []const u8) bool {
    if (content.len == 0) return false;
    if (content[0] == '-' and (content.len == 1 or content[1] == ' ' or content[1] == '\t')) return true;
    if (content[0] == '?' and (content.len == 1 or content[1] == ' ' or content[1] == '\t')) return true;
    if (Scanner.findInlineMappingColon(content) != null) return true;
    return false;
}

fn isAliasToken(raw: []const u8) ?[]const u8 {
    const value = std.mem.trim(u8, raw, " \t");
    if (value.len <= 1 or value[0] != '*') return null;
    var end: usize = 1;
    while (end < value.len and Scanner.isBlockAnchorChar(value[end])) : (end += 1) {}
    const name = value[1..end];
    if (name.len == 0) return null;
    // Check the rest is whitespace
    const rest = std.mem.trim(u8, value[end..], " ");
    if (rest.len != 0) return null;
    return name;
}

fn isSpace(c: u8) bool {
    return c == ' ' or c == '\t' or c == '\n' or c == '\r';
}

fn pinBorrowSlice(self: *Parser, source_slice: []const u8) ?[]const u8 {
    const st = self.stream orelse return null;
    const pin = st.shared_pin orelse return null;
    if (source_slice.len == 0) return pin.bytes[0..0];
    if (!isSubslice(self.scanned.source, source_slice)) return null;
    const off = sourceOffset(self.scanned.source, source_slice);
    if (off + source_slice.len > pin.bytes.len) return null;
    return pin.bytes[off .. off + source_slice.len];
}

fn pushDocumentStart(self: *Parser, explicit: bool) anyerror!void {
    try self.emitEvent(.{
        .kind = .document_start,
        .data = .{ .document_start = .{ .explicit = explicit } },
    });
}

fn pushScalar(self: *Parser, value: []const u8, style: Token.ScalarStyle, anchor: ?[]const u8, span: @import("Span.zig")) anyerror!void {
    // Borrow plain slices of the input source; only copy when the value lives in a
    // temporary buffer (joined lines, block-scalar rebuild, normalized quotes, …).
    const borrow = value.len == 0 or isSubslice(self.scanned.source, value);
    const stored: []const u8 = if (borrow) value else try self.allocator.dupe(u8, value);
    errdefer if (!borrow) self.allocator.free(stored);

    try self.emitEvent(.{
        .kind = .scalar,
        .data = .{ .scalar = .{
            .value = stored,
            .style = style,
            .anchor = if (anchor) |a| try self.allocator.dupe(u8, a) else null,
            .span = span,
            .value_owned = !borrow,
        } },
    });
}

/// Push a GPA-allocated scalar buffer; ownership transfers to the event stream.
fn blockScalarIsBorrowed(self: *Parser, value: []const u8) bool {
    if (value.len == 0) return true;
    if (isSubslice(self.scanned.source, value)) return true;
    if (self.stream) |st| {
        if (st.shared_pin) |pin| {
            if (isSubslice(pin.bytes, value)) return true;
            if (pin.backing) |b| return isSubslice(b, value);
        }
    }
    return false;
}

fn pushBlockScalar(self: *Parser, value: []const u8, style: Token.ScalarStyle, anchor: ?[]const u8, span: @import("Span.zig")) anyerror!void {
    if (blockScalarIsBorrowed(self, value)) {
        try self.pushScalar(value, style, anchor, span);
    } else {
        try self.pushScalarOwned(value, style, anchor, span);
    }
}

/// Push a GPA-allocated scalar buffer; ownership transfers to the event stream.
fn pushScalarOwned(self: *Parser, value: []const u8, style: Token.ScalarStyle, anchor: ?[]const u8, span: @import("Span.zig")) anyerror!void {
    try self.emitEvent(.{
        .kind = .scalar,
        .data = .{ .scalar = .{
            .value = value,
            .style = style,
            .anchor = if (anchor) |a| try self.allocator.dupe(u8, a) else null,
            .span = span,
            .value_owned = value.len > 0,
            .pool_backed = value.len > 0 and self.scalar_pool_backed,
        } },
    });
}

/// Normalize then push, skipping the GPA copy for plain scalars that already
/// live as trimmed subslices of `scanned.source` (the CRD / bundle hot path).
fn pushScalarNormalized(
    self: *Parser,
    raw: []const u8,
    style: Token.ScalarStyle,
    anchor: ?[]const u8,
    span: @import("Span.zig"),
) anyerror!void {
    if (style == .plain) {
        const trimmed = std.mem.trim(u8, raw, " \t");
        if (trimmed.len == 0) {
            try self.pushScalar("", .plain, anchor, span);
            return;
        }
        if (isSubslice(self.scanned.source, trimmed)) {
            try self.pushScalar(trimmed, .plain, anchor, span);
            return;
        }
        const owned = try self.scalar_allocator.dupe(u8, trimmed);
        try self.pushScalarOwned(owned, .plain, anchor, span);
        return;
    }
    if (style == .single_quoted) {
        const inner = stripOuterQuotes(raw, '\'');
        if (std.mem.indexOf(u8, inner, "''") == null) {
            if (isSubslice(self.scanned.source, inner)) {
                try self.pushScalar(inner, style, anchor, span);
            } else {
                const owned = try self.scalar_allocator.dupe(u8, inner);
                try self.pushScalarOwned(owned, style, anchor, span);
            }
            return;
        }
    }
    if (style == .double_quoted) {
        const inner = stripOuterQuotes(raw, '"');
        if (std.mem.indexOfScalar(u8, inner, '\\') == null) {
            if (isSubslice(self.scanned.source, inner)) {
                try self.pushScalar(inner, style, anchor, span);
            } else {
                const owned = try self.scalar_allocator.dupe(u8, inner);
                try self.pushScalarOwned(owned, style, anchor, span);
            }
            return;
        }
    }
    const scalar = try normalizeScalar(self.scalar_allocator, raw, style);
    try self.pushScalarOwned(scalar, style, anchor, span);
}

fn isSubslice(haystack: []const u8, needle: []const u8) bool {
    if (needle.len == 0) return true;
    const n_start = @intFromPtr(needle.ptr);
    const h_start = @intFromPtr(haystack.ptr);
    const h_end = h_start + haystack.len;
    return n_start >= h_start and n_start + needle.len <= h_end;
}

fn pushSimple(self: *Parser, kind: EventModel.Kind, span: @import("Span.zig")) anyerror!void {
    try self.emitEvent(.{
        .kind = kind,
        .data = switch (kind) {
            .stream_start => .{ .stream_start = span },
            .stream_end => .{ .stream_end = span },
            .document_start => .{ .document_start = .{ .span = span } },
            .document_end => .{ .document_end = span },
            .sequence_end => .{ .sequence_end = span },
            .mapping_end => .{ .mapping_end = span },
            else => return Error.Parse.UnexpectedToken,
        },
    });
}
