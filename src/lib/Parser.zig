//! Parser that converts scanned lines into an event stream.
const std = @import("std");
const Error = @import("Error.zig");
const Options = @import("Options.zig");
const Scanner = @import("Scanner.zig");
const Token = @import("Token.zig");
const EventModel = @import("Event.zig");

pub const Parser = @This();

allocator: std.mem.Allocator,
scanned: Scanner.ScannedDocument,
options: Options.Parse,
events: std.ArrayListUnmanaged(EventModel.Event) = .{},
index: usize = 0,

pub fn init(allocator: std.mem.Allocator, scanned: Scanner.ScannedDocument, options: Options.Parse) Parser {
    return .{
        .allocator = allocator,
        .scanned = scanned,
        .options = options,
    };
}

pub fn deinit(self: *Parser) void {
    for (self.events.items) |ev| {
        switch (ev.kind) {
            .scalar => {
                self.allocator.free(ev.data.scalar.value);
                if (ev.data.scalar.anchor) |anchor| self.allocator.free(anchor);
            },
            .alias => self.allocator.free(ev.data.alias.name),
            .sequence_start => if (ev.data.sequence_start.anchor) |a| self.allocator.free(a),
            .mapping_start => if (ev.data.mapping_start.anchor) |a| self.allocator.free(a),
            else => {},
        }
    }
    self.events.deinit(self.allocator);
    self.scanned.deinit(self.allocator);
    self.* = undefined;
}

pub fn parse(self: *Parser) ![]EventModel.Event {
    try self.pushSimple(.stream_start, .{});
    try self.pushSimple(.document_start, .{});

    if (self.scanned.lines.items.len == 0) {
        try self.pushScalar("null", .plain, null, .{});
    } else {
        try self.parseBlockValue(self.scanned.lines.items[0].indent, true);
    }

    try self.pushSimple(.document_end, .{});
    try self.pushSimple(.stream_end, .{});

    const owned = try self.events.toOwnedSlice(self.allocator);
    self.events = .{};
    return owned;
}

fn parseBlockValue(self: *Parser, indent: usize, is_root: bool) anyerror!void {
    if (self.index >= self.scanned.lines.items.len) return;
    const line = self.scanned.lines.items[self.index];
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
        defer self.allocator.free(block);
        try self.pushScalar(block, line.style, null, line.span);
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
        const rest_after = std.mem.trim(u8, stripAnchorPrefix(stripped), " ");
        if (rest_after.len == 0 and self.index < self.scanned.lines.items.len) {
            const next = self.scanned.lines.items[self.index];
            const anchor_to_attach = if (anc.name.len > 0) anc.name else null;
            if (next.kind == .sequence_item and next.indent >= indent) {
                if (anchor_to_attach) |a| {
                    try self.events.append(self.allocator, .{
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
                                try self.pushScalar("null", .plain, null, sl.span);
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
                    try self.events.append(self.allocator, .{
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
    try self.events.append(self.allocator, .{
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
                try self.pushScalar("null", .plain, null, line.span);
            }
            continue;
        }

        // Check for virtual-empty value (only anchor/tag, no real content)
        if (line.style == .plain and isVirtualEmptyValue(line.value, .plain)) {
            const raw_trimmed = std.mem.trim(u8, line.value, " ");
            const has_str = std.mem.startsWith(u8, raw_trimmed, "!!str");
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
            if (has_str) {
                try self.pushScalar("", .double_quoted, seq_anc, line.span);
            } else {
                try self.pushScalar("null", .plain, seq_anc, line.span);
            }
            continue;
        }

        // Nested sequence: value starts with "- "
        const trimmed_val = std.mem.trimLeft(u8, line.value, " ");
        if (trimmed_val.len >= 2 and trimmed_val[0] == '-' and (trimmed_val[1] == ' ' or trimmed_val[1] == '\t')) {
            const nested_indent = indent + 2;
            try self.events.append(self.allocator, .{
                .kind = .sequence_start,
                .data = .{ .sequence_start = .{ .style = .block } },
            });
            // Emit inline first item
            const inner_item = std.mem.trimLeft(u8, trimmed_val[2..], " \t");
            if (inner_item.len == 0) {
                self.index += 1;
                if (self.index < self.scanned.lines.items.len and self.scanned.lines.items[self.index].indent > nested_indent) {
                    try self.parseBlockValue(self.scanned.lines.items[self.index].indent, false);
                } else {
                    try self.pushScalar("null", .plain, null, line.span);
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
                        try self.pushScalar("null", .plain, null, nl.span);
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
            defer self.allocator.free(block);
            try self.pushScalar(block, line.style, null, line.span);
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

            // Absorb sequence_item lines at indent > seq_indent as plain text (AB8U)
            while (self.index < self.scanned.lines.items.len) {
                const nl = self.scanned.lines.items[self.index];
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
            try self.parseScalarLikeValue(final_text, .plain, line.line_no, line.indent + 2);
        } else {
            try self.parseScalarLikeValue(line.value, line.style, line.line_no, line.indent + 2);
            self.index += 1;
        }
    }

    try self.pushSimple(.sequence_end, .{});
}

fn emitSequenceItemMapping(self: *Parser, line: Scanner.ScannedLine, seq_indent: usize) anyerror!void {
    var working = stripTagPrefix(std.mem.trim(u8, line.value, " "));
    const anchor_info = extractLeadingAnchor(working);
    if (anchor_info.name.len > 0) {
        working = anchor_info.rest;
    }

    const col_idx = Scanner.findInlineMappingColon(working) orelse unreachable;
    const key = std.mem.trimRight(u8, working[0..col_idx], " \t");
    const raw_val = std.mem.trimLeft(u8, working[col_idx + 1 ..], " \t");

    try self.events.append(self.allocator, .{
        .kind = .mapping_start,
        .data = .{ .mapping_start = .{ .style = .block } },
    });

    // Emit first key
    const ks = detectInlineStyle(key);
    const nk = try normalizeScalar(self.allocator, key, ks);
    defer self.allocator.free(nk);
    try self.pushScalar(nk, ks, null, .{});

    // Emit first value
    if (raw_val.len == 0) {
        self.index += 1;
        if (self.index < self.scanned.lines.items.len and self.scanned.lines.items[self.index].indent > seq_indent) {
            try self.parseBlockValue(self.scanned.lines.items[self.index].indent, false);
        } else {
            try self.pushScalar("null", .plain, null, .{});
        }
    } else {
        const vs = detectInlineStyle(raw_val);
        if (vs == .literal or vs == .folded) {
            const block = try self.collectBlockScalar(seq_indent + 2, vs, raw_val, line.line_no);
            defer self.allocator.free(block);
            try self.pushScalar(block, vs, null, .{});
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
        try self.events.append(self.allocator, .{
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
                    try self.pushScalar("null", .plain, null, sl.span);
                }
            } else {
                try self.parseScalarLikeValue(sl.value, sl.style, sl.line_no, sl.indent + 2);
            }
        }
        try self.pushSimple(.sequence_end, .{});
    } else if (next_line.kind == .mapping_entry) {
        try self.events.append(self.allocator, .{
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
    const key = std.mem.trimRight(u8, working[0..col_idx], " \t");
    const raw_val = std.mem.trimLeft(u8, working[col_idx + 1 ..], " \t");

    try self.events.append(self.allocator, .{
        .kind = .mapping_start,
        .data = .{ .mapping_start = .{ .style = .block } },
    });
    const ks = detectInlineStyle(key);
    const nk = try normalizeScalar(self.allocator, key, ks);
    defer self.allocator.free(nk);
    try self.pushScalar(nk, ks, null, .{});
    if (raw_val.len == 0) {
        try self.pushScalar("null", .plain, null, .{});
    } else {
        try self.parseScalarLikeValue(raw_val, detectInlineStyle(raw_val), line_no, parent_indent + 2);
    }
    try self.pushSimple(.mapping_end, .{});
}

fn emitNestedSequenceValue(self: *Parser, value: []const u8, parent_indent: usize, line_no: usize, span: @import("Span.zig")) anyerror!void {
    const trimmed = std.mem.trimLeft(u8, value, " ");
    if (trimmed.len >= 2 and trimmed[0] == '-' and (trimmed[1] == ' ' or trimmed[1] == '\t')) {
        try self.events.append(self.allocator, .{
            .kind = .sequence_start,
            .data = .{ .sequence_start = .{ .style = .block } },
        });
        const inner = std.mem.trimLeft(u8, trimmed[2..], " \t");
        if (inner.len == 0) {
            try self.pushScalar("null", .plain, null, span);
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
    try self.events.append(self.allocator, .{
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
    // Emit key
    const key_without_tag = stripTagPrefix(std.mem.trim(u8, line.key, " "));
    const key_anchor_info = extractLeadingAnchor(key_without_tag);
    const key_anchor: ?[]const u8 = if (key_anchor_info.name.len > 0) key_anchor_info.name else null;
    const key_without_anchor = if (key_anchor != null) key_anchor_info.rest else stripAnchorPrefix(key_without_tag);
    if (line.key_style == .plain) {
        if (isAliasToken(key_without_anchor)) |alias_name| {
            try self.events.append(self.allocator, .{
                .kind = .alias,
                .data = .{ .alias = .{ .name = try self.allocator.dupe(u8, alias_name), .span = line.span } },
            });
        } else {
            const effective_key_style = detectInlineStyle(key_without_anchor);
            const normalized_key = try normalizeScalar(self.allocator, key_without_anchor, effective_key_style);
            defer self.allocator.free(normalized_key);
            try self.pushScalar(normalized_key, .plain, key_anchor, line.span);
        }
    } else {
        const normalized_key = try normalizeScalar(self.allocator, key_without_anchor, line.key_style);
        defer self.allocator.free(normalized_key);
        try self.pushScalar(normalized_key, .plain, key_anchor, line.span);
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
                        try self.events.append(self.allocator, .{
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
                                    try self.pushScalar("null", .plain, null, sl.span);
                                }
                            } else {
                                try self.parseScalarLikeValue(sl.value, sl.style, sl.line_no, sl.indent + 2);
                            }
                        }
                        try self.pushSimple(.sequence_end, .{});
                        return;
                    } else if (next.kind == .mapping_entry) {
                        try self.events.append(self.allocator, .{
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
                                    defer self.allocator.free(block);
                                    try self.pushScalar(block, nn.style, nv_anchor, line.span);
                                    return;
                                }
                                if (nv_anchor) |anch| {
                                    if (nn.kind == .sequence_item and nn.indent >= parent_indent) {
                                        try self.events.append(self.allocator, .{
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
                                                    try self.pushScalar("null", .plain, null, sl.span);
                                                }
                                            } else {
                                                try self.parseScalarLikeValue(sl.value, sl.style, sl.line_no, sl.indent + 2);
                                            }
                                        }
                                        try self.pushSimple(.sequence_end, .{});
                                        return;
                                    } else if (nn.kind == .mapping_entry and nn.indent >= parent_indent) {
                                        try self.events.append(self.allocator, .{
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
                                try self.events.append(self.allocator, .{
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
                    try self.events.append(self.allocator, .{
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
                                try self.pushScalar("null", .plain, null, sl.span);
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
                try self.pushScalar("null", .plain, maybe_anchor, line.span);
            }
        } else {
            try self.pushScalar("null", .plain, maybe_anchor, line.span);
        }
        return;
    }

    if (line.style == .literal or line.style == .folded) {
        const block = try self.collectBlockScalar(line.indent, line.style, line.value, line.line_no);
        defer self.allocator.free(block);
        try self.pushScalar(block, line.style, null, line.span);
        return;
    }

    // Check for tag + block scalar indicator (e.g., "!foo >1" or "!!str |")
    if (line.style == .plain and line.value.len > 0) {
        const after_tag = stripTagPrefix(std.mem.trim(u8, line.value, " "));
        const after_anchor = stripAnchorPrefix(after_tag);
        const trimmed_after = std.mem.trimLeft(u8, after_anchor, " ");
        if (trimmed_after.len > 0 and (trimmed_after[0] == '|' or trimmed_after[0] == '>')) {
            const bs_style: Token.ScalarStyle = if (trimmed_after[0] == '|') .literal else .folded;
            const block = try self.collectBlockScalar(line.indent, bs_style, trimmed_after, line.line_no);
            defer self.allocator.free(block);
            try self.pushScalar(block, bs_style, null, line.span);
            return;
        }
    }

    // Check for tag prefix + unclosed quoted scalar (e.g., !!binary "\...)
    if (line.style == .plain and line.value.len > 0) {
        const after_tag2 = stripTagPrefix(std.mem.trim(u8, line.value, " "));
        const after_anchor2 = stripAnchorPrefix(after_tag2);
        const trimmed_after2 = std.mem.trimLeft(u8, after_anchor2, " ");
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

    // Build raw line index from source
    var raw_lines: std.ArrayListUnmanaged([]const u8) = .{};
    defer raw_lines.deinit(self.allocator);
    {
        var split = std.mem.splitScalar(u8, self.scanned.source, '\n');
        while (split.next()) |rl| {
            try raw_lines.append(self.allocator, stripCR(rl));
        }
        // Remove trailing empty entry from split (artifact of trailing newline)
        if (raw_lines.items.len > 0 and raw_lines.items[raw_lines.items.len - 1].len == 0) {
            _ = raw_lines.pop();
        }
    }

    var base_indent: ?usize = if (explicit_indent) |ei| parent_indent + ei else null;
    var block_end: usize = header_line_no + 1;
    var has_content = false;

    {
        var ri = header_line_no + 1;
        while (ri < raw_lines.items.len) : (ri += 1) {
            const rline = raw_lines.items[ri];
            const li = countSpaces(rline);
            const is_blank = (li >= rline.len);

            if (is_blank) {
                block_end = ri + 1;
                continue;
            }

            if (base_indent == null) {
                if (li <= parent_indent and looksLikeStructure(rline[li..])) break;
                if (li <= parent_indent and parent_indent > 0) break;
                base_indent = li;
            }
            if (li < base_indent.?) break;

            has_content = true;
            block_end = ri + 1;
        }
    }

    // Advance parser index past consumed scanned lines
    while (self.index < self.scanned.lines.items.len and
        self.scanned.lines.items[self.index].line_no < block_end)
    {
        self.index += 1;
    }

    const bi = base_indent orelse (parent_indent + 1);

    var out: std.ArrayListUnmanaged(u8) = .{};
    defer out.deinit(self.allocator);

    if (style == .literal) {
        var ri = header_line_no + 1;
        while (ri < block_end) : (ri += 1) {
            const rline = raw_lines.items[ri];
            const li = countSpaces(rline);

            if (li >= rline.len) {
                if (has_content and li > bi) {
                    try out.appendSlice(self.allocator, rline[bi..]);
                    try out.append(self.allocator, '\n');
                } else {
                    try out.append(self.allocator, '\n');
                }
            } else if (li >= bi) {
                try out.appendSlice(self.allocator, rline[bi..]);
                try out.append(self.allocator, '\n');
            } else {
                try out.append(self.allocator, '\n');
            }
        }
    } else {
        // Folded: accumulate blank lines and emit correct newline counts
        var blank_count: usize = 0;
        var first_content = true;
        var prev_was_more = false;

        var ri = header_line_no + 1;
        while (ri < block_end) : (ri += 1) {
            const rline = raw_lines.items[ri];
            const li = countSpaces(rline);
            const is_blank = (li >= rline.len);

            if (is_blank) {
                blank_count += 1;
                continue;
            }

            if (li < bi) break;

            const more_indented = li > bi;
            const content = rline[bi..];

            if (first_content) {
                var b: usize = 0;
                while (b < blank_count) : (b += 1) try out.append(self.allocator, '\n');
            } else {
                if (blank_count > 0) {
                    const extra: usize = if (prev_was_more or more_indented) 1 else 0;
                    var b: usize = 0;
                    while (b < blank_count + extra) : (b += 1) try out.append(self.allocator, '\n');
                } else if (prev_was_more or more_indented) {
                    try out.append(self.allocator, '\n');
                } else {
                    try out.append(self.allocator, ' ');
                }
            }

            try out.appendSlice(self.allocator, content);
            blank_count = 0;
            prev_was_more = more_indented;
            first_content = false;
        }

        // Trailing blank lines for keep chomping
        if (!first_content) {
            try out.append(self.allocator, '\n');
        }
        var b: usize = 0;
        while (b < blank_count) : (b += 1) try out.append(self.allocator, '\n');
    }

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
            if (has_content) try out.append(self.allocator, '\n');
        },
        .keep => {},
    }

    return out.toOwnedSlice(self.allocator);
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

    var has_nonspecific_tag = false;
    var has_str_tag = false;
    if (working_style == .plain) {
        const before_tag = working;
        working = stripTagPrefix(working);
        if (!std.mem.eql(u8, before_tag, working)) {
            const trimmed_bt = std.mem.trimLeft(u8, before_tag, " ");
            if (std.mem.startsWith(u8, trimmed_bt, "! ") or std.mem.eql(u8, trimmed_bt, "!")) {
                has_nonspecific_tag = true;
            }
            if (std.mem.startsWith(u8, trimmed_bt, "!!str")) {
                has_str_tag = true;
            }
        }
        const extracted = extractLeadingAnchor(working);
        if (extracted.name.len > 0) {
            anchor_name = extracted.name;
            working = extracted.rest;
        }
        const before_tag2 = working;
        working = stripTagPrefix(working);
        if (!std.mem.eql(u8, before_tag2, working)) {
            const trimmed_bt2 = std.mem.trimLeft(u8, before_tag2, " ");
            if (!has_nonspecific_tag and (std.mem.startsWith(u8, trimmed_bt2, "! ") or std.mem.eql(u8, trimmed_bt2, "!"))) {
                has_nonspecific_tag = true;
            }
            if (std.mem.startsWith(u8, trimmed_bt2, "!!str")) {
                has_str_tag = true;
            }
        }
        working_style = detectInlineStyle(working);
    }

    if (working.len == 0) {
        if (has_str_tag or has_nonspecific_tag) {
            try self.pushScalar("", .double_quoted, anchor_name, .{});
        } else {
            try self.pushScalar("null", .plain, anchor_name, .{});
        }
        return;
    }

    if (working_style == .plain and working[0] == '*') {
        var end: usize = 1;
        while (end < working.len) : (end += 1) {
            const c = working[end];
            // Stop at ":" when it's the key-value separator (followed by space or end)
            if (c == ':' and (end + 1 >= working.len or working[end + 1] == ' ' or working[end + 1] == '\t')) break;
            if (!Scanner.isBlockAnchorChar(c)) break;
        }
        const alias_name = working[1..end];
        if (alias_name.len == 0) return Error.Parse.InvalidAlias;
        try self.events.append(self.allocator, .{
            .kind = .alias,
            .data = .{ .alias = .{ .name = try self.allocator.dupe(u8, alias_name), .span = .{} } },
        });
        return;
    }

    if (working[0] == '[' or working[0] == '{') {
        const pre_flow_count = self.events.items.len;
        try self.parseFlow(working, line_no, col);
        if (anchor_name) |aname| {
            if (self.events.items.len > pre_flow_count) {
                const first = &self.events.items[pre_flow_count];
                switch (first.kind) {
                    .sequence_start => first.data.sequence_start.anchor = try self.allocator.dupe(u8, aname),
                    .mapping_start => first.data.mapping_start.anchor = try self.allocator.dupe(u8, aname),
                    else => {},
                }
            }
        }
        return;
    }

    if (working_style == .plain) {
        if (Scanner.findInlineMappingColon(working)) |idx| {
            const key = std.mem.trimRight(u8, working[0..idx], " ");
            const raw_val = std.mem.trimLeft(u8, working[idx + 1 ..], " ");
            if (key.len > 0) {
                try self.events.append(self.allocator, .{
                    .kind = .mapping_start,
                    .data = .{ .mapping_start = .{ .style = .flow } },
                });
                try self.pushScalar(key, .plain, null, .{});
                try self.parseScalarLikeValue(raw_val, detectInlineStyle(raw_val), line_no, col + idx + 1);
                try self.pushSimple(.mapping_end, .{});
                return;
            }
        }
    }

    const final_style: Token.ScalarStyle = if (has_nonspecific_tag and working_style == .plain) .double_quoted else working_style;
    const scalar = try normalizeScalar(self.allocator, working, final_style);
    defer self.allocator.free(scalar);
    try self.pushScalar(scalar, final_style, anchor_name, .{});
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
    const tokens = try Scanner.tokenizeFlow(self.allocator, text, line_no, col);
    defer self.allocator.free(tokens);
    var cursor: usize = 0;
    try self.parseFlowValue(tokens, &cursor);
}

fn parseFlowValue(self: *Parser, tokens: []const Token.Token, cursor: *usize) anyerror!void {
    if (cursor.* >= tokens.len) return Error.Parse.UnexpectedToken;
    const tok = tokens[cursor.*];
    switch (tok.kind) {
        .lbracket => {
            cursor.* += 1;
            try self.events.append(self.allocator, .{
                .kind = .sequence_start,
                .data = .{ .sequence_start = .{ .style = .flow } },
            });
            while (cursor.* < tokens.len and tokens[cursor.*].kind != .rbracket and tokens[cursor.*].kind != .eof) {
                try self.parseFlowValue(tokens, cursor);
                if (cursor.* < tokens.len and tokens[cursor.*].kind == .comma) cursor.* += 1;
            }
            if (cursor.* >= tokens.len or tokens[cursor.*].kind != .rbracket) return Error.Parse.UnterminatedFlowCollection;
            cursor.* += 1;
            try self.pushSimple(.sequence_end, tok.span);
        },
        .lbrace => {
            cursor.* += 1;
            try self.events.append(self.allocator, .{
                .kind = .mapping_start,
                .data = .{ .mapping_start = .{ .style = .flow } },
            });
            while (cursor.* < tokens.len and tokens[cursor.*].kind != .rbrace and tokens[cursor.*].kind != .eof) {
                const key = tokens[cursor.*];
                if (key.kind != .scalar) return Error.Parse.UnexpectedToken;
                const norm_key = try normalizeScalar(self.allocator, key.lexeme, key.scalar_style);
                defer self.allocator.free(norm_key);
                try self.pushScalar(norm_key, key.scalar_style, null, key.span);
                cursor.* += 1;
                if (cursor.* >= tokens.len) return Error.Parse.UnexpectedToken;
                if (tokens[cursor.*].kind == .colon) {
                    cursor.* += 1;
                    try self.parseFlowValue(tokens, cursor);
                } else {
                    try self.pushScalar("null", .plain, null, key.span);
                }
                if (cursor.* < tokens.len and tokens[cursor.*].kind == .comma) cursor.* += 1;
            }
            if (cursor.* >= tokens.len or tokens[cursor.*].kind != .rbrace) return Error.Parse.UnterminatedFlowCollection;
            cursor.* += 1;
            try self.pushSimple(.mapping_end, tok.span);
        },
        .alias => {
            try self.events.append(self.allocator, .{
                .kind = .alias,
                .data = .{ .alias = .{ .name = try self.allocator.dupe(u8, tok.lexeme), .span = tok.span } },
            });
            cursor.* += 1;
        },
        .scalar => {
            const normalized = try normalizeScalar(self.allocator, tok.lexeme, tok.scalar_style);
            defer self.allocator.free(normalized);
            try self.pushScalar(normalized, tok.scalar_style, null, tok.span);
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
            var out: std.ArrayListUnmanaged(u8) = .{};
            defer out.deinit(allocator);
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
            var out: std.ArrayListUnmanaged(u8) = .{};
            defer out.deinit(allocator);
            var i: usize = 0;
            while (i < inner.len) : (i += 1) {
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
                } else {
                    try out.append(allocator, inner[i]);
                }
            }
            return out.toOwnedSlice(allocator);
        },
        else => return allocator.dupe(u8, raw),
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
    var value = std.mem.trimLeft(u8, raw, " ");
    while (value.len > 1 and value[0] == '!' and !std.mem.startsWith(u8, value, "!=")) {
        var i: usize = 1;
        if (i < value.len and value[i] == '<') {
            i += 1;
            while (i < value.len and value[i] != '>') : (i += 1) {}
            if (i < value.len and value[i] == '>') i += 1;
        } else {
            while (i < value.len and !isSpace(value[i])) : (i += 1) {}
        }
        value = std.mem.trimLeft(u8, value[i..], " ");
    }
    return value;
}

fn stripAnchorPrefix(raw: []const u8) []const u8 {
    var value = std.mem.trimLeft(u8, raw, " ");
    while (value.len > 1 and value[0] == '&') {
        var i: usize = 1;
        while (i < value.len) : (i += 1) {
            const c = value[i];
            if (c == ':' and (i + 1 >= value.len or value[i + 1] == ' ' or value[i + 1] == '\t')) break;
            if (!Scanner.isBlockAnchorChar(c)) break;
        }
        if (i <= 1) break;
        value = std.mem.trimLeft(u8, value[i..], " :\t");
    }
    return value;
}

fn extractLeadingAnchor(raw: []const u8) struct { name: []const u8, rest: []const u8 } {
    var value = std.mem.trimLeft(u8, raw, " ");
    if (value.len <= 1 or value[0] != '&') return .{ .name = "", .rest = raw };

    var i: usize = 1;
    while (i < value.len) : (i += 1) {
        const c = value[i];
        // Stop before ":" when it's the key-value separator (e.g. "&x: value" -> name "x")
        if (c == ':' and (i + 1 >= value.len or value[i + 1] == ' ' or value[i + 1] == '\t')) break;
        if (!Scanner.isBlockAnchorChar(c)) break;
    }
    if (i <= 1) return .{ .name = "", .rest = raw };

    const name = value[1..i];
    // Strip optional ": " after anchor (e.g. "&a: key" -> key)
    value = std.mem.trimLeft(u8, value[i..], " :\t");
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
    var out: std.ArrayListUnmanaged(u8) = .{};
    defer out.deinit(self.allocator);

    try out.appendSlice(self.allocator, std.mem.trim(u8, initial, " "));

    var prev_line_no = self.scanned.lines.items[self.index].line_no;
    self.index += 1;

    while (self.index < self.scanned.lines.items.len) {
        const line = self.scanned.lines.items[self.index];
        if (line.indent < min_indent or line.kind != .scalar) break;

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
    }

    return out.toOwnedSlice(self.allocator);
}

fn collectMultilineQuotedScalar(self: *Parser, base_indent: usize, style: Token.ScalarStyle) ![]u8 {
    var out: std.ArrayListUnmanaged(u8) = .{};
    defer out.deinit(self.allocator);

    const quote: u8 = if (style == .double_quoted) '"' else '\'';

    const first = self.scanned.lines.items[self.index];
    try out.appendSlice(self.allocator, first.value);
    var prev_line_no = first.line_no;
    self.index += 1;

    while (self.index < self.scanned.lines.items.len and !hasClosingQuote(out.items, quote)) {
        const line = self.scanned.lines.items[self.index];
        if (line.indent < base_indent) break;

        const gap = if (line.line_no > prev_line_no) line.line_no - prev_line_no - 1 else 0;

        stripTrailingQuoteWs(&out, style);

        if (style == .double_quoted and hasTrailingEscapedNewline(out.items)) {
            _ = out.pop();
            const trimmed = std.mem.trimLeft(u8, line.value, " \t");
            try out.appendSlice(self.allocator, trimmed);
        } else {
            const trimmed = std.mem.trimLeft(u8, std.mem.trim(u8, line.value, " \t"), " \t");
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
    }

    return out.toOwnedSlice(self.allocator);
}

fn collectMultilineQuotedValue(self: *Parser, base_indent: usize, initial: []const u8, style: Token.ScalarStyle) ![]u8 {
    var out: std.ArrayListUnmanaged(u8) = .{};
    defer out.deinit(self.allocator);

    const quote: u8 = if (style == .double_quoted) '"' else '\'';
    try out.appendSlice(self.allocator, initial);
    var prev_line_no = self.scanned.lines.items[self.index].line_no;
    self.index += 1;

    while (self.index < self.scanned.lines.items.len and !hasClosingQuote(out.items, quote)) {
        const line = self.scanned.lines.items[self.index];
        if (line.indent < base_indent) break;

        const gap = if (line.line_no > prev_line_no) line.line_no - prev_line_no - 1 else 0;

        stripTrailingQuoteWs(&out, style);

        if (style == .double_quoted and hasTrailingEscapedNewline(out.items)) {
            _ = out.pop();
            const trimmed = std.mem.trimLeft(u8, line.value, " \t");
            try out.appendSlice(self.allocator, trimmed);
        } else {
            const trimmed = std.mem.trimLeft(u8, std.mem.trim(u8, line.value, " \t"), " \t");
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
    }

    return out.toOwnedSlice(self.allocator);
}

/// 9MQT/01: In double-quoted, "... x" -> "...x" (strip space after "..." at line start)
fn trimDocEndLine(content: []const u8, style: Token.ScalarStyle) struct { prefix: []const u8, rest: []const u8 } {
    if (style != .double_quoted or content.len <= 3) return .{ .prefix = "", .rest = content };
    if (!std.mem.eql(u8, content[0..3], "...")) return .{ .prefix = "", .rest = content };
    if (content.len == 3) return .{ .prefix = "", .rest = content };
    if (content[3] != ' ') return .{ .prefix = "", .rest = content };
    return .{ .prefix = "...", .rest = std.mem.trimLeft(u8, content[3..], " ") };
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
    if (text.len < 2) return false;
    if (text[0] != quote or text[text.len - 1] != quote) return false;
    if (quote == '"') {
        var slash_count: usize = 0;
        var i: usize = text.len - 2;
        while (true) {
            if (text[i] == '\\') {
                slash_count += 1;
            } else {
                break;
            }
            if (i == 0) break;
            i -= 1;
        }
        if ((slash_count % 2) != 0) return false;
    }
    return true;
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

fn countSpaces(line: []const u8) usize {
    var i: usize = 0;
    while (i < line.len and line[i] == ' ') : (i += 1) {}
    return i;
}

fn looksLikeStructure(content: []const u8) bool {
    if (content.len == 0) return false;
    if (content[0] == '-' and (content.len == 1 or content[1] == ' ' or content[1] == '\t')) return true;
    if (content[0] == '?' and (content.len == 1 or content[1] == ' ' or content[1] == '\t')) return true;
    if (findMappingColonForStructureCheck(content)) return true;
    return false;
}

fn findMappingColonForStructureCheck(text: []const u8) bool {
    for (text, 0..) |c, idx| {
        if (c == ':' and (idx + 1 >= text.len or text[idx + 1] == ' ' or text[idx + 1] == '\t')) {
            return true;
        }
    }
    return false;
}

fn stripCR(line: []const u8) []const u8 {
    if (line.len > 0 and line[line.len - 1] == '\r') return line[0 .. line.len - 1];
    return line;
}

fn isAliasToken(raw: []const u8) ?[]const u8 {
    const value = std.mem.trim(u8, raw, " ");
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

fn pushScalar(self: *Parser, value: []const u8, style: Token.ScalarStyle, anchor: ?[]const u8, span: @import("Span.zig")) anyerror!void {
    try self.events.append(self.allocator, .{
        .kind = .scalar,
        .data = .{ .scalar = .{
            .value = try self.allocator.dupe(u8, value),
            .style = style,
            .anchor = if (anchor) |a| try self.allocator.dupe(u8, a) else null,
            .span = span,
        } },
    });
}

fn pushSimple(self: *Parser, kind: EventModel.Kind, span: @import("Span.zig")) anyerror!void {
    try self.events.append(self.allocator, .{
        .kind = kind,
        .data = switch (kind) {
            .stream_start => .{ .stream_start = span },
            .stream_end => .{ .stream_end = span },
            .document_start => .{ .document_start = span },
            .document_end => .{ .document_end = span },
            .sequence_end => .{ .sequence_end = span },
            .mapping_end => .{ .mapping_end = span },
            else => return Error.Parse.UnexpectedToken,
        },
    });
}
