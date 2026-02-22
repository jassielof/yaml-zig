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

/// Release parser-owned allocations.
pub fn deinit(self: *Parser) void {
    for (self.events.items) |ev| {
        if (ev.kind == .scalar) {
            self.allocator.free(ev.data.scalar.value);
        }
    }
    self.events.deinit(self.allocator);
    self.scanned.deinit(self.allocator);
    self.* = undefined;
}

/// Parse scanned lines into a linear YAML event stream.
pub fn parse(self: *Parser) ![]EventModel.Event {
    try self.pushSimple(.stream_start, .{});
    try self.pushSimple(.document_start, .{});

    if (self.scanned.lines.items.len == 0) {
        try self.pushScalar("null", .plain, .{});
    } else {
        try self.parseBlockValue(self.scanned.lines.items[0].indent);
    }

    try self.pushSimple(.document_end, .{});
    try self.pushSimple(.stream_end, .{});

    const owned = try self.events.toOwnedSlice(self.allocator);
    self.events = .{};
    return owned;
}

fn parseBlockValue(self: *Parser, indent: usize) anyerror!void {
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

        if (line.value.len == 0) {
            self.index += 1;
            if (self.index < self.scanned.lines.items.len and self.scanned.lines.items[self.index].indent > indent) {
                try self.parseBlockValue(self.scanned.lines.items[self.index].indent);
            } else {
                try self.pushScalar("null", .plain, line.span);
            }
            continue;
        }

        if (line.style == .literal or line.style == .folded) {
            const block = try self.collectBlockScalar(line.indent, line.style);
            defer self.allocator.free(block);
            try self.pushScalar(block, line.style, line.span);
            continue;
        }

        try self.parseScalarLikeValue(line.value, line.style, line.line_no, line.indent + 2);
        self.index += 1;
    }

    try self.pushSimple(.sequence_end, .{});
}

fn parseBlockMapping(self: *Parser, indent: usize) anyerror!void {
    try self.events.append(self.allocator, .{
        .kind = .mapping_start,
        .data = .{ .mapping_start = .{ .style = .block } },
    });

    while (self.index < self.scanned.lines.items.len) {
        const line = self.scanned.lines.items[self.index];
        if (line.indent != indent or line.kind != .mapping_entry) break;

        try self.pushScalar(line.key, .plain, line.span);
        if (line.value.len == 0) {
            self.index += 1;
            if (self.index < self.scanned.lines.items.len and self.scanned.lines.items[self.index].indent > indent) {
                try self.parseBlockValue(self.scanned.lines.items[self.index].indent);
            } else {
                try self.pushScalar("null", .plain, line.span);
            }
            continue;
        }

        if (line.style == .literal or line.style == .folded) {
            const block = try self.collectBlockScalar(line.indent, line.style);
            defer self.allocator.free(block);
            try self.pushScalar(block, line.style, line.span);
            continue;
        }

        try self.parseScalarLikeValue(line.value, line.style, line.line_no, line.indent + line.key.len + 1);
        self.index += 1;
    }

    try self.pushSimple(.mapping_end, .{});
}

fn collectBlockScalar(self: *Parser, parent_indent: usize, style: Token.ScalarStyle) anyerror![]u8 {
    self.index += 1;
    var out: std.ArrayListUnmanaged(u8) = .{};
    defer out.deinit(self.allocator);

    while (self.index < self.scanned.lines.items.len) {
        const line = self.scanned.lines.items[self.index];
        if (line.indent <= parent_indent) break;

        if (style == .literal) {
            try out.appendSlice(self.allocator, line.value);
            try out.append(self.allocator, '\n');
        } else {
            if (out.items.len > 0) try out.append(self.allocator, ' ');
            try out.appendSlice(self.allocator, std.mem.trim(u8, line.value, " "));
        }
        self.index += 1;
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
    if (value.len == 0) {
        try self.pushScalar("null", .plain, .{});
        return;
    }

    if (value[0] == '[' or value[0] == '{') {
        try self.parseFlow(value, line_no, col);
        return;
    }

    if (style == .plain) {
        if (Scanner.findInlineMappingColon(value)) |idx| {
            const key = std.mem.trimRight(u8, value[0..idx], " ");
            const raw_val = std.mem.trimLeft(u8, value[idx + 1 ..], " ");
            if (key.len > 0) {
                try self.events.append(self.allocator, .{
                    .kind = .mapping_start,
                    .data = .{ .mapping_start = .{ .style = .flow } },
                });
                try self.pushScalar(key, .plain, .{});
                try self.parseScalarLikeValue(raw_val, detectInlineStyle(raw_val), line_no, col + idx + 1);
                try self.pushSimple(.mapping_end, .{});
                return;
            }
        }
    }

    const scalar = try normalizeScalar(self.allocator, value, style);
    defer self.allocator.free(scalar);
    try self.pushScalar(scalar, style, .{});
}

fn detectInlineStyle(value: []const u8) Token.ScalarStyle {
    if (value.len == 0) return .plain;
    if (value[0] == '\'') return .single_quoted;
    if (value[0] == '"') return .double_quoted;
    if (std.mem.eql(u8, value, "|")) return .literal;
    if (std.mem.eql(u8, value, ">")) return .folded;
    return .plain;
}

fn parseFlow(self: *Parser, text: []const u8, line_no: usize, col: usize) anyerror!void {
    const tokens = try Scanner.tokenizeFlow(self.allocator, text, line_no, col);
    defer self.allocator.free(tokens);
    var cursor: usize = 0;
    try self.parseFlowValue(tokens, &cursor);
}

fn parseFlowValue(self: *Parser, tokens: []const Token.Token, cursor: *usize) anyerror!void {
    const tok = tokens[cursor.*];
    switch (tok.kind) {
        .lbracket => {
            cursor.* += 1;
            try self.events.append(self.allocator, .{
                .kind = .sequence_start,
                .data = .{ .sequence_start = .{ .style = .flow } },
            });
            while (cursor.* < tokens.len and tokens[cursor.*].kind != .rbracket) {
                try self.parseFlowValue(tokens, cursor);
                if (tokens[cursor.*].kind == .comma) cursor.* += 1;
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
            while (cursor.* < tokens.len and tokens[cursor.*].kind != .rbrace) {
                const key = tokens[cursor.*];
                if (key.kind != .scalar) return Error.Parse.UnexpectedToken;
                try self.pushScalar(key.lexeme, key.scalar_style, key.span);
                cursor.* += 1;
                if (tokens[cursor.*].kind != .colon) return Error.Parse.UnexpectedToken;
                cursor.* += 1;
                try self.parseFlowValue(tokens, cursor);
                if (tokens[cursor.*].kind == .comma) cursor.* += 1;
            }
            if (cursor.* >= tokens.len or tokens[cursor.*].kind != .rbrace) return Error.Parse.UnterminatedFlowCollection;
            cursor.* += 1;
            try self.pushSimple(.mapping_end, tok.span);
        },
        .alias => {
            try self.events.append(self.allocator, .{
                .kind = .alias,
                .data = .{ .alias = .{ .name = tok.lexeme, .span = tok.span } },
            });
            cursor.* += 1;
        },
        .scalar => {
            const normalized = try normalizeScalar(self.allocator, tok.lexeme, tok.scalar_style);
            defer self.allocator.free(normalized);
            try self.pushScalar(normalized, tok.scalar_style, tok.span);
            cursor.* += 1;
        },
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
                    const esc: u8 = switch (inner[i]) {
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        '"' => '"',
                        '\\' => '\\',
                        else => return Error.Parse.InvalidEscapeSequence,
                    };
                    try out.append(allocator, esc);
                } else {
                    try out.append(allocator, inner[i]);
                }
            }
            return out.toOwnedSlice(allocator);
        },
        else => return allocator.dupe(u8, raw),
    }
}

fn stripOuterQuotes(raw: []const u8, quote: u8) []const u8 {
    if (raw.len >= 2 and raw[0] == quote and raw[raw.len - 1] == quote) {
        return raw[1 .. raw.len - 1];
    }
    return raw;
}

fn pushScalar(self: *Parser, value: []const u8, style: Token.ScalarStyle, span: @import("Span.zig")) anyerror!void {
    try self.events.append(self.allocator, .{
        .kind = .scalar,
        .data = .{ .scalar = .{
            .value = try self.allocator.dupe(u8, value),
            .style = style,
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
