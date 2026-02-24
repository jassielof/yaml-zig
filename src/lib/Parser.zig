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
        switch (ev.kind) {
            .scalar => {
                self.allocator.free(ev.data.scalar.value);
                if (ev.data.scalar.anchor) |anchor| self.allocator.free(anchor);
            },
            .alias => self.allocator.free(ev.data.alias.name),
            else => {},
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
        try self.pushScalar("null", .plain, null, .{});
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
                try self.pushScalar("null", .plain, null, line.span);
            }
            continue;
        }

        if (line.style == .plain and std.mem.startsWith(u8, std.mem.trimLeft(u8, line.value, " "), "- ")) {
            const nested_indent = line.indent + 2;
            try self.events.append(self.allocator, .{
                .kind = .sequence_start,
                .data = .{ .sequence_start = .{ .style = .block } },
            });

            const first_nested = std.mem.trimLeft(u8, std.mem.trimLeft(u8, line.value, " ")[2..], " ");
            try self.parseScalarLikeValue(first_nested, detectInlineStyle(first_nested), line.line_no, line.indent + 2);
            self.index += 1;

            while (self.index < self.scanned.lines.items.len) {
                const nested_line = self.scanned.lines.items[self.index];
                if (nested_line.indent != nested_indent or nested_line.kind != .sequence_item) break;

                if (nested_line.value.len == 0) {
                    self.index += 1;
                    if (self.index < self.scanned.lines.items.len and self.scanned.lines.items[self.index].indent > nested_indent) {
                        try self.parseBlockValue(self.scanned.lines.items[self.index].indent);
                    } else {
                        try self.pushScalar("null", .plain, null, nested_line.span);
                    }
                    continue;
                }

                try self.parseScalarLikeValue(nested_line.value, nested_line.style, nested_line.line_no, nested_line.indent + 2);
                self.index += 1;
            }

            try self.pushSimple(.sequence_end, .{});
            continue;
        }

        if (line.value.len == 0) {
            self.index += 1;
            if (self.index < self.scanned.lines.items.len and self.scanned.lines.items[self.index].indent > indent) {
                try self.parseBlockValue(self.scanned.lines.items[self.index].indent);
            } else {
                try self.pushScalar("null", .plain, null, line.span);
            }
            continue;
        }

        if (line.style == .literal or line.style == .folded) {
            const block = try self.collectBlockScalar(line.indent, line.style, line.value, line.line_no);
            defer self.allocator.free(block);
            try self.pushScalar(block, line.style, null, line.span);
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

        const key_without_tag = stripTagPrefix(std.mem.trim(u8, line.key, " "));
        const key_without_anchor = stripAnchorPrefix(key_without_tag);
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
                try self.pushScalar(normalized_key, .plain, null, line.span);
            }
        } else {
            const effective_key_style = line.key_style;
            const normalized_key = try normalizeScalar(self.allocator, key_without_anchor, effective_key_style);
            defer self.allocator.free(normalized_key);
            try self.pushScalar(normalized_key, .plain, null, line.span);
        }

        const value_is_virtual_empty = isVirtualEmptyValue(line.value, line.style);
        if (value_is_virtual_empty) {
            const maybe_anchor = if (line.style == .plain)
                extractLeadingAnchor(stripTagPrefix(std.mem.trim(u8, line.value, " "))).name
            else
                "";
            self.index += 1;
            if (self.index < self.scanned.lines.items.len and self.scanned.lines.items[self.index].indent > indent) {
                try self.parseBlockValue(self.scanned.lines.items[self.index].indent);
            } else {
                try self.pushScalar("null", .plain, if (maybe_anchor.len > 0) maybe_anchor else null, line.span);
            }
            continue;
        }

        if (line.style == .literal or line.style == .folded) {
            const block = try self.collectBlockScalar(line.indent, line.style, line.value, line.line_no);
            defer self.allocator.free(block);
            try self.pushScalar(block, line.style, null, line.span);
            continue;
        }

        if (line.style == .plain and line.value.len > 0) {
            const joined = try self.collectPlainContinuation(line.indent, line.value);
            defer self.allocator.free(joined);
            try self.parseScalarLikeValue(joined, .plain, line.line_no, line.indent + line.key.len + 1);
            continue;
        }

        try self.parseScalarLikeValue(line.value, line.style, line.line_no, line.indent + line.key.len + 1);
        self.index += 1;
    }

    try self.pushSimple(.mapping_end, .{});
}

fn collectBlockScalar(
    self: *Parser,
    parent_indent: usize,
    style: Token.ScalarStyle,
    header_value: []const u8,
    header_line_no: usize,
) anyerror![]u8 {
    self.index += 1;
    var out: std.ArrayListUnmanaged(u8) = .{};
    defer out.deinit(self.allocator);

    var prev_line_no: ?usize = null;

    while (self.index < self.scanned.lines.items.len) {
        const line = self.scanned.lines.items[self.index];
        if (line.indent <= parent_indent) break;

        if (prev_line_no) |prev| {
            const gap = if (line.line_no > prev) line.line_no - prev - 1 else 0;
            if (style == .literal) {
                var g: usize = 0;
                while (g < gap) : (g += 1) try out.append(self.allocator, '\n');
            } else {
                if (gap > 0) {
                    var g: usize = 0;
                    while (g < gap) : (g += 1) try out.append(self.allocator, '\n');
                } else if (out.items.len > 0) {
                    try out.append(self.allocator, ' ');
                }
            }
        }

        if (style == .literal) {
            try out.appendSlice(self.allocator, line.value);
            try out.append(self.allocator, '\n');
        } else {
            try out.appendSlice(self.allocator, std.mem.trim(u8, line.value, " "));
        }
        prev_line_no = line.line_no;
        self.index += 1;
    }

    if (out.items.len == 0) {
        const boundary_line_no = if (self.index < self.scanned.lines.items.len)
            self.scanned.lines.items[self.index].line_no
        else
            countSourceLines(self.scanned.source);
        const blank_count = if (boundary_line_no > header_line_no) boundary_line_no - header_line_no - 1 else 0;
        var i: usize = 0;
        while (i < blank_count) : (i += 1) try out.append(self.allocator, '\n');
    }

    const chomp = detectChompMode(header_value);
    switch (chomp) {
        .strip => {
            while (out.items.len > 0 and out.items[out.items.len - 1] == '\n') {
                _ = out.pop();
            }
        },
        .clip => {
            var trailing: usize = 0;
            while (trailing < out.items.len and out.items[out.items.len - 1 - trailing] == '\n') : (trailing += 1) {}
            if (trailing == 0 and out.items.len > 0 and style == .literal) {
                try out.append(self.allocator, '\n');
            } else if (trailing > 1) {
                out.items.len -= (trailing - 1);
            }
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

    if (working_style == .plain) {
        working = stripTagPrefix(working);
        const extracted = extractLeadingAnchor(working);
        if (extracted.name.len > 0) {
            anchor_name = extracted.name;
            working = extracted.rest;
        }
        working_style = detectInlineStyle(working);
    }

    if (working.len == 0) {
        try self.pushScalar("null", .plain, anchor_name, .{});
        return;
    }

    if (working_style == .plain and working[0] == '*') {
        const alias_name = std.mem.trimLeft(u8, working[1..], " ");
        if (alias_name.len == 0) return Error.Parse.InvalidAlias;
        try self.events.append(self.allocator, .{
            .kind = .alias,
            .data = .{ .alias = .{ .name = try self.allocator.dupe(u8, alias_name), .span = .{} } },
        });
        return;
    }

    if (working[0] == '[' or working[0] == '{') {
        try self.parseFlow(working, line_no, col);
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

    const scalar = try normalizeScalar(self.allocator, working, working_style);
    defer self.allocator.free(scalar);
    try self.pushScalar(scalar, working_style, anchor_name, .{});
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
                try self.pushScalar(key.lexeme, key.scalar_style, null, key.span);
                cursor.* += 1;
                if (cursor.* >= tokens.len) return Error.Parse.UnexpectedToken;
                if (tokens[cursor.*].kind == .colon) {
                    cursor.* += 1;
                    try self.parseFlowValue(tokens, cursor);
                } else {
                    try self.pushScalar("null", .plain, null, key.span);
                }
                if (tokens[cursor.*].kind == .comma) cursor.* += 1;
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

fn stripTagPrefix(raw: []const u8) []const u8 {
    var value = std.mem.trimLeft(u8, raw, " ");
    while (value.len > 1 and value[0] == '!' and !std.mem.startsWith(u8, value, "!=") and !std.mem.startsWith(u8, value, "!=")) {
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
        while (i < value.len and isNameChar(value[i])) : (i += 1) {}
        value = std.mem.trimLeft(u8, value[i..], " ");
    }
    return value;
}

fn extractLeadingAnchor(raw: []const u8) struct { name: []const u8, rest: []const u8 } {
    var value = std.mem.trimLeft(u8, raw, " ");
    if (value.len <= 1 or value[0] != '&') return .{ .name = "", .rest = raw };

    var i: usize = 1;
    while (i < value.len and isNameChar(value[i])) : (i += 1) {}
    if (i <= 1) return .{ .name = "", .rest = raw };

    const name = value[1..i];
    value = std.mem.trimLeft(u8, value[i..], " ");
    return .{ .name = name, .rest = value };
}

fn isVirtualEmptyValue(value: []const u8, style: Token.ScalarStyle) bool {
    if (std.mem.trim(u8, value, " ").len == 0) return true;
    if (style != .plain) return false;
    const no_tag = stripTagPrefix(std.mem.trim(u8, value, " "));
    const no_anchor = stripAnchorPrefix(no_tag);
    return std.mem.trim(u8, no_anchor, " ").len == 0;
}

fn collectPlainContinuation(self: *Parser, base_indent: usize, initial: []const u8) ![]u8 {
    var out: std.ArrayListUnmanaged(u8) = .{};
    defer out.deinit(self.allocator);

    try out.appendSlice(self.allocator, std.mem.trim(u8, initial, " "));

    var prev_line_no = self.scanned.lines.items[self.index].line_no;
    self.index += 1;

    while (self.index < self.scanned.lines.items.len) {
        const line = self.scanned.lines.items[self.index];
        if (line.indent <= base_indent or line.kind != .scalar) break;

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
    self.index += 1;

    while (self.index < self.scanned.lines.items.len and !hasClosingQuote(out.items, quote)) {
        const line = self.scanned.lines.items[self.index];
        if (line.indent < base_indent) break;
        try out.append(self.allocator, ' ');
        const trimmed = std.mem.trim(u8, line.value, " \t");
        if (std.mem.startsWith(u8, trimmed, "... ")) {
            try out.appendSlice(self.allocator, "...");
            try out.appendSlice(self.allocator, std.mem.trimLeft(u8, trimmed[4..], " "));
        } else {
            try out.appendSlice(self.allocator, trimmed);
        }
        self.index += 1;
    }

    return out.toOwnedSlice(self.allocator);
}

fn hasClosingQuote(text: []const u8, quote: u8) bool {
    if (text.len == 0) return false;
    if (text[text.len - 1] != quote) return false;
    if (quote == '"') {
        var slash_count: usize = 0;
        var i: usize = text.len - 1;
        while (i > 0) {
            i -= 1;
            if (text[i] == '\\') {
                slash_count += 1;
            } else {
                break;
            }
        }
        if ((slash_count % 2) != 0) return false;
    }
    return true;
}

const ChompMode = enum {
    clip,
    strip,
    keep,
};

fn detectChompMode(header_value: []const u8) ChompMode {
    const trimmed = std.mem.trim(u8, header_value, " ");
    if (trimmed.len == 0) return .clip;
    for (trimmed) |ch| {
        if (ch == '+') return .keep;
        if (ch == '-') return .strip;
    }
    return .clip;
}

fn countSourceLines(source: []const u8) usize {
    if (source.len == 0) return 0;
    var lines: usize = 1;
    for (source) |ch| {
        if (ch == '\n') lines += 1;
    }
    return lines;
}

fn isAliasToken(raw: []const u8) ?[]const u8 {
    const value = std.mem.trim(u8, raw, " ");
    if (value.len <= 1 or value[0] != '*') return null;
    const name = std.mem.trimLeft(u8, value[1..], " ");
    if (name.len == 0) return null;
    for (name) |ch| {
        if (!isNameChar(ch)) return null;
    }
    return name;
}

fn isSpace(c: u8) bool {
    return c == ' ' or c == '\t' or c == '\n' or c == '\r';
}

fn isNameChar(c: u8) bool {
    return (c >= 'a' and c <= 'z') or
        (c >= 'A' and c <= 'Z') or
        (c >= '0' and c <= '9') or
        c == '_' or c == '-';
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
