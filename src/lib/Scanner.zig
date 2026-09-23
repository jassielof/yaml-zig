//! YAML scanning phase.
const std = @import("std");

const Error = @import("Error.zig");
const Mark = @import("Mark.zig");
const Options = @import("Options.zig");
const Span = @import("Span.zig");
const TokenModel = @import("Token.zig");

pub const LineKind = enum {
    sequence_item,
    mapping_entry,
    scalar,
};

pub const ScannedLine = struct {
    line_no: usize,
    indent: usize,
    kind: LineKind,
    key: []const u8 = "",
    key_style: TokenModel.ScalarStyle = .plain,
    value: []const u8 = "",
    style: TokenModel.ScalarStyle = .plain,
    span: Span = .{},
};

pub const ScannedDocument = struct {
    source: []const u8,
    lines: std.ArrayListUnmanaged(ScannedLine),
    /// Buffers for values joined across physical lines (multiline flow).
    owned: std.ArrayListUnmanaged([]u8) = .empty,

    pub fn deinit(self: *ScannedDocument, allocator: std.mem.Allocator) void {
        for (self.owned.items) |buf| allocator.free(buf);
        self.owned.deinit(allocator);
        self.lines.deinit(allocator);
        self.* = undefined;
    }
};

pub const Scanner = @This();

allocator: std.mem.Allocator,
source: []const u8,
options: Options.Parse,
lines: std.ArrayListUnmanaged(ScannedLine) = .empty,

pub fn init(allocator: std.mem.Allocator, source: []const u8, options: Options.Parse) Scanner {
    return .{
        .allocator = allocator,
        .source = source,
        .options = options,
    };
}

pub fn deinit(self: *Scanner) void {
    self.lines.deinit(self.allocator);
    self.* = undefined;
}

pub fn scan(self: *Scanner) !ScannedDocument {
    var physical: std.ArrayListUnmanaged([]const u8) = .empty;
    defer physical.deinit(self.allocator);
    var split = std.mem.splitScalar(u8, self.source, '\n');
    while (split.next()) |raw_line| {
        try physical.append(self.allocator, stripCarriageReturn(raw_line));
    }

    var owned: std.ArrayListUnmanaged([]u8) = .empty;
    errdefer {
        for (owned.items) |buf| self.allocator.free(buf);
        owned.deinit(self.allocator);
    }

    var line_no: usize = 0;
    var past_doc_start = false;

    while (line_no < physical.items.len) : (line_no += 1) {
        const line = physical.items[line_no];
        const indent = countIndent(line);
        var content = stripInlineComment(std.mem.trimStart(u8, line[indent..], " "));
        if (content.len == 0 or std.mem.startsWith(u8, content, "#")) continue;
        if (!past_doc_start and content[0] == '%') continue;
        // Document start "---" only when followed by space, tab, or end (not plain scalar like "---word1")
        if (content.len >= 3 and std.mem.eql(u8, content[0..3], "---")) {
            if (content.len == 3 or content[3] == ' ' or content[3] == '\t') {
                past_doc_start = true;
                content = std.mem.trimStart(u8, content[3..], " \t");
                if (content.len == 0) continue;
            }
        }
        if (content.len >= 3 and std.mem.eql(u8, content[0..3], "...")) {
            if (content.len == 3 or content[3] == ' ' or content[3] == '\t') {
                const after = std.mem.trim(u8, content[3..], " \t");
                if (after.len != 0) return Error.Parse.UnexpectedToken;
                continue;
            }
        }

        if (content[0] == '-' and (content.len == 1 or content[1] == ' ' or content[1] == '\t')) {
            const sequence_raw = if (content.len == 1)
                ""
            else
                stripInlineComment(std.mem.trimStart(u8, content[2..], " \t"));
            const sequence_value = try joinUnclosedFlow(self.allocator, physical.items, &line_no, sequence_raw, &owned);
            const sequence_style = detectStyle(sequence_value);
            try self.lines.append(self.allocator, .{
                .line_no = line_no,
                .indent = indent,
                .kind = .sequence_item,
                .value = trimPlainTrailing(sequence_value, sequence_style),
                .style = sequence_style,
                .span = makeSpan(line_no, indent, line.len),
            });
            continue;
        }

        // Explicit mapping key: ? key
        if (content[0] == '?' and (content.len == 1 or content[1] == ' ' or content[1] == '\t')) {
            const key = if (content.len <= 1)
                ""
            else
                std.mem.trim(u8, content[2..], " \t");
            const real_key = if (key.len > 0) stripInlineComment(key) else key;
            try self.lines.append(self.allocator, .{
                .line_no = line_no,
                .indent = indent,
                .kind = .mapping_entry,
                .key = if (real_key.len > 0) real_key else "~",
                .key_style = if (real_key.len > 0) detectStyle(real_key) else .plain,
                .value = "",
                .style = .plain,
                .span = makeSpan(line_no, indent, line.len),
            });
            continue;
        }

        if (findMappingColon(content)) |idx| {
            const key = std.mem.trim(u8, content[0..idx], " \t");
            if (key.len == 0) return Error.Parse.InvalidMappingKey;
            const raw_value = stripInlineComment(std.mem.trimStart(u8, content[idx + 1 ..], " \t"));
            const value = try joinUnclosedFlow(self.allocator, physical.items, &line_no, raw_value, &owned);
            const value_style = detectStyle(value);
            const key_style = detectStyle(key);
            try self.lines.append(self.allocator, .{
                .line_no = line_no,
                .indent = indent,
                .kind = .mapping_entry,
                .key = trimPlainTrailing(key, key_style),
                .key_style = key_style,
                .value = trimPlainTrailing(value, value_style),
                .style = value_style,
                .span = makeSpan(line_no, indent, line.len),
            });
            continue;
        }

        const scalar_value = try joinUnclosedFlow(self.allocator, physical.items, &line_no, content, &owned);
        try self.lines.append(self.allocator, .{
            .line_no = line_no,
            .indent = indent,
            .kind = .scalar,
            .value = scalar_value,
            .style = detectStyle(scalar_value),
            .span = makeSpan(line_no, indent, line.len),
        });
    }

    const result_lines = self.lines;
    self.lines = .empty;
    return .{
        .source = self.source,
        .lines = result_lines,
        .owned = owned,
    };
}

pub fn tokenizeFlow(
    allocator: std.mem.Allocator,
    text: []const u8,
    line_no: usize,
    column_base: usize,
    folded: *std.ArrayListUnmanaged([]u8),
) ![]TokenModel.Token {
    var out: std.ArrayListUnmanaged(TokenModel.Token) = .empty;
    defer out.deinit(allocator);

    var i: usize = 0;
    while (i < text.len) {
        const c = text[i];
        if (c == ' ' or c == '\t' or c == '\n' or c == '\r') {
            i += 1;
            continue;
        }

        const start_col = column_base + i;
        switch (c) {
            '[' => try out.append(allocator, .{ .kind = .lbracket, .span = makeSpan(line_no, start_col, start_col + 1) }),
            ']' => try out.append(allocator, .{ .kind = .rbracket, .span = makeSpan(line_no, start_col, start_col + 1) }),
            '{' => try out.append(allocator, .{ .kind = .lbrace, .span = makeSpan(line_no, start_col, start_col + 1) }),
            '}' => try out.append(allocator, .{ .kind = .rbrace, .span = makeSpan(line_no, start_col, start_col + 1) }),
            ',' => try out.append(allocator, .{ .kind = .comma, .span = makeSpan(line_no, start_col, start_col + 1) }),
            ':' => {
                if (!isFlowValueColon(text, i)) {
                    const start = i;
                    i += 1;
                    while (i < text.len and !isFlowValueColon(text, i) and !isFlowDelimiter(text[i]) and text[i] != '\n' and text[i] != '\r') : (i += 1) {
                        if (text[i] == '#' and (text[i - 1] == ' ' or text[i - 1] == '\t')) break;
                    }
                    const lexeme = std.mem.trim(u8, text[start..i], " \t");
                    if (lexeme.len == 0) continue;
                    try out.append(allocator, .{
                        .kind = .scalar,
                        .lexeme = lexeme,
                        .span = makeSpan(line_no, start_col, column_base + i),
                        .scalar_style = .plain,
                    });
                    continue;
                }
                try out.append(allocator, .{ .kind = .colon, .span = makeSpan(line_no, start_col, start_col + 1) });
            },
            '*', '&' => {
                const marker = c;
                i += 1;
                const name_start = i;
                while (i < text.len and isFlowNameChar(text[i])) : (i += 1) {}
                const lexeme = text[name_start..i];
                try out.append(allocator, .{
                    .kind = if (marker == '*') .alias else .scalar,
                    .lexeme = lexeme,
                    .span = makeSpan(line_no, start_col, column_base + i),
                    .scalar_style = .plain,
                });
                continue;
            },
            '#' => {
                // Comment: skip until end of line or end of text
                while (i < text.len and text[i] != '\n') : (i += 1) {}
                continue;
            },
            '!' => {
                i += 1;
                while (i < text.len and text[i] != ' ' and text[i] != '\t' and text[i] != '\n' and !isFlowDelimiter(text[i])) : (i += 1) {}
                continue;
            },
            '\'', '"' => {
                const quote = c;
                i += 1;
                const content_start = i;
                while (i < text.len) : (i += 1) {
                    if (text[i] == quote) {
                        if (quote == '\'' and i + 1 < text.len and text[i + 1] == '\'') {
                            i += 1;
                            continue;
                        }
                        break;
                    }
                    if (quote == '"' and text[i] == '\\' and i + 1 < text.len) {
                        i += 1;
                    }
                }
                if (i >= text.len) return Error.Parse.UnterminatedString;
                const content = text[content_start..i];
                i += 1;
                const lexeme = if (std.mem.indexOfAny(u8, content, "\n\r") != null)
                    try foldFlowQuoted(allocator, content, folded)
                else
                    content;
                try out.append(allocator, .{
                    .kind = .scalar,
                    .lexeme = lexeme,
                    .scalar_style = if (quote == '"') .double_quoted else .single_quoted,
                    .span = makeSpan(line_no, start_col, column_base + i),
                });
                continue;
            },
            else => {
                const start = i;
                while (i < text.len and !isFlowValueColon(text, i) and !isFlowDelimiter(text[i]) and text[i] != '\n' and text[i] != '\r') : (i += 1) {
                    if (text[i] == '#' and i > start and (text[i - 1] == ' ' or text[i - 1] == '\t')) break;
                }
                const lexeme = std.mem.trim(u8, text[start..i], " \t");
                if (lexeme.len == 0) continue;
                try out.append(allocator, .{
                    .kind = .scalar,
                    .lexeme = lexeme,
                    .span = makeSpan(line_no, column_base + start, column_base + i),
                    .scalar_style = .plain,
                });
                continue;
            },
        }
        i += 1;
    }

    try out.append(allocator, .{ .kind = .eof, .span = makeSpan(line_no, column_base + text.len, column_base + text.len) });
    return out.toOwnedSlice(allocator);
}

pub fn findInlineMappingColon(text: []const u8) ?usize {
    return findMappingColon(text);
}

fn stripCarriageReturn(line: []const u8) []const u8 {
    if (line.len > 0 and line[line.len - 1] == '\r') return line[0 .. line.len - 1];
    return line;
}

fn countIndent(line: []const u8) usize {
    var i: usize = 0;
    while (i < line.len and line[i] == ' ') : (i += 1) {}
    return i;
}

fn detectStyle(value: []const u8) TokenModel.ScalarStyle {
    if (value.len == 0) return .plain;
    if (value[0] == '\'') return .single_quoted;
    if (value[0] == '"') return .double_quoted;
    if (value[0] == '|') return .literal;
    if (value[0] == '>') return .folded;
    return .plain;
}

fn makeSpan(line_no: usize, start_col: usize, end_col: usize) Span {
    return .{
        .start = Mark{ .line = line_no, .column = start_col, .offset = 0 },
        .end = Mark{ .line = line_no, .column = end_col, .offset = 0 },
    };
}

fn findMappingColon(text: []const u8) ?usize {
    var in_single = false;
    var in_double = false;
    var depth_square: usize = 0;
    var depth_curly: usize = 0;
    var in_anchor = false;
    var skip_next = false;

    // The value indicator is the first ": " / ":\t" / trailing ":" that is not
    // inside quotes, flow collections, or an anchor/alias name. ':' is a legal
    // anchor character, including immediately before whitespace ("&a: key").
    for (text, 0..) |c, idx| {
        if (skip_next) {
            skip_next = false;
            continue;
        }
        if (in_anchor) {
            const ends = c == ' ' or c == '\t' or c == ',' or c == '[' or c == ']' or c == '{' or c == '}';
            if (!ends) continue;
            in_anchor = false;
        }
        switch (c) {
            '\\' => {
                if (in_double) skip_next = true;
            },
            '\'' => {
                if (!in_double) in_single = !in_single;
            },
            '"' => {
                if (!in_single) in_double = !in_double;
            },
            '[' => {
                if (!in_single and !in_double) depth_square += 1;
            },
            ']' => {
                if (!in_single and !in_double and depth_square > 0) depth_square -= 1;
            },
            '{' => {
                if (!in_single and !in_double) depth_curly += 1;
            },
            '}' => {
                if (!in_single and !in_double and depth_curly > 0) depth_curly -= 1;
            },
            '&', '*' => {
                if (!in_single and !in_double and depth_square == 0 and depth_curly == 0) {
                    if (idx == 0 or text[idx - 1] == ' ' or text[idx - 1] == '\t') in_anchor = true;
                }
            },
            ':' => {
                if (!in_single and !in_double and depth_square == 0 and depth_curly == 0) {
                    if (idx + 1 >= text.len or text[idx + 1] == ' ' or text[idx + 1] == '\t') return idx;
                }
            },
            else => {},
        }
    }
    return null;
}

fn trimPlainTrailing(text: []const u8, style: TokenModel.ScalarStyle) []const u8 {
    if (style != .plain) return text;
    return std.mem.trimEnd(u8, text, " \t");
}

const FlowBalance = struct {
    square: i32 = 0,
    curly: i32 = 0,
    quote: u8 = 0,
    escape: bool = false,

    fn closed(self: FlowBalance) bool {
        return self.square <= 0 and self.curly <= 0 and self.quote == 0 and !self.escape;
    }
};

fn feedFlowBalance(balance: *FlowBalance, text: []const u8) void {
    for (text) |c| {
        if (balance.escape) {
            balance.escape = false;
            continue;
        }
        if (balance.quote != 0) {
            if (balance.quote == '"' and c == '\\') {
                balance.escape = true;
                continue;
            }
            if (c == balance.quote) balance.quote = 0;
            continue;
        }
        switch (c) {
            '\'' => balance.quote = '\'',
            '"' => balance.quote = '"',
            '[' => balance.square += 1,
            ']' => if (balance.square > 0) {
                balance.square -= 1;
            },
            '{' => balance.curly += 1,
            '}' => if (balance.curly > 0) {
                balance.curly -= 1;
            },
            else => {},
        }
    }
}

fn flowUnclosed(text: []const u8) bool {
    var balance: FlowBalance = .{};
    feedFlowBalance(&balance, text);
    return balance.square > 0 or balance.curly > 0;
}

/// Pull following physical lines into `initial` until flow brackets and quotes balance.
/// `index` is updated to the last consumed line.
fn joinUnclosedFlow(
    allocator: std.mem.Allocator,
    lines: []const []const u8,
    index: *usize,
    initial: []const u8,
    owned: *std.ArrayListUnmanaged([]u8),
) ![]const u8 {
    if (!flowUnclosed(initial)) return initial;

    var buf: std.ArrayListUnmanaged(u8) = .empty;
    errdefer buf.deinit(allocator);
    try buf.appendSlice(allocator, initial);

    var i = index.* + 1;
    while (i < lines.len and flowUnclosed(buf.items)) : (i += 1) {
        try buf.append(allocator, '\n');
        try buf.appendSlice(allocator, lines[i]);
    }
    if (i == index.* + 1) return initial;

    index.* = i - 1;
    const slice = try buf.toOwnedSlice(allocator);
    try owned.append(allocator, slice);
    return slice;
}

fn stripInlineComment(text: []const u8) []const u8 {
    var in_single = false;
    var in_double = false;
    var depth_square: usize = 0;
    var depth_curly: usize = 0;
    var skip_next = false;

    for (text, 0..) |c, idx| {
        if (skip_next) {
            skip_next = false;
            continue;
        }
        switch (c) {
            '\\' => {
                if (in_double) skip_next = true;
            },
            '\'' => {
                if (!in_double) in_single = !in_single;
            },
            '"' => {
                if (!in_single) in_double = !in_double;
            },
            '[' => {
                if (!in_single and !in_double) depth_square += 1;
            },
            ']' => {
                if (!in_single and !in_double and depth_square > 0) depth_square -= 1;
            },
            '{' => {
                if (!in_single and !in_double) depth_curly += 1;
            },
            '}' => {
                if (!in_single and !in_double and depth_curly > 0) depth_curly -= 1;
            },
            '#' => {
                if (!in_single and !in_double and depth_square == 0 and depth_curly == 0) {
                    if (idx == 0 or text[idx - 1] == ' ' or text[idx - 1] == '\t') {
                        return std.mem.trimEnd(u8, text[0..idx], " \t");
                    }
                }
            },
            else => {},
        }
    }
    if (in_single or in_double) return text;
    return std.mem.trimStart(u8, text, " \t");
}

fn foldFlowQuoted(allocator: std.mem.Allocator, inner: []const u8, folded: *std.ArrayListUnmanaged([]u8)) ![]const u8 {
    var out: std.ArrayListUnmanaged(u8) = .empty;
    errdefer out.deinit(allocator);

    var i: usize = 0;
    while (i < inner.len) {
        if (inner[i] != '\n' and inner[i] != '\r') {
            try out.append(allocator, inner[i]);
            i += 1;
            continue;
        }

        var breaks: usize = 0;
        while (i < inner.len and (inner[i] == '\n' or inner[i] == '\r')) {
            if (inner[i] == '\r') i += 1;
            if (i < inner.len and inner[i] == '\n') i += 1;
            breaks += 1;
        }
        while (i < inner.len and (inner[i] == ' ' or inner[i] == '\t')) i += 1;

        if (breaks <= 1) {
            try out.append(allocator, ' ');
        } else {
            var extra: usize = 1;
            while (extra < breaks) : (extra += 1) try out.append(allocator, '\n');
        }
    }

    const slice = try out.toOwnedSlice(allocator);
    errdefer allocator.free(slice);
    try folded.append(allocator, slice);
    return slice;
}

fn isFlowDelimiter(c: u8) bool {
    return c == '[' or c == ']' or c == '{' or c == '}' or c == ',';
}

fn isFlowValueColon(text: []const u8, idx: usize) bool {
    if (idx >= text.len or text[idx] != ':') return false;
    if (idx + 1 >= text.len) return true;
    return switch (text[idx + 1]) {
        ' ', '\t', '\n', '\r', ',', ']', '}', '[', '{' => true,
        else => false,
    };
}

fn isFlowNameChar(c: u8) bool {
    return (c >= 'a' and c <= 'z') or
        (c >= 'A' and c <= 'Z') or
        (c >= '0' and c <= '9') or
        c == '_' or c == '-' or c == '.';
}

/// Anchor/alias names in block context: any non-whitespace, non-flow-indicator char.
pub fn isBlockAnchorChar(c: u8) bool {
    return switch (c) {
        ' ', '\t', '\n', '\r', '[', ']', '{', '}', ',' => false,
        0 => false,
        else => true,
    };
}
