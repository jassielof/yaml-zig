//! YAML scanning phase.
const std = @import("std");
const Error = @import("Error.zig");
const Mark = @import("Mark.zig");
const Span = @import("Span.zig");
const Options = @import("Options.zig");
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

    pub fn deinit(self: *ScannedDocument, allocator: std.mem.Allocator) void {
        self.lines.deinit(allocator);
        self.* = undefined;
    }
};

pub const Scanner = @This();

allocator: std.mem.Allocator,
source: []const u8,
options: Options.Parse,
lines: std.ArrayListUnmanaged(ScannedLine) = .{},

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
    var split = std.mem.splitScalar(u8, self.source, '\n');
    var line_no: usize = 0;
    var past_doc_start = false;

    while (split.next()) |raw_line| : (line_no += 1) {
        const line = stripCarriageReturn(raw_line);
        const indent = try countIndent(line);
        var content = stripInlineComment(std.mem.trimLeft(u8, line[indent..], " "));
        if (content.len == 0 or std.mem.startsWith(u8, content, "#")) continue;
        if (!past_doc_start and content[0] == '%') continue;
        // Document start "---" only when followed by space, tab, or end (not plain scalar like "---word1")
        if (content.len >= 3 and std.mem.eql(u8, content[0..3], "---")) {
            if (content.len == 3 or content[3] == ' ' or content[3] == '\t') {
                past_doc_start = true;
                content = std.mem.trimLeft(u8, content[3..], " \t");
                if (content.len == 0) continue;
            }
        }
        if (std.mem.startsWith(u8, content, "...")) {
            const after = std.mem.trim(u8, content[3..], " \t");
            if (after.len == 0) continue;
        }

        if (content[0] == '-' and (content.len == 1 or content[1] == ' ' or content[1] == '\t')) {
            const sequence_value = if (content.len == 1)
                ""
            else
                stripInlineComment(std.mem.trimLeft(u8, content[2..], " \t"));
            try self.lines.append(self.allocator, .{
                .line_no = line_no,
                .indent = indent,
                .kind = .sequence_item,
                .value = sequence_value,
                .style = detectStyle(sequence_value),
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
            const key = std.mem.trimRight(u8, content[0..idx], " \t");
            if (key.len == 0) return Error.Parse.InvalidMappingKey;
            const value = stripInlineComment(std.mem.trimLeft(u8, content[idx + 1 ..], " \t"));
            try self.lines.append(self.allocator, .{
                .line_no = line_no,
                .indent = indent,
                .kind = .mapping_entry,
                .key = key,
                .key_style = detectStyle(key),
                .value = value,
                .style = detectStyle(value),
                .span = makeSpan(line_no, indent, line.len),
            });
            continue;
        }

        try self.lines.append(self.allocator, .{
            .line_no = line_no,
            .indent = indent,
            .kind = .scalar,
            .value = content,
            .style = detectStyle(content),
            .span = makeSpan(line_no, indent, line.len),
        });
    }

    const result_lines = self.lines;
    self.lines = .{};
    return .{
        .source = self.source,
        .lines = result_lines,
    };
}

pub fn tokenizeFlow(
    allocator: std.mem.Allocator,
    text: []const u8,
    line_no: usize,
    column_base: usize,
) ![]TokenModel.Token {
    var out: std.ArrayListUnmanaged(TokenModel.Token) = .{};
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
            ':' => try out.append(allocator, .{ .kind = .colon, .span = makeSpan(line_no, start_col, start_col + 1) }),
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
                try out.append(allocator, .{
                    .kind = .scalar,
                    .lexeme = content,
                    .scalar_style = if (quote == '"') .double_quoted else .single_quoted,
                    .span = makeSpan(line_no, start_col, column_base + i),
                });
                continue;
            },
            else => {
                const start = i;
                while (i < text.len and !isFlowDelimiter(text[i]) and text[i] != '\n' and text[i] != '\r') : (i += 1) {}
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

fn countIndent(line: []const u8) !usize {
    var i: usize = 0;
    while (i < line.len) : (i += 1) {
        if (line[i] == ' ') continue;
        if (line[i] == '\t') return Error.Parse.InvalidIndentation;
        break;
    }
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
    var skip_next = false;

    // Find colon that separates key from value.
    // Use first colon when value starts with "&" or "*" (e.g. "a: &x: foo" -> key "a").
    // Otherwise use rightmost (e.g. "key ends with two colons::: value" -> key "key ends with two colons::").
    var last_valid: ?usize = null;
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
                if (idx == 0 or in_single) {
                    if (!in_double) in_single = !in_single;
                }
            },
            '"' => {
                if (idx == 0 or in_double) {
                    if (!in_single) in_double = !in_double;
                }
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
            ':' => {
                if (!in_single and !in_double and depth_square == 0 and depth_curly == 0) {
                    if (idx + 1 >= text.len or text[idx + 1] == ' ' or text[idx + 1] == '\t') {
                        const after = std.mem.trimLeft(u8, text[idx + 1 ..], " \t");
                        if (after.len > 0 and (after[0] == '&' or after[0] == '*')) {
                            return idx;
                        }
                        last_valid = idx;
                    }
                }
            },
            else => {},
        }
    }
    return last_valid;
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
                        return std.mem.trimRight(u8, text[0..idx], " \t");
                    }
                }
            },
            else => {},
        }
    }
    if (in_single or in_double) return text;
    return std.mem.trimRight(u8, text, " \t");
}

fn isFlowDelimiter(c: u8) bool {
    return c == '[' or c == ']' or c == '{' or c == '}' or c == ',' or c == ':';
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
