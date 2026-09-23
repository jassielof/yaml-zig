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
    /// Value for a preceding `?` key (`: value`).
    explicit_value,
    /// Ends the current document. The next line starts another one.
    document_end,
    /// A `---` document with no content. Its node is null.
    empty_document,
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
    /// The key was introduced by `?`, so following indented lines belong to the key
    /// until a matching `:` line.
    explicit_key: bool = false,
    /// A comment-only line separates this line from the previous content line.
    after_comment: bool = false,
    /// A trailing `#` comment ended this line, so a plain scalar cannot continue.
    ends_with_comment: bool = false,
    /// This line is the first node of a document introduced by `---`.
    started_explicit: bool = false,
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
    var emitted_in_doc = false;
    var open_explicit = false;
    var pending_directive = false;
    var yaml_directive_count: usize = 0;
    var comment_pending = false;
    var saw_tag_directive = false;
    var doc_has_tag_directive = false;
    var next_doc_explicit = false;
    var awaiting_explicit_value = false;

    while (line_no < physical.items.len) : (line_no += 1) {
        const line = physical.items[line_no];
        const indent = countIndent(line);
        const raw_content = std.mem.trimStart(u8, line[indent..], " ");
        const line_ends_with_comment = hasInlineComment(raw_content);
        var content = stripInlineComment(raw_content);
        if (content.len == 0 or std.mem.startsWith(u8, content, "#")) {
            if (raw_content.len > 0 and raw_content[0] == '#') comment_pending = true;
            continue;
        }

        // Directives are recognized only before a document has content. A '%' line
        // after content is a plain scalar ("scalar\n%YAML 1.2").
        if (content[0] == '%' and !emitted_in_doc) {
            if (open_explicit) return Error.Parse.UnexpectedToken;
            try validateDirective(content, &yaml_directive_count);
            if (std.mem.startsWith(u8, content, "%TAG")) saw_tag_directive = true;
            pending_directive = true;
            continue;
        }

        var from_marker = false;
        // Document start "---" only when followed by space, tab, or end (not plain scalar like "---word1")
        if (isDocumentMarker(content, "---")) {
            const rest = stripInlineComment(std.mem.trimStart(u8, content[3..], " \t"));
            if (emitted_in_doc) {
                try self.appendMarker(.document_end, line_no);
                emitted_in_doc = false;
            } else if (open_explicit) {
                try self.appendMarker(.empty_document, line_no);
                self.lines.items[self.lines.items.len - 1].started_explicit = true;
            }
            open_explicit = true;
            pending_directive = false;
            yaml_directive_count = 0;
            doc_has_tag_directive = saw_tag_directive;
            saw_tag_directive = false;
            next_doc_explicit = true;
            awaiting_explicit_value = false;
            if (rest.len == 0) continue;
            content = rest;
            from_marker = true;
        }
        if (!from_marker and isDocumentMarker(content, "...")) {
            if (pending_directive) return Error.Parse.UnexpectedToken;
            const after = std.mem.trim(u8, content[3..], " \t");
            if (after.len != 0 and !std.mem.startsWith(u8, after, "#")) return Error.Parse.UnexpectedToken;
            if (emitted_in_doc) {
                try self.appendMarker(.document_end, line_no);
                emitted_in_doc = false;
            } else if (open_explicit) {
                try self.appendMarker(.empty_document, line_no);
                self.lines.items[self.lines.items.len - 1].started_explicit = true;
                next_doc_explicit = false;
            }
            open_explicit = false;
            pending_directive = false;
            doc_has_tag_directive = false;
            awaiting_explicit_value = false;
            continue;
        }

        if (pending_directive) return Error.Parse.UnexpectedToken;
        try rejectBadTag(content);
        // A `---` line already consumed the %TAG flag. Do not clear it on the
        // first content line of that explicit document.
        if (!from_marker and !emitted_in_doc and !open_explicit) {
            doc_has_tag_directive = saw_tag_directive;
            saw_tag_directive = false;
        }
        if (!doc_has_tag_directive and hasNamedTagHandle(content)) return Error.Parse.UnexpectedToken;
        if (hasTabbedBlockIndicator(line)) return Error.Parse.InvalidIndentation;
        // A block mapping cannot share the document-start line (`--- a: b`).
        if (from_marker and findMappingColon(content) != null) return Error.Parse.UnexpectedToken;
        // `&anchor - item` is not a sequence; the dash must start the line.
        if (anchorThenBlockEntry(content)) return Error.Parse.UnexpectedToken;

        emitted_in_doc = true;
        open_explicit = false;

        if (content[0] == '-' and (content.len == 1 or content[1] == ' ' or content[1] == '\t')) {
            const sequence_raw = if (content.len == 1)
                ""
            else
                stripInlineComment(std.mem.trimStart(u8, content[2..], " \t"));
            const sequence_value = try joinUnclosedFlow(self.allocator, physical.items, &line_no, sequence_raw, &owned, indent, false);
            const sequence_style = detectStyle(sequence_value);
            try ensureBlockHeader(sequence_style, sequence_value);
            try self.lines.append(self.allocator, .{
                .line_no = line_no,
                .indent = indent,
                .kind = .sequence_item,
                .value = trimPlainTrailing(sequence_value, sequence_style),
                .style = sequence_style,
                .span = makeSpan(line_no, indent, line.len),
            });
            self.stampComment(&comment_pending, line_ends_with_comment, &next_doc_explicit);
            continue;
        }

        // Explicit mapping key: ? key
        if (content[0] == '?' and (content.len == 1 or content[1] == ' ' or content[1] == '\t')) {
            const key = if (content.len <= 1)
                ""
            else
                std.mem.trim(u8, content[2..], " \t");
            const real_key = if (key.len > 0) stripInlineComment(key) else key;
            const key_style = if (real_key.len > 0) detectStyle(real_key) else .plain;
            try ensureBlockHeader(key_style, real_key);
            try self.lines.append(self.allocator, .{
                .line_no = line_no,
                .indent = indent,
                .kind = .mapping_entry,
                .key = if (real_key.len > 0) real_key else "~",
                .key_style = key_style,
                .value = "",
                .style = .plain,
                .span = makeSpan(line_no, indent, line.len),
                .explicit_key = true,
            });
            awaiting_explicit_value = true;
            self.stampComment(&comment_pending, line_ends_with_comment, &next_doc_explicit);
            continue;
        }

        // Explicit mapping value: ": value" at the start of the line.
        // Without a preceding `?`, this is an empty-key mapping entry (`: a`).
        if (content[0] == ':' and (content.len == 1 or content[1] == ' ' or content[1] == '\t')) {
            if (!awaiting_explicit_value) {
                const raw_value = if (content.len == 1)
                    ""
                else
                    stripInlineComment(std.mem.trimStart(u8, content[2..], " \t"));
                const value = try joinUnclosedFlow(self.allocator, physical.items, &line_no, raw_value, &owned, indent, false);
                const value_style = detectStyle(value);
                try ensureBlockHeader(value_style, value);
                try self.lines.append(self.allocator, .{
                    .line_no = line_no,
                    .indent = indent,
                    .kind = .mapping_entry,
                    .key = "",
                    .key_style = .plain,
                    .value = trimPlainTrailing(value, value_style),
                    .style = value_style,
                    .span = makeSpan(line_no, indent, line.len),
                });
                self.stampComment(&comment_pending, line_ends_with_comment, &next_doc_explicit);
                continue;
            }
            awaiting_explicit_value = false;
            const raw_value = if (content.len == 1)
                ""
            else
                stripInlineComment(std.mem.trimStart(u8, content[2..], " \t"));
            const value = try joinUnclosedFlow(self.allocator, physical.items, &line_no, raw_value, &owned, indent, false);
            const value_style = detectStyle(value);
            try ensureBlockHeader(value_style, value);
            try self.lines.append(self.allocator, .{
                .line_no = line_no,
                .indent = indent,
                .kind = .explicit_value,
                .value = trimPlainTrailing(value, value_style),
                .style = value_style,
                .span = makeSpan(line_no, indent, line.len),
            });
            self.stampComment(&comment_pending, line_ends_with_comment, &next_doc_explicit);
            continue;
        }

        if (findMappingColon(content)) |idx| {
            const key = std.mem.trim(u8, content[0..idx], " \t");
            if (key.len == 0) return Error.Parse.InvalidMappingKey;
            const raw_value = stripInlineComment(std.mem.trimStart(u8, content[idx + 1 ..], " \t"));
            const value = try joinUnclosedFlow(self.allocator, physical.items, &line_no, raw_value, &owned, indent, false);
            const value_style = detectStyle(value);
            const key_style = detectStyle(key);
            try ensureBlockHeader(key_style, key);
            try ensureBlockHeader(value_style, value);
            try rejectDanglingQuote(value);
            if (value_style == .plain and findMappingColon(value) != null and (value.len == 0 or (value[0] != '[' and value[0] != '{'))) {
                return Error.Parse.UnexpectedToken;
            }
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
            self.stampComment(&comment_pending, line_ends_with_comment, &next_doc_explicit);
            continue;
        }

        const flow_head = std.mem.trimStart(u8, content, " \t");
        const flow_opens_line = !from_marker and flow_head.len > 0 and (flow_head[0] == '[' or flow_head[0] == '{');
        const scalar_value = try joinUnclosedFlow(self.allocator, physical.items, &line_no, content, &owned, indent, flow_opens_line);
        const scalar_style = detectStyle(scalar_value);
        try ensureBlockHeader(scalar_style, scalar_value);
        try self.lines.append(self.allocator, .{
            .line_no = line_no,
            .indent = indent,
            .kind = .scalar,
            .value = scalar_value,
            .style = scalar_style,
            .span = makeSpan(line_no, indent, line.len),
        });
        self.stampComment(&comment_pending, line_ends_with_comment, &next_doc_explicit);
    }

    if (pending_directive) return Error.Parse.UnexpectedToken;
    if (open_explicit and !emitted_in_doc) try self.appendMarker(.empty_document, physical.items.len);

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
    // True after a quoted scalar until the next token. `:` then separates even
    // when it is not followed by whitespace (`"key":value`, `"foo"\n  :bar`).
    var json_key_ready = false;
    var pending_anchor: []const u8 = "";
    while (i < text.len) {
        const c = text[i];
        if (c == ' ' or c == '\t' or c == '\n' or c == '\r') {
            i += 1;
            continue;
        }

        const start_col = column_base + i;
        switch (c) {
            '[' => {
                json_key_ready = false;
                try out.append(allocator, .{ .kind = .lbracket, .span = makeSpan(line_no, start_col, start_col + 1) });
            },
            ']' => {
                json_key_ready = false;
                try out.append(allocator, .{ .kind = .rbracket, .span = makeSpan(line_no, start_col, start_col + 1) });
            },
            '{' => {
                json_key_ready = false;
                try out.append(allocator, .{ .kind = .lbrace, .span = makeSpan(line_no, start_col, start_col + 1) });
            },
            '}' => {
                json_key_ready = false;
                try out.append(allocator, .{ .kind = .rbrace, .span = makeSpan(line_no, start_col, start_col + 1) });
            },
            ',' => {
                json_key_ready = false;
                try out.append(allocator, .{ .kind = .comma, .span = makeSpan(line_no, start_col, start_col + 1) });
            },
            ':' => {
                if (!isFlowValueColon(text, i) and !json_key_ready) {
                    const start = i;
                    i = nextPlainEnd(text, start + 1);
                    const lexeme = try plainFlowLexeme(allocator, text[start..i], folded);
                    if (lexeme.len == 0) continue;
                    json_key_ready = false;
                    try out.append(allocator, .{
                        .kind = .scalar,
                        .lexeme = lexeme,
                        .span = makeSpan(line_no, start_col, column_base + i),
                        .scalar_style = .plain,
                        .anchor = pending_anchor,
                    });
                    pending_anchor = "";
                    continue;
                }
                json_key_ready = false;
                try out.append(allocator, .{
                    .kind = .colon,
                    .span = makeSpan(line_no, start_col, start_col + 1),
                    // A break between a flow-sequence key and ':' is invalid.
                    .indent = if (colonFollowsBreak(text, i)) 1 else 0,
                });
            },
            '*' => {
                json_key_ready = false;
                i += 1;
                const name_start = i;
                while (i < text.len and isFlowNameChar(text[i])) : (i += 1) {}
                const lexeme = text[name_start..i];
                try out.append(allocator, .{
                    .kind = .alias,
                    .lexeme = lexeme,
                    .span = makeSpan(line_no, start_col, column_base + i),
                    .scalar_style = .plain,
                });
                continue;
            },
            '&' => {
                json_key_ready = false;
                i += 1;
                const name_start = i;
                while (i < text.len and isFlowNameChar(text[i])) : (i += 1) {}
                pending_anchor = text[name_start..i];
                continue;
            },
            '?' => {
                if (i + 1 >= text.len or text[i + 1] == ' ' or text[i + 1] == '\t' or text[i + 1] == '\n' or text[i + 1] == '\r' or text[i + 1] == ',' or text[i + 1] == '}' or text[i + 1] == ']') {
                    i += 1;
                    var look = i;
                    while (look < text.len and (text[look] == ' ' or text[look] == '\t' or text[look] == '\n' or text[look] == '\r')) : (look += 1) {}
                    if (look >= text.len or text[look] == ',' or text[look] == '}' or text[look] == ']' or text[look] == ':') {
                        try out.append(allocator, .{
                            .kind = .scalar,
                            .lexeme = "",
                            .span = makeSpan(line_no, start_col, start_col + 1),
                            .scalar_style = .plain,
                        });
                    }
                    continue;
                }
                const start = i;
                i = nextPlainEnd(text, start);
                const lexeme = try plainFlowLexeme(allocator, text[start..i], folded);
                if (lexeme.len == 0) continue;
                json_key_ready = false;
                try out.append(allocator, .{
                    .kind = .scalar,
                    .lexeme = lexeme,
                    .span = makeSpan(line_no, start_col, column_base + i),
                    .scalar_style = .plain,
                    .anchor = pending_anchor,
                });
                pending_anchor = "";
                continue;
            },
            '#' => {
                if (i > 0) {
                    const prev = text[i - 1];
                    const separated = prev == ' ' or prev == '\t' or prev == '\n' or prev == '\r';
                    if (!separated) return Error.Parse.UnexpectedToken;
                }
                // A comment does not end a JSON-like key (`{ "foo" # comment\n  :bar }`).
                while (i < text.len and text[i] != '\n') : (i += 1) {}
                continue;
            },
            '!' => {
                json_key_ready = false;
                i += 1;
                while (i < text.len and text[i] != ' ' and text[i] != '\t' and text[i] != '\n' and text[i] != '\r' and text[i] != ':' and !isFlowDelimiter(text[i])) : (i += 1) {}
                var look = i;
                while (look < text.len and (text[look] == ' ' or text[look] == '\t' or text[look] == '\n' or text[look] == '\r')) : (look += 1) {}
                const bare = look >= text.len or text[look] == ',' or text[look] == ':' or text[look] == ']' or text[look] == '}' or text[look] == '#';
                if (bare) {
                    try out.append(allocator, .{
                        .kind = .scalar,
                        .lexeme = "",
                        .span = makeSpan(line_no, start_col, column_base + i),
                        .scalar_style = .double_quoted,
                    });
                    json_key_ready = true;
                }
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
                    .anchor = pending_anchor,
                });
                pending_anchor = "";
                json_key_ready = true;
                continue;
            },
            else => {
                const start = i;
                i = nextPlainEnd(text, start);
                const lexeme = try plainFlowLexeme(allocator, text[start..i], folded);
                if (lexeme.len == 0) continue;
                json_key_ready = false;
                try out.append(allocator, .{
                    .kind = .scalar,
                    .lexeme = lexeme,
                    .span = makeSpan(line_no, column_base + start, column_base + i),
                    .scalar_style = .plain,
                    .anchor = pending_anchor,
                });
                pending_anchor = "";
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

fn appendMarker(self: *Scanner, kind: LineKind, line_no: usize) !void {
    try self.lines.append(self.allocator, .{
        .line_no = line_no,
        .indent = 0,
        .kind = kind,
    });
}

fn tabBeforeColumn(line: []const u8, column: usize) bool {
    var i: usize = 0;
    while (i < line.len and i < column) : (i += 1) {
        if (line[i] == '\t') return true;
        if (line[i] != ' ') return false;
    }
    return false;
}

fn hasNamedTagHandle(text: []const u8) bool {
    var i: usize = 0;
    while (i < text.len) : (i += 1) {
        if (text[i] != '!') continue;
        if (i > 0 and text[i - 1] != ' ' and text[i - 1] != '\t') continue;
        if (i + 1 < text.len and (text[i + 1] == '!' or text[i + 1] == '<')) continue;
        var j = i + 1;
        while (j < text.len and isTagNameChar(text[j])) : (j += 1) {}
        if (j > i + 1 and j < text.len and text[j] == '!') return true;
    }
    return false;
}

fn isTagNameChar(c: u8) bool {
    return std.ascii.isAlphanumeric(c) or c == '-';
}

/// A tab is separating block structure (`-\t-`, `\tb:`, `:\tkey:`). A tab before a
/// plain scalar (`-\tbaz`, `-\t-1`) is separation, not indentation.
fn hasTabbedBlockIndicator(line: []const u8) bool {
    var i = countIndent(line);
    var saw_tab = false;
    while (i < line.len and (line[i] == ' ' or line[i] == '\t')) : (i += 1) {
        if (line[i] == '\t') saw_tab = true;
    }
    if (i >= line.len) return false;
    if (saw_tab and isBlockStructureToken(line[i..])) return true;
    if (line[i] != '-' and line[i] != '?' and line[i] != ':') return false;
    if (line[i] != ':' and i + 1 < line.len and line[i + 1] != ' ' and line[i + 1] != '\t') return false;
    var j = i + 1;
    var sep_tab = false;
    while (j < line.len and (line[j] == ' ' or line[j] == '\t')) : (j += 1) {
        if (line[j] == '\t') sep_tab = true;
    }
    if (!sep_tab or j >= line.len) return false;
    return isBlockStructureToken(line[j..]);
}

fn isBlockStructureToken(text: []const u8) bool {
    if (text.len == 0) return false;
    if ((text[0] == '-' or text[0] == '?' or text[0] == ':') and
        (text.len == 1 or text[1] == ' ' or text[1] == '\t'))
    {
        return true;
    }
    return findMappingColon(text) != null;
}

fn anchorThenBlockEntry(text: []const u8) bool {
    var rest = std.mem.trimStart(u8, text, " \t");
    if (rest.len == 0 or (rest[0] != '&' and rest[0] != '!')) return false;
    var saw_property = false;
    while (rest.len > 0 and (rest[0] == '&' or rest[0] == '!')) {
        saw_property = true;
        var i: usize = 1;
        if (rest[0] == '!' and i < rest.len and rest[i] == '<') {
            while (i < rest.len and rest[i] != '>') : (i += 1) {}
            if (i < rest.len) i += 1;
        } else {
            while (i < rest.len and rest[i] != ' ' and rest[i] != '\t') : (i += 1) {}
        }
        rest = std.mem.trimStart(u8, rest[i..], " \t");
    }
    if (!saw_property or rest.len == 0) return false;
    return rest[0] == '-' and (rest.len == 1 or rest[1] == ' ' or rest[1] == '\t');
}

fn rejectBadTag(text: []const u8) !void {
    // A plain scalar of symbols (`!"#$...{|}~`) is not a tag. Tags that contain
    // `{}[],` are followed by whitespace and a separate token.
    if (std.mem.indexOfAny(u8, text, " \t") == null) return;
    var i: usize = 0;
    while (i < text.len) : (i += 1) {
        const at_tag = text[i] == '!' and (i == 0 or text[i - 1] == ' ' or text[i - 1] == '\t');
        if (!at_tag) continue;
        i += 1;
        if (i < text.len and text[i] == '<') {
            while (i < text.len and text[i] != '>') : (i += 1) {}
            continue;
        }
        if (i < text.len and text[i] == '!') i += 1;
        while (i < text.len and text[i] != ' ' and text[i] != '\t') : (i += 1) {
            switch (text[i]) {
                '{', '}', '[', ']', ',' => return Error.Parse.UnexpectedToken,
                else => {},
            }
        }
    }
}

fn isDocumentMarker(content: []const u8, marker: []const u8) bool {
    if (!std.mem.startsWith(u8, content, marker)) return false;
    if (content.len == marker.len) return true;
    return content[marker.len] == ' ' or content[marker.len] == '\t';
}

fn validateDirective(content: []const u8, yaml_count: *usize) !void {
    const body = directiveBody(content);
    const name_end = std.mem.indexOfAny(u8, body, " \t") orelse body.len;
    const name = body[0..name_end];
    if (std.mem.eql(u8, name, "%YAML")) {
        const rest = std.mem.trim(u8, body[name_end..], " \t");
        // Exactly one version token. 1.1 and 1.2 are known; any other x.y is a
        // warning in the spec examples and still produces the following document.
        var tokens = std.mem.tokenizeAny(u8, rest, " \t");
        const version = tokens.next() orelse return Error.Parse.UnexpectedToken;
        if (tokens.next() != null) return Error.Parse.UnexpectedToken;
        if (!isYamlVersionToken(version)) return Error.Parse.UnexpectedToken;
        yaml_count.* += 1;
        if (yaml_count.* > 1) return Error.Parse.UnexpectedToken;
        return;
    }
    if (std.mem.eql(u8, name, "%TAG")) {
        const rest = std.mem.trim(u8, body[name_end..], " \t");
        const split_at = std.mem.indexOfAny(u8, rest, " \t") orelse return Error.Parse.UnexpectedToken;
        if (split_at == 0) return Error.Parse.UnexpectedToken;
        if (std.mem.trim(u8, rest[split_at..], " \t").len == 0) return Error.Parse.UnexpectedToken;
        return;
    }
    // Reserved directives (for example %FOO) are ignored.
    if (body.len < 2 or body[0] != '%') return Error.Parse.UnexpectedToken;
}

fn rejectDanglingQuote(text: []const u8) !void {
    if (text.len == 0 or (text[0] != '"' and text[0] != '\'')) return;
    const quote = text[0];
    var i: usize = 1;
    while (i < text.len) : (i += 1) {
        if (quote == '"' and text[i] == '\\' and i + 1 < text.len) {
            i += 1;
            continue;
        }
        if (text[i] == quote) {
            if (quote == '\'' and i + 1 < text.len and text[i + 1] == '\'') {
                i += 1;
                continue;
            }
            i += 1;
            break;
        }
    }
    if (i >= text.len) return;
    if (text[i] == ' ' or text[i] == '\t') {
        const rest = std.mem.trimStart(u8, text[i..], " \t");
        if (rest.len == 0 or rest[0] == '#') return;
    }
    return Error.Parse.UnexpectedToken;
}

fn isYamlVersionToken(token: []const u8) bool {
    const dot = std.mem.indexOfScalar(u8, token, '.') orelse return false;
    if (dot == 0 or dot + 1 >= token.len) return false;
    for (token[0..dot]) |c| if (c < '0' or c > '9') return false;
    for (token[dot + 1 ..]) |c| if (c < '0' or c > '9') return false;
    return true;
}

fn directiveBody(content: []const u8) []const u8 {
    var in_space = false;
    for (content, 0..) |c, idx| {
        if (c == ' ' or c == '\t') {
            in_space = true;
            continue;
        }
        if (c == '#' and in_space) return std.mem.trimEnd(u8, content[0..idx], " \t");
        in_space = false;
    }
    return std.mem.trimEnd(u8, content, " \t");
}

fn ensureBlockHeader(style: TokenModel.ScalarStyle, text: []const u8) !void {
    if (style != .literal and style != .folded) return;
    if (text.len == 0) return;
    var i: usize = 1;
    var saw_chomp = false;
    var saw_indent = false;
    while (i < text.len) : (i += 1) {
        switch (text[i]) {
            '+', '-' => {
                if (saw_chomp) return Error.Parse.UnexpectedToken;
                saw_chomp = true;
            },
            '1'...'9' => {
                if (saw_indent) return Error.Parse.UnexpectedToken;
                saw_indent = true;
            },
            ' ', '\t' => {
                const rest = std.mem.trimStart(u8, text[i..], " \t");
                if (rest.len == 0 or rest[0] == '#') return;
                return Error.Parse.UnexpectedToken;
            },
            else => return Error.Parse.UnexpectedToken,
        }
    }
}

fn findMappingColon(text: []const u8) ?usize {
    var in_single = false;
    var in_double = false;
    var depth_square: usize = 0;
    var depth_curly: usize = 0;
    var in_anchor = false;
    var skip_next = false;
    const idx_start: usize = 0;

    // A quoted key is quoted from the first character. An apostrophe later in a
    // plain key is just a plain character (`a!"'()*: safe`).
    if (text.len > 0 and (text[0] == '\'' or text[0] == '"')) {
        const quote = text[0];
        var i: usize = 1;
        while (i < text.len) : (i += 1) {
            if (quote == '"' and text[i] == '\\' and i + 1 < text.len) {
                i += 1;
                continue;
            }
            if (text[i] == quote) {
                if (quote == '\'' and i + 1 < text.len and text[i + 1] == '\'') {
                    i += 1;
                    continue;
                }
                i += 1;
                break;
            }
        }
        while (i < text.len and (text[i] == ' ' or text[i] == '\t')) : (i += 1) {}
        if (i < text.len and text[i] == ':') return i;
        return null;
    }

    // The value indicator is the first ": " / ":\t" / trailing ":" that is not
    // inside quotes, flow collections, or an anchor/alias name. ':' is a legal
    // anchor character, including immediately before whitespace ("&a: key").
    for (text[idx_start..], idx_start..) |c, idx| {
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
                if ((depth_square > 0 or depth_curly > 0) and !in_double) in_single = !in_single;
            },
            '"' => {
                if ((depth_square > 0 or depth_curly > 0) and !in_single) in_double = !in_double;
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

pub fn flowUnclosed(text: []const u8) bool {
    var balance: FlowBalance = .{};
    feedFlowBalance(&balance, text);
    return balance.square > 0 or balance.curly > 0;
}

/// Pull following physical lines into `initial` until flow brackets and quotes balance.
/// `index` is updated to the last consumed line.
fn stampComment(self: *Scanner, comment_pending: *bool, ends_with_comment: bool, next_doc_explicit: *bool) void {
    const line = &self.lines.items[self.lines.items.len - 1];
    line.after_comment = comment_pending.*;
    line.ends_with_comment = ends_with_comment;
    line.started_explicit = next_doc_explicit.*;
    comment_pending.* = false;
    next_doc_explicit.* = false;
}

fn hasInlineComment(text: []const u8) bool {
    return stripInlineComment(text).len != text.len;
}

fn joinUnclosedFlow(
    allocator: std.mem.Allocator,
    lines: []const []const u8,
    index: *usize,
    initial: []const u8,
    owned: *std.ArrayListUnmanaged([]u8),
    parent_indent: usize,
    opener_at_line_start: bool,
) ![]const u8 {
    if (!flowUnclosed(initial)) return initial;

    var buf: std.ArrayListUnmanaged(u8) = .empty;
    errdefer buf.deinit(allocator);
    try buf.appendSlice(allocator, initial);

    // Only a value that itself opens a flow collection is indentation-sensitive
    // (`flow: [a,\nb]`). A `{` inside later plain or block text is not a flow node.
    const trimmed_initial = std.mem.trimStart(u8, initial, " \t");
    const enforce_indent = trimmed_initial.len > 0 and (trimmed_initial[0] == '[' or trimmed_initial[0] == '{');
    // A flow node nested in a block value cannot continue at the parent's column.
    // A flow node that opens the line can (`[\nfoo: bar\n]`).
    const min_indent = if (opener_at_line_start) parent_indent else parent_indent + 1;

    var i = index.* + 1;
    while (i < lines.len and flowUnclosed(buf.items)) : (i += 1) {
        const cont = lines[i];
        const cont_text = std.mem.trim(u8, cont, " \t");
        if (cont_text.len != 0 and (isDocumentMarker(cont_text, "---") or isDocumentMarker(cont_text, "..."))) {
            return Error.Parse.UnexpectedToken;
        }
        if (enforce_indent and cont_text.len != 0 and (countIndent(cont) < min_indent or tabBeforeColumn(cont, min_indent))) {
            return Error.Parse.InvalidIndentation;
        }
        try buf.append(allocator, '\n');
        try buf.appendSlice(allocator, cont);
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

fn colonFollowsBreak(text: []const u8, idx: usize) bool {
    var k = idx;
    while (k > 0) {
        k -= 1;
        if (text[k] == '\n' or text[k] == '\r') return true;
        if (text[k] != ' ' and text[k] != '\t') return false;
    }
    return false;
}

fn nextPlainEnd(text: []const u8, start: usize) usize {
    var i = start;
    while (i < text.len and !isFlowValueColon(text, i) and !isFlowDelimiter(text[i])) : (i += 1) {
        if (text[i] == '#' and i > start and (text[i - 1] == ' ' or text[i - 1] == '\t' or text[i - 1] == '\n' or text[i - 1] == '\r')) break;
    }
    return i;
}

fn plainFlowLexeme(
    allocator: std.mem.Allocator,
    raw: []const u8,
    folded: *std.ArrayListUnmanaged([]u8),
) ![]const u8 {
    const lexeme = if (std.mem.indexOfAny(u8, raw, "\n\r") == null)
        std.mem.trim(u8, raw, " \t")
    else
        std.mem.trim(u8, try foldFlowQuoted(allocator, raw, folded), " \t");
    // A bare dash inside a flow collection is not a plain scalar (`[-]`, `[-, -]`).
    if (std.mem.eql(u8, lexeme, "-")) return Error.Parse.UnexpectedToken;
    return lexeme;
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
