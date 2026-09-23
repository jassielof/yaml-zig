//! YAML parser and serializer.
//!
//! This module exposes the public API. Internal implementation is split across focused files to keep each component understandable and maintainable.
const std = @import("std");

pub const Mark = @import("Mark.zig");
pub const Span = @import("Span.zig");
pub const Error = @import("Error.zig");
pub const Options = @import("Options.zig");
pub const Token = @import("Token.zig");
pub const Event = @import("Event.zig");
pub const Input = @import("Input.zig");
pub const Scanner = @import("Scanner.zig");
pub const Parser = @import("Parser.zig");
pub const Scalar = @import("Scalar.zig");
pub const Schema = @import("Schema.zig");
pub const Node = @import("Node.zig");
pub const Document = @import("Document.zig");
pub const Composer = @import("Composer.zig");
pub const Emitter = @import("Emitter.zig");
pub const Serializer = @import("Serializer.zig");

/// Parse YAML text into a heap-owning `Document`.
///
/// The returned document owns all parsed data via an internal arena and must be
/// released with `deinit`.
pub fn parseDocument(
    allocator: std.mem.Allocator,
    source: []const u8,
    options: Options.Parse,
) !Document {
    const docs = try parseStream(allocator, source, options);
    defer allocator.free(docs);
    if (docs.len == 0) return Document.init(allocator, .null);
    if (docs.len != 1) {
        for (docs) |*doc| doc.deinit();
        return Error.Parse.UnexpectedToken;
    }
    return docs[0];
}

/// Parse every document in a YAML stream.
///
/// An empty stream (no document markers and no content) returns an empty slice.
/// Each returned document must be deinited, and the slice must be freed.
pub fn parseStream(
    allocator: std.mem.Allocator,
    source: []const u8,
    options: Options.Parse,
) ![]Document {
    var scanner = Scanner.init(allocator, source, options);
    defer scanner.deinit();
    const scanned = try scanner.scan();

    var parser = Parser.init(allocator, scanned, options);
    defer parser.deinit();
    const events = try parser.parse();

    return Composer.composeStream(allocator, events, options);
}

test parseDocument {
    const allocator = std.testing.allocator;
    var doc = try parseDocument(allocator, "name: Alice\nage: 30", .{});
    defer doc.deinit();
    // verify the root is a mapping
    try std.testing.expect(doc.root.tag == .mapping);
}

/// Format a YAML stream as yaml-test-suite events (`+STR`, `=VAL`, ...).
pub fn formatTestsuiteEvents(allocator: std.mem.Allocator, source: []const u8) ![]u8 {
    var scanner = Scanner.init(allocator, source, .{});
    defer scanner.deinit();
    const scanned = try scanner.scan();

    var parser = Parser.init(allocator, scanned, .{});
    defer parser.deinit();
    const events = try parser.parse();
    defer Composer.freeEvents(allocator, events);

    var out: std.ArrayListUnmanaged(u8) = .empty;
    errdefer out.deinit(allocator);
    for (events) |ev| try appendSuiteEvent(allocator, &out, ev);
    return out.toOwnedSlice(allocator);
}

fn appendSuiteEvent(allocator: std.mem.Allocator, out: *std.ArrayListUnmanaged(u8), ev: Event.Event) !void {
    switch (ev.kind) {
        .stream_start => try out.appendSlice(allocator, "+STR\n"),
        .stream_end => try out.appendSlice(allocator, "-STR\n"),
        .document_start => {
            try out.appendSlice(allocator, "+DOC");
            if (ev.data.document_start.explicit) try out.appendSlice(allocator, " ---");
            try out.append(allocator, '\n');
        },
        .document_end => try out.appendSlice(allocator, "-DOC\n"),
        .mapping_start => {
            try out.appendSlice(allocator, "+MAP");
            if (ev.data.mapping_start.style == .flow) try out.appendSlice(allocator, " {}");
            try appendAnchorAndTag(allocator, out, ev.data.mapping_start.anchor, ev.data.mapping_start.tag);
            try out.append(allocator, '\n');
        },
        .mapping_end => try out.appendSlice(allocator, "-MAP\n"),
        .sequence_start => {
            try out.appendSlice(allocator, "+SEQ");
            if (ev.data.sequence_start.style == .flow) try out.appendSlice(allocator, " []");
            try appendAnchorAndTag(allocator, out, ev.data.sequence_start.anchor, ev.data.sequence_start.tag);
            try out.append(allocator, '\n');
        },
        .sequence_end => try out.appendSlice(allocator, "-SEQ\n"),
        .scalar => {
            try out.appendSlice(allocator, "=VAL");
            try appendAnchorAndTag(allocator, out, ev.data.scalar.anchor, ev.data.scalar.tag);
            try out.appendSlice(allocator, switch (ev.data.scalar.style) {
                .plain => " :",
                .single_quoted => " '",
                .double_quoted => " \"",
                .literal => " |",
                .folded => " >",
            });
            try appendEscapedSuiteText(allocator, out, ev.data.scalar.value);
            try out.append(allocator, '\n');
        },
        .alias => {
            try out.appendSlice(allocator, "=ALI *");
            try out.appendSlice(allocator, ev.data.alias.name);
            try out.append(allocator, '\n');
        },
    }
}

fn appendAnchorAndTag(allocator: std.mem.Allocator, out: *std.ArrayListUnmanaged(u8), anchor: ?[]const u8, tag: ?[]const u8) !void {
    if (anchor) |name| {
        try out.appendSlice(allocator, " &");
        try out.appendSlice(allocator, name);
    }
    if (tag) |text| {
        try out.appendSlice(allocator, " <");
        try out.appendSlice(allocator, text);
        try out.append(allocator, '>');
    }
}

fn appendEscapedSuiteText(allocator: std.mem.Allocator, out: *std.ArrayListUnmanaged(u8), text: []const u8) !void {
    var view = std.unicode.Utf8View.init(text) catch return error.InvalidUtf8;
    var iter = view.iterator();
    while (iter.nextCodepointSlice()) |slice| {
        const codepoint = std.unicode.utf8Decode(slice) catch return error.InvalidUtf8;
        switch (codepoint) {
            '\\' => try out.appendSlice(allocator, "\\\\"),
            0 => try out.appendSlice(allocator, "\\0"),
            '\x08' => try out.appendSlice(allocator, "\\b"),
            '\x0c' => try out.appendSlice(allocator, "\\f"),
            '\n' => try out.appendSlice(allocator, "\\n"),
            '\r' => try out.appendSlice(allocator, "\\r"),
            '\t' => try out.appendSlice(allocator, "\\t"),
            '\x07' => try out.appendSlice(allocator, "\\a"),
            '\x0b' => try out.appendSlice(allocator, "\\v"),
            '\x1b' => try out.appendSlice(allocator, "\\e"),
            0x85 => try out.appendSlice(allocator, "\\N"),
            0xa0 => try out.appendSlice(allocator, "\\_"),
            0x2028 => try out.appendSlice(allocator, "\\L"),
            0x2029 => try out.appendSlice(allocator, "\\P"),
            else => {
                if ((codepoint >= 0x01 and codepoint <= 0x1f) or codepoint == 0x7f or (codepoint >= 0x80 and codepoint <= 0x9f)) {
                    var buf: [4]u8 = undefined;
                    const escaped = std.fmt.bufPrint(&buf, "\\x{x:0>2}", .{codepoint}) catch return error.InvalidUtf8;
                    try out.appendSlice(allocator, escaped);
                } else {
                    try out.appendSlice(allocator, slice);
                }
            },
        }
    }
}

/// Parse YAML text and return only the root node tree.
pub fn parseNode(
    allocator: std.mem.Allocator,
    source: []const u8,
    options: Options.Parse,
) !Node {
    var document = try parseDocument(allocator, source, options);
    defer document.deinit();
    return try document.cloneRoot(allocator);
}

/// Serialize a parsed document into UTF-8 YAML bytes.
///
/// Returned bytes are allocated with `allocator`.
pub fn stringifyDocument(
    allocator: std.mem.Allocator,
    document: *const Document,
    options: Options.Stringify,
) ![]u8 {
    return Serializer.stringifyDocument(allocator, document, options);
}

test stringifyDocument {
    const allocator = std.testing.allocator;
    var doc = try parseDocument(allocator, "key: value", .{});
    defer doc.deinit();

    const out = try stringifyDocument(allocator, &doc, .{});
    defer allocator.free(out);
    try std.testing.expectEqualStrings("key: value\n", out);
}
/// Serialize a standalone node tree into UTF-8 YAML bytes.
///
/// Returned bytes are allocated with `allocator`.
pub fn stringifyNode(
    allocator: std.mem.Allocator,
    node: *const Node,
    options: Options.Stringify,
) ![]u8 {
    return Serializer.stringifyNode(allocator, node, options);
}
