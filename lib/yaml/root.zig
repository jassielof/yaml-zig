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
pub const Scanner = @import("Scanner.zig");
pub const Parser = @import("Parser.zig");
pub const Scalar = @import("Scalar.zig");
pub const Schema = @import("Schema.zig");
pub const Node = @import("Node.zig");
pub const Document = @import("Document.zig");
pub const Composer = @import("Composer.zig");
pub const Emitter = @import("Emitter.zig");
pub const Serializer = @import("Serializer.zig");
pub const Reflect = @import("Reflect.zig");
pub const CharClass = @import("CharClass.zig");

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
    if (docs.len == 0) {
        return Document.init(allocator, std.heap.ArenaAllocator.init(allocator), .null);
    }
    if (docs.len != 1) {
        for (docs) |*doc| doc.deinit();
        return Error.Parse.UnexpectedToken;
    }
    return docs[0];
}

test parseDocument {
    const allocator = std.testing.allocator;
    const source =
        \\language: zig
        \\versions:
        \\  - 0.15
        \\  - 0.16
        \\features:
        \\  parser: true
        \\  serializer: true
    ;

    var doc = try parseDocument(allocator, source, .{});
    defer doc.deinit();

    const map = switch (doc.root) {
        .mapping => |m| m,
        else => return error.TestUnexpectedResult,
    };
    const language = findMapValue(map.items, "language") orelse return error.TestUnexpectedResult;
    const versions = findMapValue(map.items, "versions") orelse return error.TestUnexpectedResult;
    const features = findMapValue(map.items, "features") orelse return error.TestUnexpectedResult;

    try std.testing.expectEqualStrings("zig", switch (language.*) {
        .string => |s| s,
        else => return error.TestUnexpectedResult,
    });
    try std.testing.expectEqual(@as(usize, 2), switch (versions.*) {
        .sequence => |seq| seq.items.len,
        else => return error.TestUnexpectedResult,
    });
    try std.testing.expectEqual(@as(usize, 2), switch (features.*) {
        .mapping => |nested| nested.items.len,
        else => return error.TestUnexpectedResult,
    });
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

/// Parse YAML into a flat event stream (SAX-style).
///
/// The caller owns the returned slice and must free it with
/// `Composer.freeEvents`. No `Document` tree is built.
pub fn parseEvents(
    allocator: std.mem.Allocator,
    source: []const u8,
    options: Options.Parse,
) ![]Event.Event {
    var scanner = Scanner.init(allocator, source, options);
    defer scanner.deinit();
    const scanned = try scanner.scan();

    var parser = Parser.init(allocator, scanned, options);
    defer parser.deinit();
    return parser.parse();
}

test parseEvents {
    const allocator = std.testing.allocator;
    const events = try parseEvents(allocator, "a: 1\n", .{});
    defer Composer.freeEvents(allocator, events);
    try std.testing.expect(events.len >= 4);
    try std.testing.expect(events[0].kind == .stream_start);
}

/// Parse YAML into a typed Zig value (`std.json.parseFromSlice`-style).
///
/// String fields are allocated with `allocator` and must be freed by the caller
/// (or live in an arena).
pub fn parseFromSlice(comptime T: type, allocator: std.mem.Allocator, source: []const u8, options: Options.Parse) !T {
    var doc = try parseDocument(allocator, source, options);
    defer doc.deinit();
    return Reflect.fromNode(T, allocator, doc.root);
}

/// Serialize a Zig value to YAML text.
pub fn stringifyFrom(allocator: std.mem.Allocator, value: anytype, options: Options.Stringify) ![]u8 {
    var node = try Reflect.toNode(allocator, value);
    defer node.deinit(allocator);
    return Serializer.stringifyNode(allocator, &node, options);
}

test parseStream {
    const allocator = std.testing.allocator;
    const docs = try parseStream(allocator,
        \\---
        \\first
        \\---
        \\second
    , .{});
    defer {
        for (docs) |*doc| doc.deinit();
        allocator.free(docs);
    }

    try std.testing.expectEqual(@as(usize, 2), docs.len);
    try std.testing.expectEqualStrings("first", switch (docs[0].root) {
        .string => |s| s,
        else => return error.TestUnexpectedResult,
    });
    try std.testing.expectEqualStrings("second", switch (docs[1].root) {
        .string => |s| s,
        else => return error.TestUnexpectedResult,
    });
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

test formatTestsuiteEvents {
    const allocator = std.testing.allocator;
    const events = try formatTestsuiteEvents(allocator, "hello");
    defer allocator.free(events);
    try std.testing.expectEqualStrings(
        \\+STR
        \\+DOC
        \\=VAL :hello
        \\-DOC
        \\-STR
        \\
    , events);
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
) !Node.Node {
    var document = try parseDocument(allocator, source, options);
    defer document.deinit();
    return try document.cloneRoot(allocator);
}

test parseNode {
    const allocator = std.testing.allocator;
    var node = try parseNode(allocator, "[1, 2, 3]", .{});
    defer node.deinit(allocator);
    try std.testing.expectEqual(@as(usize, 3), switch (node) {
        .sequence => |seq| seq.items.len,
        else => return error.TestUnexpectedResult,
    });
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
    const source =
        \\project:
        \\  name: weld
        \\  modules:
        \\    - yaml
        \\    - json
    ;

    var doc = try parseDocument(allocator, source, .{});
    defer doc.deinit();

    const rendered = try stringifyDocument(allocator, &doc, .{});
    defer allocator.free(rendered);

    var reparsed = try parseDocument(allocator, rendered, .{});
    defer reparsed.deinit();

    const root = switch (reparsed.root) {
        .mapping => |m| m,
        else => return error.TestUnexpectedResult,
    };
    const project = findMapValue(root.items, "project") orelse return error.TestUnexpectedResult;
    const modules = switch (project.*) {
        .mapping => |inner| findMapValue(inner.items, "modules") orelse return error.TestUnexpectedResult,
        else => return error.TestUnexpectedResult,
    };
    try std.testing.expect(switch (modules.*) {
        .sequence => true,
        else => false,
    });
}

/// Serialize a standalone node tree into UTF-8 YAML bytes.
///
/// Returned bytes are allocated with `allocator`.
pub fn stringifyNode(
    allocator: std.mem.Allocator,
    node: *const Node.Node,
    options: Options.Stringify,
) ![]u8 {
    return Serializer.stringifyNode(allocator, node, options);
}

test stringifyNode {
    const allocator = std.testing.allocator;
    var node = try parseNode(allocator, "key: value", .{});
    defer node.deinit(allocator);

    const out = try stringifyNode(allocator, &node, .{});
    defer allocator.free(out);
    try std.testing.expectEqualStrings("key: value\n", out);
}

test "core scalar resolution keeps quoted values as strings" {
    const allocator = std.testing.allocator;
    const source =
        \\plain_bool: true
        \\plain_null: null
        \\plain_int: 42
        \\quoted_hex: "0xFFEEBB"
        \\quoted_true: 'true'
    ;

    var doc = try parseDocument(allocator, source, .{});
    defer doc.deinit();

    const map = switch (doc.root) {
        .mapping => |m| m,
        else => return error.TestUnexpectedResult,
    };

    try std.testing.expect(switch ((findMapValue(map.items, "plain_bool") orelse return error.TestUnexpectedResult).*) {
        .bool => true,
        else => false,
    });
    try std.testing.expect(switch ((findMapValue(map.items, "plain_null") orelse return error.TestUnexpectedResult).*) {
        .null => true,
        else => false,
    });
    try std.testing.expectEqual(@as(i64, 42), switch ((findMapValue(map.items, "plain_int") orelse return error.TestUnexpectedResult).*) {
        .int => |v| v,
        else => return error.TestUnexpectedResult,
    });
    try std.testing.expectEqualStrings("0xFFEEBB", switch ((findMapValue(map.items, "quoted_hex") orelse return error.TestUnexpectedResult).*) {
        .string => |v| v,
        else => return error.TestUnexpectedResult,
    });
    try std.testing.expectEqualStrings("true", switch ((findMapValue(map.items, "quoted_true") orelse return error.TestUnexpectedResult).*) {
        .string => |v| v,
        else => return error.TestUnexpectedResult,
    });
}

fn findMapValue(map: []const Node.MapEntry, key: []const u8) ?*const Node.Node {
    for (map) |*entry| {
        if (std.mem.eql(u8, entry.key, key)) return &entry.value;
    }
    return null;
}

test "block scalar body may contain mapping-like text" {
    const a = std.testing.allocator;
    const src =
        \\a: |-
        \\  Valid values: "=" (equality), "!=" (inequality), "=~" (regex match), "!~" (regex non-match).
        \\b: x
    ;
    var doc = try parseDocument(a, src, .{});
    defer doc.deinit();
    try std.testing.expect(doc.root == .mapping);
    const desc = doc.root.mapping.items[0].value;
    try std.testing.expect(desc == .string);
    try std.testing.expect(std.mem.indexOf(u8, desc.string, "\"=\"") != null);
}

test "sequence item mapping folds multiline plain values" {
    const a = std.testing.allocator;
    const src =
        \\items:
        \\  - message: externalId can only be used when roleArn is
        \\      specified
        \\    rule: '!has(self.externalId) || has(self.roleArn)'
    ;
    var doc = try parseDocument(a, src, .{});
    defer doc.deinit();
    const item = doc.root.mapping.items[0].value.sequence.items[0];
    try std.testing.expect(item == .mapping);
    try std.testing.expectEqual(@as(usize, 2), item.mapping.items.len);
    try std.testing.expectEqualStrings(
        "externalId can only be used when roleArn is specified",
        item.mapping.items[0].value.string,
    );
    try std.testing.expectEqualStrings(
        "!has(self.externalId) || has(self.roleArn)",
        item.mapping.items[1].value.string,
    );
}

test "multiline single-quoted mapping value" {
    const a = std.testing.allocator;
    const src =
        \\path:
        \\  description: 'Required: Path is  the relative
        \\    path name of the file to be created. Must
        \\    not be absolute or contain the ''..''
        \\    path. Must be utf-8 encoded. The first
        \\    item of the relative path must not start
        \\    with ''..'''
        \\  type: string
    ;
    var doc = try parseDocument(a, src, .{});
    defer doc.deinit();
    const desc = doc.root.mapping.items[0].value.mapping.items[0].value.string;
    try std.testing.expectEqualStrings(
        "Required: Path is  the relative path name of the file to be created. Must not be absolute or contain the '..' path. Must be utf-8 encoded. The first item of the relative path must not start with '..'",
        desc,
    );
}

test "reject dangling quote after mapping value" {
    const a = std.testing.allocator;
    try std.testing.expectError(error.UnexpectedToken, parseDocument(a, "k: \"v\" trailing\n", .{}));
}

test "markdown table lines are plain scalars not block headers" {
    const a = std.testing.allocator;
    const src =
        \\desc: |
        \\  | zone1 | zone2 |
        \\  |  P P  |  P P  |
        \\next: ok
    ;
    var doc = try parseDocument(a, src, .{});
    defer doc.deinit();
    const text = doc.root.mapping.items[0].value.string;
    try std.testing.expect(std.mem.indexOf(u8, text, "| zone1 |") != null);
}

test "invalid block scalar indent indicators still rejected" {
    const a = std.testing.allocator;
    try std.testing.expectError(error.UnexpectedToken, parseDocument(a, "--- |0\n", .{}));
    try std.testing.expectError(error.UnexpectedToken, parseDocument(a, "--- |10\n", .{}));
}

test "document path accessor" {
    const a = std.testing.allocator;
    var doc = try parseDocument(a,
        \\metadata:
        \\  name: demo
        \\items:
        \\  - id: 1
    , .{});
    defer doc.deinit();
    const name = doc.get("metadata.name") orelse return error.TestUnexpectedResult;
    try std.testing.expectEqualStrings("demo", name.string);
    const id = doc.get("items.0.id") orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(@as(i64, 1), id.int);
}

test "parseFromSlice typed struct" {
    const a = std.testing.allocator;
    const Config = struct {
        name: []const u8,
        count: i64,
        enabled: bool = true,
    };
    const cfg = try parseFromSlice(Config, a,
        \\name: zig
        \\count: 3
    , .{});
    defer a.free(cfg.name);
    try std.testing.expectEqualStrings("zig", cfg.name);
    try std.testing.expectEqual(@as(i64, 3), cfg.count);
    try std.testing.expect(cfg.enabled);
}

test "round-trip parse emit parse" {
    const a = std.testing.allocator;
    const src =
        \\a: 1
        \\b:
        \\  - x
        \\  - y
    ;
    var doc1 = try parseDocument(a, src, .{});
    defer doc1.deinit();
    const emitted = try Serializer.stringifyDocument(a, &doc1, .{});
    defer a.free(emitted);
    var doc2 = try parseDocument(a, emitted, .{});
    defer doc2.deinit();
    try std.testing.expectEqual(@as(usize, 2), doc2.root.mapping.items.len);
}

