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
    var scanner = Scanner.init(allocator, source, options);
    defer scanner.deinit();
    const scanned = try scanner.scan();

    var parser = Parser.init(allocator, scanned, options);
    defer parser.deinit();
    const events = try parser.parse();

    return Composer.compose(allocator, events, options);
}

test parseDocument {
    const allocator = std.testing.allocator;
    var doc = try parseDocument(allocator, "name: Alice\nage: 30", .{});
    defer doc.deinit();
    // verify the root is a mapping
    try std.testing.expect(doc.root.tag == .mapping);
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
