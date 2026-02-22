//! High-level serialization API.
const std = @import("std");
const Document = @import("Document.zig");
const Node = @import("Node.zig").Node;
const Options = @import("Options.zig");
const Emitter = @import("Emitter.zig");

pub const Serializer = @This();

/// Serialize a parsed document to YAML text.
pub fn stringifyDocument(
    allocator: std.mem.Allocator,
    document: *const Document,
    options: Options.Stringify,
) ![]u8 {
    var emitter = Emitter.init(allocator, options);
    defer emitter.deinit();
    return emitter.emitDocument(&document.root);
}

/// Serialize a standalone node to YAML text.
pub fn stringifyNode(
    allocator: std.mem.Allocator,
    node: *const Node,
    options: Options.Stringify,
) ![]u8 {
    var emitter = Emitter.init(allocator, options);
    defer emitter.deinit();
    return emitter.emitDocument(node);
}
