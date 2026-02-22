//! YAML document container with arena-backed ownership.
const std = @import("std");
const Node = @import("Node.zig").Node;

pub const Document = @This();

allocator: std.mem.Allocator,
arena: std.heap.ArenaAllocator,
root: Node,

/// Initialize a document with an already-owned root node.
///
/// Ownership of `root` is transferred to the document.
pub fn init(allocator: std.mem.Allocator, root: Node) Document {
    return .{
        .allocator = allocator,
        .arena = std.heap.ArenaAllocator.init(allocator),
        .root = root,
    };
}

/// Release all memory owned by the document.
pub fn deinit(self: *Document) void {
    self.root.deinit(self.allocator);
    self.arena.deinit();
}

/// Deep-clone the root node using an external allocator.
pub fn cloneRoot(self: *const Document, allocator: std.mem.Allocator) !Node {
    return self.root.clone(allocator);
}
