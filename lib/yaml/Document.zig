//! YAML document container with arena-backed ownership.
const std = @import("std");
const Node = @import("Node.zig").Node;

pub const Document = @This();

allocator: std.mem.Allocator,
arena: std.heap.ArenaAllocator,
root: Node,

/// Initialize a document that owns `root` via `arena`.
///
/// All node/string memory must have been allocated from `arena.allocator()`.
/// `deinit` releases everything in one shot.
pub fn init(allocator: std.mem.Allocator, arena: std.heap.ArenaAllocator, root: Node) Document {
    return .{
        .allocator = allocator,
        .arena = arena,
        .root = root,
    };
}

/// Release all memory owned by the document (O(1) arena teardown).
pub fn deinit(self: *Document) void {
    self.arena.deinit();
    self.* = undefined;
}

/// Deep-clone the root node using an external allocator.
pub fn cloneRoot(self: *const Document, allocator: std.mem.Allocator) !Node {
    return self.root.clone(allocator);
}

/// Look up a dotted path such as `metadata.name` or `items.0.id`.
///
/// Sequence indices are decimal. Returns null when any segment is missing
/// or the path does not resolve to a node.
pub fn get(self: *const Document, path: []const u8) ?*const Node {
    return getPath(&self.root, path);
}

pub fn getPath(root: *const Node, path: []const u8) ?*const Node {
    var current: *const Node = root;
    var rest = path;
    while (rest.len > 0) {
        const dot = std.mem.indexOfScalar(u8, rest, '.') orelse rest.len;
        const seg = rest[0..dot];
        rest = if (dot < rest.len) rest[dot + 1 ..] else "";
        if (seg.len == 0) return null;

        current = switch (current.*) {
            .mapping => |*map| blk: {
                for (map.items) |*entry| {
                    if (std.mem.eql(u8, entry.key, seg)) break :blk &entry.value;
                }
                return null;
            },
            .sequence => |*seq| blk: {
                const idx = std.fmt.parseInt(usize, seg, 10) catch return null;
                if (idx >= seq.items.len) return null;
                break :blk &seq.items[idx];
            },
            else => return null,
        };
    }
    return current;
}
