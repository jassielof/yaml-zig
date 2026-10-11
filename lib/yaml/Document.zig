//! YAML document container with arena-backed ownership.
const std = @import("std");
const Node = @import("Node.zig").Node;

pub const Document = @This();

/// Refcounted copy of the scanner input, shared by every document in a stream
/// so borrowed scalar slices stay valid without per-document full-file dupes.
pub const SharedPin = struct {
    allocator: std.mem.Allocator,
    bytes: []const u8,
    refs: usize,
    owns_bytes: bool,

    pub fn create(allocator: std.mem.Allocator, source: []const u8) !*SharedPin {
        const pin = try allocator.create(SharedPin);
        errdefer allocator.destroy(pin);
        const bytes = try allocator.dupe(u8, source);
        pin.* = .{
            .allocator = allocator,
            .bytes = bytes,
            .refs = 0,
            .owns_bytes = true,
        };
        return pin;
    }

    /// Pin `source` without copying; `source` must outlive all retains.
    pub fn createBorrowed(allocator: std.mem.Allocator, source: []const u8) !*SharedPin {
        const pin = try allocator.create(SharedPin);
        pin.* = .{
            .allocator = allocator,
            .bytes = source,
            .refs = 0,
            .owns_bytes = false,
        };
        return pin;
    }

    pub fn retain(self: *SharedPin) void {
        self.refs += 1;
    }

    pub fn release(self: *SharedPin) void {
        if (self.refs > 0) self.refs -= 1;
        if (self.refs == 0) {
            const allocator = self.allocator;
            if (self.owns_bytes) allocator.free(@constCast(self.bytes));
            allocator.destroy(self);
        }
    }
};

/// Arena for parser-built scalar buffers (block scalars, normalized quotes).
pub const OwnedScalarPool = struct {
    parent: std.mem.Allocator,
    arena: std.heap.ArenaAllocator,
    refs: usize,

    pub fn create(parent: std.mem.Allocator) !*OwnedScalarPool {
        const pool = try parent.create(OwnedScalarPool);
        pool.* = .{
            .parent = parent,
            .arena = std.heap.ArenaAllocator.init(parent),
            .refs = 1,
        };
        return pool;
    }

    pub fn retain(self: *OwnedScalarPool) void {
        self.refs += 1;
    }

    pub fn release(self: *OwnedScalarPool) void {
        if (self.refs == 0) return;
        self.refs -= 1;
        if (self.refs == 0) {
            const parent = self.parent;
            self.arena.deinit();
            parent.destroy(self);
        }
    }
};

allocator: std.mem.Allocator,
arena: std.heap.ArenaAllocator,
root: Node,
/// Optional shared input pin; string nodes may point into `shared_pin.bytes`.
shared_pin: ?*SharedPin = null,
/// Optional pool for parser-built scalar strings referenced by the tree.
scalar_pool: ?*OwnedScalarPool = null,

/// Initialize a document that owns `root` via `arena`.
///
/// All node/string memory must have been allocated from `arena.allocator()`,
/// except slices borrowed from `shared_pin` when it is non-null.
/// `deinit` releases everything in one shot (and drops one pin retain).
pub fn init(
    allocator: std.mem.Allocator,
    arena: std.heap.ArenaAllocator,
    root: Node,
    shared_pin: ?*SharedPin,
    scalar_pool: ?*OwnedScalarPool,
) Document {
    return .{
        .allocator = allocator,
        .arena = arena,
        .root = root,
        .shared_pin = shared_pin,
        .scalar_pool = scalar_pool,
    };
}

/// Release all memory owned by the document (O(1) arena teardown).
pub fn deinit(self: *Document) void {
    if (self.shared_pin) |pin| pin.release();
    if (self.scalar_pool) |pool| pool.release();
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
