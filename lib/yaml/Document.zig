//! YAML document container with arena-backed ownership.
const std = @import("std");
const Node = @import("Node.zig").Node;

pub const Document = @This();

/// Refcounted copy of the scanner input, shared by every document in a stream
/// so borrowed scalar slices stay valid without per-document full-file dupes.
pub const SharedPin = struct {
    allocator: std.mem.Allocator,
    /// Logical input bytes (same length as the scanned source when owned).
    bytes: []const u8,
    /// Owned allocation including compaction slack; null when borrowed.
    backing: ?[]u8 = null,
    refs: usize,
    owns_bytes: bool,

    pub fn create(allocator: std.mem.Allocator, source: []const u8) !*SharedPin {
        const pin = try allocator.create(SharedPin);
        errdefer allocator.destroy(pin);
        // Extra tail room for in-place literal block compaction (strip chomp may add then remove `\n`).
        const slack = @max(@as(usize, 4096), source.len / 16);
        const backing = try allocator.alloc(u8, source.len + slack);
        @memcpy(backing[0..source.len], source);
        pin.* = .{
            .allocator = allocator,
            .bytes = backing[0..source.len],
            .backing = backing,
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
            .backing = null,
            .refs = 0,
            .owns_bytes = false,
        };
        return pin;
    }

    pub fn mutableBacking(self: *SharedPin) ?[]u8 {
        if (!self.owns_bytes) return null;
        return self.backing;
    }

    pub fn retain(self: *SharedPin) void {
        self.refs += 1;
    }

    pub fn release(self: *SharedPin) void {
        if (self.refs > 0) self.refs -= 1;
        if (self.refs == 0) {
            const allocator = self.allocator;
            if (self.backing) |b| allocator.free(b);
            allocator.destroy(self);
        }
    }
};

/// Arena for parser-built scalar buffers (block scalars, normalized quotes).
pub const OwnedScalarPool = struct {
    parent: std.mem.Allocator,
    arena: std.heap.ArenaAllocator,
    /// Bump buffer for block/literal scalars (one growable slab per parse).
    scratch: std.ArrayListUnmanaged(u8) = .empty,
    refs: usize,

    pub fn create(parent: std.mem.Allocator) !*OwnedScalarPool {
        const pool = try parent.create(OwnedScalarPool);
        pool.* = .{
            .parent = parent,
            .arena = std.heap.ArenaAllocator.init(parent),
            .scratch = .empty,
            .refs = 1,
        };
        return pool;
    }

    pub fn startScalar(self: *OwnedScalarPool) usize {
        return self.scratch.items.len;
    }

    pub fn truncateScalar(self: *OwnedScalarPool, start: usize) void {
        self.scratch.shrinkRetainingCapacity(start);
    }

    pub fn scalarSlice(self: *OwnedScalarPool, start: usize) []const u8 {
        return self.scratch.items[start..];
    }

    pub fn retain(self: *OwnedScalarPool) void {
        self.refs += 1;
    }

    pub fn scratchAlloc(self: *OwnedScalarPool) std.mem.Allocator {
        return self.arena.allocator();
    }

    pub fn release(self: *OwnedScalarPool) void {
        if (self.refs == 0) return;
        self.refs -= 1;
        if (self.refs == 0) {
            const parent = self.parent;
            self.scratch.deinit(self.arena.allocator());
            self.arena.deinit();
            parent.destroy(self);
        }
    }
};

/// Node-tree memory: one GPA slab + bump arena for large inputs, plain arena otherwise.
pub const DocumentArena = struct {
    parent: std.mem.Allocator,
    slab: ?[]u8 = null,
    bump: std.heap.FixedBufferAllocator,
    arena: std.heap.ArenaAllocator,

    pub fn init(parent: std.mem.Allocator, hint_bytes: usize) !DocumentArena {
        _ = hint_bytes;
        return initDirect(parent);
    }

    pub fn initDirect(parent: std.mem.Allocator) DocumentArena {
        return .{
            .parent = parent,
            .slab = null,
            .bump = undefined,
            .arena = std.heap.ArenaAllocator.init(parent),
        };
    }

    pub fn deinit(self: *DocumentArena) void {
        _ = self.arena.deinit();
        if (self.slab) |s| {
            self.parent.free(s);
            self.slab = null;
        }
    }

    pub fn allocator(self: *DocumentArena) std.mem.Allocator {
        return self.arena.allocator();
    }
};

allocator: std.mem.Allocator,
storage: DocumentArena,
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
    storage: DocumentArena,
    root: Node,
    shared_pin: ?*SharedPin,
    scalar_pool: ?*OwnedScalarPool,
) Document {
    return .{
        .allocator = allocator,
        .storage = storage,
        .root = root,
        .shared_pin = shared_pin,
        .scalar_pool = scalar_pool,
    };
}

/// Release all memory owned by the document (O(1) arena teardown).
pub fn deinit(self: *Document) void {
    if (self.shared_pin) |pin| pin.release();
    if (self.scalar_pool) |pool| pool.release();
    self.storage.deinit();
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
