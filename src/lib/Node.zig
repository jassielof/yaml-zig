//! YAML node tree model.
const std = @import("std");

pub const MapEntry = struct {
    /// Mapping key, always owned by the node tree.
    key: []u8,
    /// Mapping value node.
    value: Node,
};

pub const Node = union(enum) {
    null,
    bool: bool,
    int: i64,
    float: f64,
    string: []u8,
    sequence: std.ArrayListUnmanaged(Node),
    mapping: std.ArrayListUnmanaged(MapEntry),

    /// Deep clone a node and all descendants.
    pub fn clone(self: Node, allocator: std.mem.Allocator) !Node {
        return switch (self) {
            .null => .null,
            .bool => |v| .{ .bool = v },
            .int => |v| .{ .int = v },
            .float => |v| .{ .float = v },
            .string => |v| .{ .string = try allocator.dupe(u8, v) },
            .sequence => |seq| blk: {
                var out: std.ArrayListUnmanaged(Node) = .{};
                errdefer {
                    for (out.items) |*item| item.deinit(allocator);
                    out.deinit(allocator);
                }
                try out.ensureTotalCapacity(allocator, seq.items.len);
                for (seq.items) |item| {
                    try out.append(allocator, try item.clone(allocator));
                }
                break :blk .{ .sequence = out };
            },
            .mapping => |map| blk: {
                var out: std.ArrayListUnmanaged(MapEntry) = .{};
                errdefer {
                    for (out.items) |*entry| {
                        allocator.free(entry.key);
                        entry.value.deinit(allocator);
                    }
                    out.deinit(allocator);
                }
                try out.ensureTotalCapacity(allocator, map.items.len);
                for (map.items) |entry| {
                    try out.append(allocator, .{
                        .key = try allocator.dupe(u8, entry.key),
                        .value = try entry.value.clone(allocator),
                    });
                }
                break :blk .{ .mapping = out };
            },
        };
    }

    /// Recursively free all memory owned by this node.
    ///
    /// After deinit, the node becomes `.null`.
    pub fn deinit(self: *Node, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .string => |bytes| allocator.free(bytes),
            .sequence => |*seq| {
                for (seq.items) |*item| item.deinit(allocator);
                seq.deinit(allocator);
            },
            .mapping => |*map| {
                for (map.items) |*entry| {
                    allocator.free(entry.key);
                    entry.value.deinit(allocator);
                }
                map.deinit(allocator);
            },
            else => {},
        }
        self.* = .null;
    }

    /// Returns true when the node is a scalar value.
    pub fn isScalar(self: Node) bool {
        return switch (self) {
            .null, .bool, .int, .float, .string => true,
            else => false,
        };
    }
};
