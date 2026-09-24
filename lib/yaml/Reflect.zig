//! Reflection helpers for typed YAML (de)serialization.
const std = @import("std");
const Node = @import("Node.zig").Node;
const MapEntry = @import("Node.zig").MapEntry;

/// Convert a YAML node into a Zig value (`std.json.parseFromValue`-style).
pub fn fromNode(comptime T: type, allocator: std.mem.Allocator, node: Node) !T {
    return switch (@typeInfo(T)) {
        .bool => switch (node) {
            .bool => |v| v,
            else => error.UnexpectedToken,
        },
        .int => switch (node) {
            .int => |v| std.math.cast(T, v) orelse error.UnexpectedToken,
            else => error.UnexpectedToken,
        },
        .float => switch (node) {
            .float => |v| @floatCast(v),
            .int => |v| @floatFromInt(v),
            else => error.UnexpectedToken,
        },
        .optional => |info| switch (node) {
            .null => null,
            else => try fromNode(info.child, allocator, node),
        },
        .pointer => |info| blk: {
            if (info.size != .slice or info.child != u8) return error.UnexpectedToken;
            const s = switch (node) {
                .string => |v| v,
                else => return error.UnexpectedToken,
            };
            break :blk try allocator.dupe(u8, s);
        },
        .array => |info| blk: {
            const seq = switch (node) {
                .sequence => |s| s,
                else => return error.UnexpectedToken,
            };
            if (seq.items.len != info.len) return error.UnexpectedToken;
            var out: T = undefined;
            for (seq.items, 0..) |item, i| {
                out[i] = try fromNode(info.child, allocator, item);
            }
            break :blk out;
        },
        .@"struct" => |info| blk: {
            if (info.is_tuple) return error.UnexpectedToken;
            const map = switch (node) {
                .mapping => |m| m,
                else => return error.UnexpectedToken,
            };
            var out: T = undefined;
            inline for (info.fields) |field| {
                if (findMap(map.items, field.name)) |child| {
                    @field(out, field.name) = try fromNode(field.type, allocator, child.*);
                } else if (field.defaultValue()) |default| {
                    @field(out, field.name) = default;
                } else {
                    return error.UnexpectedToken;
                }
            }
            break :blk out;
        },
        .@"enum" => |info| blk: {
            const s = switch (node) {
                .string => |v| v,
                else => return error.UnexpectedToken,
            };
            inline for (info.fields) |field| {
                if (std.mem.eql(u8, field.name, s)) break :blk @field(T, field.name);
            }
            return error.UnexpectedToken;
        },
        else => error.UnexpectedToken,
    };
}

fn findMap(items: []const MapEntry, key: []const u8) ?*const Node {
    for (items) |*entry| {
        if (std.mem.eql(u8, entry.key, key)) return &entry.value;
    }
    return null;
}

/// Convert a Zig value into a heap-owned YAML node tree.
pub fn toNode(allocator: std.mem.Allocator, value: anytype) !Node {
    const T = @TypeOf(value);
    return switch (@typeInfo(T)) {
        .bool => .{ .bool = value },
        .int => .{ .int = @intCast(value) },
        .float => .{ .float = @floatCast(value) },
        .optional => if (value) |v| try toNode(allocator, v) else .null,
        .pointer => |info| blk: {
            if (info.size == .slice and info.child == u8) {
                break :blk .{ .string = try allocator.dupe(u8, value) };
            }
            if (info.size == .slice) {
                var seq: std.ArrayListUnmanaged(Node) = .empty;
                errdefer {
                    for (seq.items) |*item| item.deinit(allocator);
                    seq.deinit(allocator);
                }
                for (value) |item| try seq.append(allocator, try toNode(allocator, item));
                break :blk .{ .sequence = seq };
            }
            return error.UnexpectedToken;
        },
        .@"struct" => |info| blk: {
            var map: std.ArrayListUnmanaged(MapEntry) = .empty;
            errdefer {
                for (map.items) |*entry| {
                    allocator.free(entry.key);
                    entry.value.deinit(allocator);
                }
                map.deinit(allocator);
            }
            inline for (info.fields) |field| {
                try map.append(allocator, .{
                    .key = try allocator.dupe(u8, field.name),
                    .value = try toNode(allocator, @field(value, field.name)),
                });
            }
            break :blk .{ .mapping = map };
        },
        .@"enum" => .{ .string = try allocator.dupe(u8, @tagName(value)) },
        else => error.UnexpectedToken,
    };
}
