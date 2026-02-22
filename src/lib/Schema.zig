//! YAML 1.2 Core scalar resolution helpers.
const std = @import("std");
const Token = @import("Token.zig");
const Node = @import("Node.zig").Node;

pub const Schema = @This();

/// Resolve a parsed scalar into a YAML node.
///
/// For Core schema resolution, only plain scalars are type-resolved. Quoted
/// scalars are always emitted as strings to avoid accidental numeric/bool casts.
pub fn resolveScalar(
    allocator: std.mem.Allocator,
    value: []const u8,
    style: Token.ScalarStyle,
    resolve_core_schema: bool,
) !Node {
    if (!resolve_core_schema or style != .plain) {
        return .{ .string = try allocator.dupe(u8, value) };
    }

    if (isNull(value)) return .null;
    if (isBool(value)) |b| return .{ .bool = b };
    if (parseInt(value)) |v| return .{ .int = v } else |_| {}
    if (parseFloat(value)) |v| return .{ .float = v } else |_| {}

    return .{ .string = try allocator.dupe(u8, value) };
}

fn isNull(value: []const u8) bool {
    return std.ascii.eqlIgnoreCase(value, "null") or
        std.mem.eql(u8, value, "~");
}

fn isBool(value: []const u8) ?bool {
    if (std.ascii.eqlIgnoreCase(value, "true")) return true;
    if (std.ascii.eqlIgnoreCase(value, "false")) return false;
    return null;
}

fn parseInt(value: []const u8) !i64 {
    return std.fmt.parseInt(i64, value, 10);
}

fn parseFloat(value: []const u8) !f64 {
    return std.fmt.parseFloat(f64, value);
}
