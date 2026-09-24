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
    return resolveScalarOwned(allocator, value, false, style, resolve_core_schema);
}

/// Like `resolveScalar`, but when `value_owned` is true and the result is a
/// string, ownership of `value` is transferred to the node (no extra copy).
/// Non-string results free `value` when it was owned.
pub fn resolveScalarOwned(
    allocator: std.mem.Allocator,
    value: []const u8,
    value_owned: bool,
    style: Token.ScalarStyle,
    resolve_core_schema: bool,
) !Node {
    const finish_string = struct {
        fn go(alloc: std.mem.Allocator, v: []const u8, owned: bool) !Node {
            if (owned) return .{ .string = @constCast(v) };
            return .{ .string = try alloc.dupe(u8, v) };
        }
    }.go;

    const drop_owned = struct {
        fn go(alloc: std.mem.Allocator, v: []const u8, owned: bool) void {
            if (owned) alloc.free(v);
        }
    }.go;

    if (!resolve_core_schema or style != .plain) {
        return finish_string(allocator, value, value_owned);
    }

    if (isNull(value)) {
        drop_owned(allocator, value, value_owned);
        return .null;
    }
    if (isBool(value)) |b| {
        drop_owned(allocator, value, value_owned);
        return .{ .bool = b };
    }

    if (couldBeNumber(value)) {
        if (parseInt(allocator, value)) |v| {
            drop_owned(allocator, value, value_owned);
            return .{ .int = v };
        } else |_| {}
        if (parseFloat(allocator, value)) |v| {
            drop_owned(allocator, value, value_owned);
            return .{ .float = v };
        } else |_| {}
    }

    return finish_string(allocator, value, value_owned);
}

/// Cheap gate before int/float parsing. Underscores and hex/oct prefixes are handled inside.
fn couldBeNumber(value: []const u8) bool {
    const text = std.mem.trim(u8, value, " ");
    if (text.len == 0) return false;
    const c = text[0];
    return (c >= '0' and c <= '9') or c == '-' or c == '+' or c == '.';
}

fn isNull(value: []const u8) bool {
    if (value.len == 0) return true;
    return std.ascii.eqlIgnoreCase(value, "null") or
        std.mem.eql(u8, value, "~");
}

fn isBool(value: []const u8) ?bool {
    if (std.ascii.eqlIgnoreCase(value, "true")) return true;
    if (std.ascii.eqlIgnoreCase(value, "false")) return false;
    return null;
}

fn parseInt(allocator: std.mem.Allocator, value: []const u8) !i64 {
    var text = std.mem.trim(u8, value, " ");
    if (text.len == 0) return error.InvalidCharacter;

    var sign: i64 = 1;
    if (text[0] == '+' or text[0] == '-') {
        if (text[0] == '-') sign = -1;
        text = text[1..];
    }
    if (text.len == 0) return error.InvalidCharacter;

    var base: u8 = 10;
    if (text.len >= 2 and text[0] == '0') {
        if (text[1] == 'x' or text[1] == 'X') {
            base = 16;
            text = text[2..];
        } else if (text[1] == 'o' or text[1] == 'O') {
            base = 8;
            text = text[2..];
        }
    }

    const normalized = try removeUnderscores(allocator, text);
    defer allocator.free(normalized);
    if (normalized.len == 0) return error.InvalidCharacter;

    const magnitude = try std.fmt.parseInt(i64, normalized, base);
    if (sign < 0) return -magnitude;
    return magnitude;
}

fn parseFloat(allocator: std.mem.Allocator, value: []const u8) !f64 {
    const text = std.mem.trim(u8, value, " ");
    if (std.ascii.eqlIgnoreCase(text, ".inf") or std.ascii.eqlIgnoreCase(text, "+.inf")) return std.math.inf(f64);
    if (std.ascii.eqlIgnoreCase(text, "-.inf")) return -std.math.inf(f64);
    if (std.ascii.eqlIgnoreCase(text, ".nan")) return std.math.nan(f64);

    const normalized = try removeUnderscores(allocator, text);
    defer allocator.free(normalized);
    return std.fmt.parseFloat(f64, normalized);
}

fn removeUnderscores(allocator: std.mem.Allocator, value: []const u8) ![]u8 {
    var final_len: usize = 0;
    for (value) |ch| {
        if (ch != '_') final_len += 1;
    }
    var out = try allocator.alloc(u8, final_len);
    errdefer allocator.free(out);

    var j: usize = 0;
    for (value) |ch| {
        if (ch == '_') continue;
        out[j] = ch;
        j += 1;
    }
    return out;
}
