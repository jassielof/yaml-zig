//! Scalar metadata used for schema resolution.
const Token = @import("Token.zig");

pub const Scalar = @This();

text: []const u8,
style: Token.ScalarStyle = .plain,
tag: ?[]const u8 = null,
