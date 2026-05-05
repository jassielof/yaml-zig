//! YAML.
const std = @import("std");
const refAllDecls = std.testing.refAllDecls;

pub const fy = @import("fy");
pub const yaml = @import("yaml");

comptime {
    refAllDecls(@This());
}
