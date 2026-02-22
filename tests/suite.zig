const std = @import("std");
const testing = std.testing;

comptime {
    _ = @import("basic.zig");
    _ = @import("spec.zig");
}

test {
    testing.refAllDecls(@This());
}
