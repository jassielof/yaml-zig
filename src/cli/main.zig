const std = @import("std");
const metadata = @import("../../build.zig.zon");
const fangz = @import("fangz");

pub fn main() !void {
    const version = metadata.version;
    std.debug.print("yaml-zig version: {s}\n", .{version});
    std.debug.print("Hola\n", .{});
}
