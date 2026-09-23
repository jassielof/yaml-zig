//! Source span information.
const Mark = @import("Mark.zig");

pub const Span = @This();

start: Mark = .{},
end: Mark = .{},
