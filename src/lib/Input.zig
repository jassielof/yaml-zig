//! UTF-8 source wrapper used by scanner/parser.
const Mark = @import("Mark.zig");

pub const Input = @This();

bytes: []const u8,
index: usize = 0,
line: usize = 0,
column: usize = 0,

pub fn init(bytes: []const u8) Input {
    return .{ .bytes = bytes };
}

pub fn peek(self: *const Input) ?u8 {
    if (self.index >= self.bytes.len) return null;
    return self.bytes[self.index];
}

pub fn advance(self: *Input) ?u8 {
    const ch = self.peek() orelse return null;
    self.index += 1;
    if (ch == '\n') {
        self.line += 1;
        self.column = 0;
    } else {
        self.column += 1;
    }
    return ch;
}

pub fn mark(self: *const Input) Mark {
    return .{
        .offset = self.index,
        .line = self.line,
        .column = self.column,
    };
}
