//! Compile-time byte classification tables for the scanner hot path.
const std = @import("std");

/// Characters that end an unquoted plain scalar in block context (approx).
pub const plain_stop: [256]bool = blk: {
    var t: [256]bool = [_]bool{false} ** 256;
    t['\n'] = true;
    t['\r'] = true;
    t['#'] = true;
    t[':'] = true;
    t[','] = true;
    t['['] = true;
    t[']'] = true;
    t['{'] = true;
    t['}'] = true;
    break :blk t;
};

pub inline fn isPlainStop(c: u8) bool {
    return plain_stop[c];
}

/// Count leading ASCII spaces with a word-at-a-time fast path.
pub fn countLeadingSpaces(line: []const u8) usize {
    var i: usize = 0;
    // SWAR: process usize-width chunks of spaces (0x20).
    const usize_bytes = @sizeOf(usize);
    const splat: usize = std.math.maxInt(usize) / 255 * ' ';
    while (i + usize_bytes <= line.len) {
        const word = std.mem.readInt(usize, line[i..][0..usize_bytes], .little);
        if (word != splat) break;
        i += usize_bytes;
    }
    while (i < line.len and line[i] == ' ') : (i += 1) {}
    return i;
}
