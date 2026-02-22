//! Source position information.
pub const Mark = @This();

/// Zero-based byte offset in input.
offset: usize = 0,
/// Zero-based line.
line: usize = 0,
/// Zero-based column (byte-based).
column: usize = 0,
