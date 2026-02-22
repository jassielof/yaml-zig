//! Token model produced by the scanner.
const Span = @import("Span.zig");

pub const ScalarStyle = enum {
    plain,
    single_quoted,
    double_quoted,
    literal,
    folded,
};

pub const CollectionStyle = enum {
    block,
    flow,
};

pub const Kind = enum {
    stream_start,
    stream_end,
    document_start,
    document_end,
    indent,
    dedent,
    newline,
    dash,
    colon,
    comma,
    lbrace,
    rbrace,
    lbracket,
    rbracket,
    alias,
    scalar,
    eof,
};

pub const Token = @This();

kind: Kind,
span: Span = .{},
lexeme: []const u8 = "",
scalar_style: ScalarStyle = .plain,
indent: usize = 0,
