//! Parser event stream model.
const Span = @import("Span.zig");
const Token = @import("Token.zig");

pub const Event = @This();

pub const Kind = enum {
    stream_start,
    stream_end,
    document_start,
    document_end,
    sequence_start,
    sequence_end,
    mapping_start,
    mapping_end,
    scalar,
    alias,
};

pub const SequenceStart = struct {
    style: Token.CollectionStyle = .block,
    span: Span = .{},
};

pub const MappingStart = struct {
    style: Token.CollectionStyle = .block,
    span: Span = .{},
};

pub const Scalar = struct {
    value: []const u8,
    style: Token.ScalarStyle = .plain,
    span: Span = .{},
};

pub const Alias = struct {
    name: []const u8,
    span: Span = .{},
};

kind: Kind,
data: union(Kind) {
    stream_start: Span,
    stream_end: Span,
    document_start: Span,
    document_end: Span,
    sequence_start: SequenceStart,
    sequence_end: Span,
    mapping_start: MappingStart,
    mapping_end: Span,
    scalar: Scalar,
    alias: Alias,
},
