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
    anchor: ?[]const u8 = null,
    /// Tag text inside the test-suite brackets, such as `tag:yaml.org,2002:str`.
    tag: ?[]const u8 = null,
    span: Span = .{},
};

pub const MappingStart = struct {
    style: Token.CollectionStyle = .block,
    anchor: ?[]const u8 = null,
    tag: ?[]const u8 = null,
    span: Span = .{},
};

pub const Scalar = struct {
    value: []const u8,
    style: Token.ScalarStyle = .plain,
    anchor: ?[]const u8 = null,
    tag: ?[]const u8 = null,
    span: Span = .{},
    /// When true, `value` is heap memory owned by the event and must be freed
    /// (or transferred) by the consumer. When false, `value` borrows the parse source.
    value_owned: bool = false,
};

pub const DocumentStart = struct {
    span: Span = .{},
    /// The document was introduced by an explicit `---` marker.
    explicit: bool = false,
};

pub const Alias = struct {
    name: []const u8,
    span: Span = .{},
};

kind: Kind,
data: union(Kind) {
    stream_start: Span,
    stream_end: Span,
    document_start: DocumentStart,
    document_end: Span,
    sequence_start: SequenceStart,
    sequence_end: Span,
    mapping_start: MappingStart,
    mapping_end: Span,
    scalar: Scalar,
    alias: Alias,
},
