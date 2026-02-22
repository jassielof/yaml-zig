//! Error sets used by parser and serializer.
pub const Error = @This();

pub const Parse = error{
    UnexpectedToken,
    UnexpectedIndent,
    InvalidIndentation,
    InvalidMappingKey,
    UnterminatedString,
    UnterminatedFlowCollection,
    InvalidEscapeSequence,
    InvalidAlias,
    DuplicateKey,
    UnsupportedFeature,
};

pub const Serialize = error{
    InvalidState,
    InvalidScalar,
};
