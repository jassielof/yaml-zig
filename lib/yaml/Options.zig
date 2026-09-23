//! Parse and stringify options.
pub const Options = @This();

/// Controls behavior when a mapping contains duplicate keys.
pub const DuplicateKeyBehavior = enum {
    /// Return a parse error when a duplicate key is encountered.
    reject,
    /// Keep only the last key/value pair and overwrite previous entries.
    keep_last,
};

/// Parse-time behavior knobs.
pub const Parse = struct {
    /// Duplicate key policy for mappings.
    duplicate_keys: DuplicateKeyBehavior = .reject,
    /// Resolve plain scalars according to YAML 1.2 Core schema.
    ///
    /// Quoted scalars remain strings regardless of this flag.
    resolve_core_schema: bool = true,
};

/// Stringify-time formatting options.
pub const Stringify = struct {
    /// Number of spaces used per indentation level.
    indent_width: u8 = 2,
    /// Emit `---` and `...` document markers.
    emit_document_markers: bool = false,
};
