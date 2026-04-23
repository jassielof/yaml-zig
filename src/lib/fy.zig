//! Zig wrapper around the vendored libfyaml C library.
const std = @import("std");
const builtin = @import("builtin");

pub const c = if (builtin.os.tag == .windows) blk: {
    // Stub for Windows
    break :blk struct {
        pub const struct_fy_parser = opaque {};
        pub const struct_fy_event = opaque {};
        pub const struct_fy_diag = opaque {};
        pub const struct_fy_document = opaque {};
        pub const struct_fy_node = opaque {};
        pub const struct_fy_token = opaque {};
    };
} else blk: {
    break :blk @cImport({
        @cInclude("config.h");
        @cInclude("stdlib.h");
        @cInclude("libfyaml.h");
    });
};

pub const Error = error{
    ParseFailed,
    EmitFailed,
    EventFormatFailed,
    OutOfMemory,
    UnsupportedOnWindows,
};

pub const TestsuiteParseResult = struct {
    events: []u8,
    had_stream_error: bool,
};

pub const Document = struct {
    raw: if (builtin.os.tag == .windows) void else *c.struct_fy_document,

    pub fn deinit(self: *Document) void {
        if (builtin.os.tag != .windows) {
            c.fy_document_destroy(self.raw);
        }
    }

    pub fn root(self: *Document) ?*c.struct_fy_node {
        if (builtin.os.tag == .windows) return null;
        return c.fy_document_root(self.raw);
    }

    pub fn emitJsonAlloc(self: *Document, allocator: std.mem.Allocator) Error![]u8 {
        if (builtin.os.tag == .windows) return error.UnsupportedOnWindows;
        const emitted = c.fy_emit_document_to_string(self.raw, c.FYECF_MODE_JSON) orelse {
            return error.EmitFailed;
        };
        defer c.free(emitted);

        return allocator.dupe(u8, std.mem.span(@as([*:0]u8, @ptrCast(emitted))));
    }
};

pub fn parseDocument(source: []const u8) Error!Document {
    if (builtin.os.tag == .windows) return error.UnsupportedOnWindows;

    var cfg = std.mem.zeroes(c.struct_fy_parse_cfg);
    cfg.flags = @as(@TypeOf(cfg.flags), c.FYPCF_QUIET | c.FYPCF_RESOLVE_DOCUMENT);
    const diag = fyz_create_silent_diag();
    defer if (diag) |resolved| c.fy_diag_destroy(resolved);
    cfg.diag = diag;

    const raw = c.fy_document_build_from_string(&cfg, @ptrCast(source.ptr), source.len) orelse {
        return error.ParseFailed;
    };

    return .{ .raw = raw };
}

pub fn parseTestsuiteEventsAlloc(allocator: std.mem.Allocator, source: []const u8) Error![]u8 {
    if (builtin.os.tag == .windows) return error.UnsupportedOnWindows;
    const result = try parseTestsuiteEventsDetailedAlloc(allocator, source);
    return result.events;
}

pub fn parseTestsuiteEventsDetailedAlloc(allocator: std.mem.Allocator, source: []const u8) Error!TestsuiteParseResult {
    if (builtin.os.tag == .windows) return error.UnsupportedOnWindows;

    var cfg = std.mem.zeroes(c.struct_fy_parse_cfg);
    cfg.flags = @as(@TypeOf(cfg.flags), c.FYPCF_QUIET);
    const diag = fyz_create_silent_diag();
    defer if (diag) |resolved| c.fy_diag_destroy(resolved);
    cfg.diag = diag;

    const parser = c.fy_parser_create(&cfg) orelse return error.ParseFailed;
    defer c.fy_parser_destroy(parser);

    if (c.fy_parser_set_string(parser, @ptrCast(source.ptr), source.len) != 0) {
        return error.ParseFailed;
    }

    var output: std.ArrayListUnmanaged(u8) = .empty;
    errdefer output.deinit(allocator);

    while (true) {
        const eventp = fy_parse_private(parser);
        if (eventp == null) break;
        defer fy_parse_eventp_recycle(parser, eventp.?);

        try appendTestsuiteEvent(allocator, &output, &eventp.?.e);
    }

    return .{
        .events = try output.toOwnedSlice(allocator),
        .had_stream_error = c.fy_parser_get_stream_error(parser),
    };
}

// C binding types (real on non-Windows, stubs on Windows)
const fy_list_head_type = if (builtin.os.tag == .windows)
    opaque {}
else
    extern struct {
        next: ?*@This(),
        prev: ?*@This(),
    };

const fy_eventp_type = if (builtin.os.tag == .windows)
    opaque {}
else
    extern struct {
        node: fy_list_head_type,
        e: c.struct_fy_event,
    };

// C extern declarations
extern "c" fn fy_parse_private(fyp: *c.struct_fy_parser) ?*fy_eventp_type;
extern "c" fn fy_parse_eventp_recycle(fyp: *c.struct_fy_parser, fyep: *fy_eventp_type) void;
extern "c" fn fyz_create_silent_diag() ?*c.struct_fy_diag;

fn appendTestsuiteEvent(
    allocator: std.mem.Allocator,
    output: *std.ArrayListUnmanaged(u8),
    event: *c.struct_fy_event,
) Error!void {
    switch (event.type) {
        c.FYET_STREAM_START => try output.appendSlice(allocator, "+STR\n"),
        c.FYET_STREAM_END => try output.appendSlice(allocator, "-STR\n"),
        c.FYET_DOCUMENT_START => {
            try output.appendSlice(allocator, "+DOC");
            if (!c.fy_document_event_is_implicit(event)) try output.appendSlice(allocator, " ---");
            try output.append(allocator, '\n');
        },
        c.FYET_DOCUMENT_END => {
            try output.appendSlice(allocator, "-DOC");
            if (!c.fy_document_event_is_implicit(event)) try output.appendSlice(allocator, " ...");
            try output.append(allocator, '\n');
        },
        c.FYET_MAPPING_START => {
            try output.appendSlice(allocator, "+MAP");
            if (c.fy_event_get_node_style(event) == c.FYNS_FLOW) try output.appendSlice(allocator, " {}");
            try appendAnchorAndTag(allocator, output, c.fy_event_get_anchor_token(event), c.fy_event_get_tag_token(event));
            try output.append(allocator, '\n');
        },
        c.FYET_MAPPING_END => try output.appendSlice(allocator, "-MAP\n"),
        c.FYET_SEQUENCE_START => {
            try output.appendSlice(allocator, "+SEQ");
            if (c.fy_event_get_node_style(event) == c.FYNS_FLOW) try output.appendSlice(allocator, " []");
            try appendAnchorAndTag(allocator, output, c.fy_event_get_anchor_token(event), c.fy_event_get_tag_token(event));
            try output.append(allocator, '\n');
        },
        c.FYET_SEQUENCE_END => try output.appendSlice(allocator, "-SEQ\n"),
        c.FYET_SCALAR => {
            try output.appendSlice(allocator, "=VAL");
            try appendAnchorAndTag(allocator, output, c.fy_event_get_anchor_token(event), c.fy_event_get_tag_token(event));

            const value_token = c.fy_event_get_token(event) orelse return error.EventFormatFailed;
            try appendScalarStylePrefix(allocator, output, value_token);
            try appendEscapedTokenText(allocator, output, value_token);
            try output.append(allocator, '\n');
        },
        c.FYET_ALIAS => {
            try output.appendSlice(allocator, "=ALI *");
            try appendTokenText(allocator, output, c.fy_event_get_token(event));
            try output.append(allocator, '\n');
        },
        else => return error.EventFormatFailed,
    }
}

fn appendAnchorAndTag(
    allocator: std.mem.Allocator,
    output: *std.ArrayListUnmanaged(u8),
    anchor: ?*c.struct_fy_token,
    tag: ?*c.struct_fy_token,
) Error!void {
    if (anchor) |token| {
        try output.appendSlice(allocator, " &");
        try appendTokenText(allocator, output, token);
    }
    if (tag) |token| {
        try output.appendSlice(allocator, " <");
        try appendTokenText(allocator, output, token);
        try output.append(allocator, '>');
    }
}

fn appendScalarStylePrefix(
    allocator: std.mem.Allocator,
    output: *std.ArrayListUnmanaged(u8),
    value_token: *c.struct_fy_token,
) Error!void {
    switch (c.fy_token_scalar_style(value_token)) {
        c.FYSS_PLAIN => try output.appendSlice(allocator, " :"),
        c.FYSS_SINGLE_QUOTED => try output.appendSlice(allocator, " '"),
        c.FYSS_DOUBLE_QUOTED => try output.appendSlice(allocator, " \""),
        c.FYSS_LITERAL => try output.appendSlice(allocator, " |"),
        c.FYSS_FOLDED => try output.appendSlice(allocator, " >"),
        else => return error.EventFormatFailed,
    }
}

fn appendEscapedTokenText(
    allocator: std.mem.Allocator,
    output: *std.ArrayListUnmanaged(u8),
    token: *c.struct_fy_token,
) Error!void {
    var text_len: usize = 0;
    const text_ptr = c.fy_token_get_text(token, &text_len) orelse return error.EventFormatFailed;
    try appendEscapedText(allocator, output, @as([*]const u8, @ptrCast(text_ptr))[0..text_len]);
}

fn appendTokenText(
    allocator: std.mem.Allocator,
    output: *std.ArrayListUnmanaged(u8),
    token: ?*c.struct_fy_token,
) Error!void {
    const resolved = token orelse return error.EventFormatFailed;
    var text_len: usize = 0;
    const text_ptr = c.fy_token_get_text(resolved, &text_len) orelse return error.EventFormatFailed;
    try output.appendSlice(allocator, @as([*]const u8, @ptrCast(text_ptr))[0..text_len]);
}

fn appendEscapedText(
    allocator: std.mem.Allocator,
    output: *std.ArrayListUnmanaged(u8),
    text: []const u8,
) Error!void {
    var view = std.unicode.Utf8View.init(text) catch return error.EventFormatFailed;
    var iter = view.iterator();

    while (iter.nextCodepointSlice()) |slice| {
        const codepoint = std.unicode.utf8Decode(slice) catch return error.EventFormatFailed;

        switch (codepoint) {
            '\\' => try output.appendSlice(allocator, "\\\\"),
            0 => try output.appendSlice(allocator, "\\0"),
            '\x08' => try output.appendSlice(allocator, "\\b"),
            '\x0c' => try output.appendSlice(allocator, "\\f"),
            '\n' => try output.appendSlice(allocator, "\\n"),
            '\r' => try output.appendSlice(allocator, "\\r"),
            '\t' => try output.appendSlice(allocator, "\\t"),
            '\x07' => try output.appendSlice(allocator, "\\a"),
            '\x0b' => try output.appendSlice(allocator, "\\v"),
            '\x1b' => try output.appendSlice(allocator, "\\e"),
            0x85 => try output.appendSlice(allocator, "\\N"),
            0xa0 => try output.appendSlice(allocator, "\\_"),
            0x2028 => try output.appendSlice(allocator, "\\L"),
            0x2029 => try output.appendSlice(allocator, "\\P"),
            else => {
                if ((codepoint >= 0x01 and codepoint <= 0x1f) or codepoint == 0x7f or (codepoint >= 0x80 and codepoint <= 0x9f)) {
                    var buf: [4]u8 = undefined;
                    const escaped = std.fmt.bufPrint(&buf, "\\x{x:0>2}", .{codepoint}) catch return error.EventFormatFailed;
                    try output.appendSlice(allocator, escaped);
                } else {
                    try output.appendSlice(allocator, slice);
                }
            },
        }
    }
}
