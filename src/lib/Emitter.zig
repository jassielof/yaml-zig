//! YAML text emission helpers.
const std = @import("std");
const Node = @import("Node.zig").Node;
const Options = @import("Options.zig");

pub const Emitter = @This();

allocator: std.mem.Allocator,
options: Options.Stringify,
buffer: std.ArrayListUnmanaged(u8),

pub fn init(allocator: std.mem.Allocator, options: Options.Stringify) Emitter {
    return .{
        .allocator = allocator,
        .options = options,
        .buffer = .{},
    };
}

pub fn deinit(self: *Emitter) void {
    self.buffer.deinit(self.allocator);
    self.* = undefined;
}

pub fn emitDocument(self: *Emitter, root: *const Node) anyerror![]u8 {
    if (self.options.emit_document_markers) {
        try self.buffer.appendSlice(self.allocator, "---\n");
    }
    try self.emitNode(root, 0, true);
    if (self.options.emit_document_markers) {
        try self.buffer.appendSlice(self.allocator, "\n...\n");
    } else if (self.buffer.items.len == 0 or self.buffer.items[self.buffer.items.len - 1] != '\n') {
        try self.buffer.append(self.allocator, '\n');
    }
    return self.buffer.toOwnedSlice(self.allocator);
}

fn emitNode(self: *Emitter, node: *const Node, indent: usize, line_start: bool) anyerror!void {
    switch (node.*) {
        .null => try self.writeText("null", indent, line_start),
        .bool => |v| try self.writeText(if (v) "true" else "false", indent, line_start),
        .int => |v| try self.writeFmt(indent, line_start, "{d}", .{v}),
        .float => |v| try self.writeFmt(indent, line_start, "{d}", .{v}),
        .string => |v| try self.writeScalar(v, indent, line_start),
        .sequence => |seq| try self.emitSequence(seq.items, indent, line_start),
        .mapping => |map| try self.emitMapping(map.items, indent, line_start),
    }
}

fn emitSequence(self: *Emitter, seq: []const Node, indent: usize, line_start: bool) anyerror!void {
    if (seq.len == 0) {
        try self.writeText("[]", indent, line_start);
        return;
    }

    var first = true;
    for (seq) |*item| {
        if (!first) try self.buffer.append(self.allocator, '\n');
        first = false;
        try self.writeIndent(indent);
        try self.buffer.appendSlice(self.allocator, "- ");
        if (item.isScalar()) {
            try self.emitNode(item, indent + self.options.indent_width, false);
        } else {
            try self.emitNode(item, indent + self.options.indent_width, true);
        }
    }
}

fn emitMapping(self: *Emitter, map: []const @import("Node.zig").MapEntry, indent: usize, line_start: bool) anyerror!void {
    if (map.len == 0) {
        try self.writeText("{}", indent, line_start);
        return;
    }

    var first = true;
    for (map) |*entry| {
        if (!first) try self.buffer.append(self.allocator, '\n');
        first = false;
        if (line_start) try self.writeIndent(indent);
        try self.writeKey(entry.key);
        try self.buffer.appendSlice(self.allocator, ":");
        if (entry.value.isScalar()) {
            try self.buffer.append(self.allocator, ' ');
            try self.emitNode(&entry.value, indent + self.options.indent_width, false);
        } else {
            try self.buffer.append(self.allocator, '\n');
            try self.emitNode(&entry.value, indent + self.options.indent_width, true);
        }
    }
}

fn writeText(self: *Emitter, text: []const u8, indent: usize, line_start: bool) anyerror!void {
    if (line_start) try self.writeIndent(indent);
    try self.buffer.appendSlice(self.allocator, text);
}

fn writeFmt(
    self: *Emitter,
    indent: usize,
    line_start: bool,
    comptime fmt: []const u8,
    args: anytype,
) anyerror!void {
    if (line_start) try self.writeIndent(indent);
    var tmp: [128]u8 = undefined;
    const rendered = try std.fmt.bufPrint(&tmp, fmt, args);
    try self.buffer.appendSlice(self.allocator, rendered);
}

fn writeIndent(self: *Emitter, indent: usize) anyerror!void {
    try self.buffer.appendNTimes(self.allocator, ' ', indent);
}

fn writeKey(self: *Emitter, key: []const u8) anyerror!void {
    if (needsQuotes(key)) {
        try self.buffer.append(self.allocator, '"');
        try writeEscaped(self, key);
        try self.buffer.append(self.allocator, '"');
    } else {
        try self.buffer.appendSlice(self.allocator, key);
    }
}

fn writeScalar(self: *Emitter, text: []const u8, indent: usize, line_start: bool) anyerror!void {
    if (line_start) try self.writeIndent(indent);
    if (text.len == 0) {
        try self.buffer.appendSlice(self.allocator, "\"\"");
        return;
    }
    if (needsQuotes(text)) {
        try self.buffer.append(self.allocator, '"');
        try writeEscaped(self, text);
        try self.buffer.append(self.allocator, '"');
    } else {
        try self.buffer.appendSlice(self.allocator, text);
    }
}

fn writeEscaped(self: *Emitter, text: []const u8) anyerror!void {
    for (text) |c| {
        switch (c) {
            '"' => try self.buffer.appendSlice(self.allocator, "\\\""),
            '\\' => try self.buffer.appendSlice(self.allocator, "\\\\"),
            '\n' => try self.buffer.appendSlice(self.allocator, "\\n"),
            '\r' => try self.buffer.appendSlice(self.allocator, "\\r"),
            '\t' => try self.buffer.appendSlice(self.allocator, "\\t"),
            else => try self.buffer.append(self.allocator, c),
        }
    }
}

fn needsQuotes(text: []const u8) bool {
    if (text.len == 0) return true;
    if (std.mem.eql(u8, text, "null") or std.mem.eql(u8, text, "~")) return true;
    if (std.ascii.eqlIgnoreCase(text, "true") or std.ascii.eqlIgnoreCase(text, "false")) return true;
    for (text) |c| {
        if (c == ':' or c == '#' or c == '\n' or c == '\r' or c == '\t' or c == '[' or c == ']' or c == '{' or c == '}' or c == ',') {
            return true;
        }
    }
    if (text[0] == '-' or text[0] == '?' or text[0] == '!' or text[0] == '&' or text[0] == '*') return true;
    return false;
}
