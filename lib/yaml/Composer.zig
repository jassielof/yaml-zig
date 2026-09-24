//! Composer that turns parser events into a document tree.
const std = @import("std");
const Error = @import("Error.zig");
const Options = @import("Options.zig");
const Event = @import("Event.zig").Event;
const Schema = @import("Schema.zig");
const Node = @import("Node.zig").Node;
const MapEntry = @import("Node.zig").MapEntry;
const Document = @import("Document.zig");

pub const Composer = @This();

/// Compose a full document from parser events.
///
/// This function takes ownership of the `events` slice and always frees it.
pub fn compose(
    allocator: std.mem.Allocator,
    events: []Event,
    options: Options.Parse,
) !Document {
    const docs = try composeStream(allocator, events, options);
    defer allocator.free(docs);
    if (docs.len != 1) {
        for (docs) |*doc| doc.deinit();
        return Error.Parse.UnexpectedToken;
    }
    return docs[0];
}

/// Compose every document in a stream.
///
/// This function takes ownership of the `events` slice and always frees it.
/// The returned slice is owned by `allocator`; each document must be deinited.
///
/// Each document's node tree lives in that document's arena (O(1) `deinit`).
pub fn composeStream(
    allocator: std.mem.Allocator,
    events: []Event,
    options: Options.Parse,
) ![]Document {
    defer freeEvents(allocator, events);

    if (events.len < 2 or events[0].kind != .stream_start) return Error.Parse.UnexpectedToken;

    var docs: std.ArrayListUnmanaged(Document) = .empty;
    errdefer {
        for (docs.items) |*doc| doc.deinit();
        docs.deinit(allocator);
    }

    var index: usize = 1;
    while (index < events.len and events[index].kind != .stream_end) {
        if (events[index].kind != .document_start) return Error.Parse.UnexpectedToken;
        index += 1;

        var arena = std.heap.ArenaAllocator.init(allocator);
        errdefer _ = arena.deinit();
        const a = arena.allocator();

        // Anchor map and node memory both live in the document arena.
        var anchors: std.StringHashMapUnmanaged(Node) = .empty;

        const root = try composeNode(a, events, &index, options, &anchors, null);
        if (index >= events.len or events[index].kind != .document_end) return Error.Parse.UnexpectedToken;
        index += 1;

        try docs.append(allocator, Document.init(allocator, arena, root));
    }

    if (index >= events.len or events[index].kind != .stream_end) return Error.Parse.UnexpectedToken;
    return docs.toOwnedSlice(allocator);
}

fn composeNode(
    allocator: std.mem.Allocator,
    events: []Event,
    index: *usize,
    options: Options.Parse,
    anchors: *std.StringHashMapUnmanaged(Node),
    skip_anchor: ?[]const u8,
) !Node {
    if (index.* >= events.len) return Error.Parse.UnexpectedToken;
    const ev = events[index.*];
    switch (ev.kind) {
        .scalar => {
            index.* += 1;
            const scalar = ev.data.scalar;
            // Always copy into the document arena. Event buffers (possibly GPA-owned)
            // stay marked `value_owned` so freeEvents can release them.
            const resolved = if (scalar.tag) |tag|
                try resolveTaggedScalar(allocator, tag, scalar.value)
            else
                try Schema.resolveScalar(
                    allocator,
                    scalar.value,
                    scalar.style,
                    options.resolve_core_schema,
                );

            if (scalar.anchor) |anchor_name| {
                if (skip_anchor == null or !std.mem.eql(u8, anchor_name, skip_anchor.?)) {
                    try putAnchor(allocator, anchors, anchor_name, resolved);
                }
            }

            return resolved;
        },
        .sequence_start => {
            const seq_anchor = ev.data.sequence_start.anchor;
            index.* += 1;
            var seq: std.ArrayListUnmanaged(Node) = .empty;

            while (index.* < events.len and events[index.*].kind != .sequence_end) {
                try seq.append(allocator, try composeNode(allocator, events, index, options, anchors, null));
            }
            if (index.* >= events.len or events[index.*].kind != .sequence_end) return Error.Parse.UnexpectedToken;
            index.* += 1;
            const result: Node = .{ .sequence = seq };
            if (seq_anchor) |anchor_name| {
                try putAnchor(allocator, anchors, anchor_name, result);
            }
            return result;
        },
        .mapping_start => {
            const map_anchor = ev.data.mapping_start.anchor;
            index.* += 1;
            var map: std.ArrayListUnmanaged(MapEntry) = .empty;
            // O(1) duplicate-key checks (CRD `properties` blocks are wide).
            var seen: std.StringHashMapUnmanaged(usize) = .empty;

            while (index.* < events.len and events[index.*].kind != .mapping_end) {
                const key_ev = events[index.*];
                const key = switch (key_ev.kind) {
                    .scalar => blk: {
                        const resolved_key = if (key_ev.data.scalar.tag) |tag|
                            try resolveTaggedScalar(allocator, tag, key_ev.data.scalar.value)
                        else
                            try Schema.resolveScalar(
                                allocator,
                                key_ev.data.scalar.value,
                                key_ev.data.scalar.style,
                                options.resolve_core_schema,
                            );
                        if (key_ev.data.scalar.anchor) |anchor_name| {
                            try putAnchor(allocator, anchors, anchor_name, resolved_key);
                        }
                        break :blk try nodeToKeyString(allocator, resolved_key);
                    },
                    .alias => blk: {
                        const alias_name = events[index.*].data.alias.name;
                        const aliased = anchors.get(alias_name) orelse return Error.Parse.InvalidAlias;
                        break :blk try nodeToKeyString(allocator, aliased);
                    },
                    else => return Error.Parse.InvalidMappingKey,
                };
                index.* += 1;
                const key_anchor = if (key_ev.kind == .scalar) key_ev.data.scalar.anchor else null;
                const value = try composeNode(allocator, events, index, options, anchors, key_anchor);

                if (seen.get(key)) |existing_idx| {
                    switch (options.duplicate_keys) {
                        .reject => return Error.Parse.DuplicateKey,
                        .keep_last => {
                            map.items[existing_idx] = .{ .key = key, .value = value };
                            continue;
                        },
                    }
                }

                const idx = map.items.len;
                try map.append(allocator, .{ .key = key, .value = value });
                try seen.put(allocator, key, idx);
            }
            if (index.* >= events.len or events[index.*].kind != .mapping_end) return Error.Parse.UnexpectedToken;
            index.* += 1;
            const result: Node = .{ .mapping = map };
            if (map_anchor) |anchor_name| {
                try putAnchor(allocator, anchors, anchor_name, result);
            }
            return result;
        },
        .alias => {
            const alias_name = ev.data.alias.name;
            const aliased = anchors.get(alias_name) orelse return Error.Parse.InvalidAlias;
            index.* += 1;
            // Clone only when the anchor is actually referenced.
            return aliased.clone(allocator);
        },
        else => return Error.Parse.UnexpectedToken,
    }
}

pub fn freeEvents(allocator: std.mem.Allocator, events: []Event) void {
    for (events) |ev| {
        switch (ev.kind) {
            .scalar => {
                if (ev.data.scalar.value_owned) allocator.free(ev.data.scalar.value);
                if (ev.data.scalar.anchor) |anchor| allocator.free(anchor);
                if (ev.data.scalar.tag) |tag| allocator.free(tag);
            },
            .alias => allocator.free(ev.data.alias.name),
            .sequence_start => {
                if (ev.data.sequence_start.anchor) |a| allocator.free(a);
                if (ev.data.sequence_start.tag) |t| allocator.free(t);
            },
            .mapping_start => {
                if (ev.data.mapping_start.anchor) |a| allocator.free(a);
                if (ev.data.mapping_start.tag) |t| allocator.free(t);
            },
            else => {},
        }
    }
    allocator.free(events);
}

/// Record an anchor without cloning. The node already lives in the document
/// arena; clones happen on first alias use.
fn putAnchor(
    allocator: std.mem.Allocator,
    anchors: *std.StringHashMapUnmanaged(Node),
    name: []const u8,
    value: Node,
) !void {
    const gop = try anchors.getOrPut(allocator, name);
    if (!gop.found_existing) {
        gop.key_ptr.* = try allocator.dupe(u8, name);
    }
    gop.value_ptr.* = value;
}

fn resolveTaggedScalar(allocator: std.mem.Allocator, tag: []const u8, value: []const u8) !Node {
    if (std.mem.eql(u8, tag, "tag:yaml.org,2002:null")) return .null;
    if (std.mem.eql(u8, tag, "tag:yaml.org,2002:bool")) {
        if (std.ascii.eqlIgnoreCase(value, "true")) return .{ .bool = true };
        if (std.ascii.eqlIgnoreCase(value, "false")) return .{ .bool = false };
    }
    if (std.mem.eql(u8, tag, "tag:yaml.org,2002:int")) {
        if (std.fmt.parseInt(i64, value, 10)) |n| return .{ .int = n } else |_| {}
    }
    return .{ .string = try allocator.dupe(u8, value) };
}

fn nodeToKeyString(allocator: std.mem.Allocator, node: Node) ![]u8 {
    return switch (node) {
        .null => try allocator.dupe(u8, "null"),
        .bool => |v| try allocator.dupe(u8, if (v) "true" else "false"),
        .int => |v| try std.fmt.allocPrint(allocator, "{d}", .{v}),
        .float => |v| try std.fmt.allocPrint(allocator, "{d}", .{v}),
        .string => |v| try allocator.dupe(u8, v),
        else => Error.Parse.InvalidMappingKey,
    };
}
