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
    defer freeEvents(allocator, events);

    var anchors: std.StringHashMapUnmanaged(Node) = .{};
    defer {
        var it = anchors.iterator();
        while (it.next()) |entry| {
            allocator.free(entry.key_ptr.*);
            entry.value_ptr.deinit(allocator);
        }
        anchors.deinit(allocator);
    }

    if (events.len < 4) return Error.Parse.UnexpectedToken;
    if (events[0].kind != .stream_start or events[1].kind != .document_start) {
        return Error.Parse.UnexpectedToken;
    }

    var index: usize = 2;
    const root = try composeNode(allocator, events, &index, options, &anchors, null);

    if (index >= events.len or events[index].kind != .document_end) {
        var owned_root = root;
        owned_root.deinit(allocator);
        return Error.Parse.UnexpectedToken;
    }
    index += 1;
    if (index >= events.len or events[index].kind != .stream_end) {
        var owned_root = root;
        owned_root.deinit(allocator);
        return Error.Parse.UnexpectedToken;
    }

    return Document.init(allocator, root);
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
            const resolved = try Schema.resolveScalar(
                allocator,
                ev.data.scalar.value,
                ev.data.scalar.style,
                options.resolve_core_schema,
            );

            if (ev.data.scalar.anchor) |anchor_name| {
                if (skip_anchor == null or !std.mem.eql(u8, anchor_name, skip_anchor.?)) {
                    try putAnchor(allocator, anchors, anchor_name, resolved);
                }
            }

            return resolved;
        },
        .sequence_start => {
            const seq_anchor = ev.data.sequence_start.anchor;
            index.* += 1;
            var seq: std.ArrayListUnmanaged(Node) = .{};
            errdefer {
                for (seq.items) |*item| item.deinit(allocator);
                seq.deinit(allocator);
            }

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
            var map: std.ArrayListUnmanaged(MapEntry) = .{};
            errdefer {
                for (map.items) |*entry| {
                    allocator.free(entry.key);
                    entry.value.deinit(allocator);
                }
                map.deinit(allocator);
            }

            while (index.* < events.len and events[index.*].kind != .mapping_end) {
                const key_ev = events[index.*];
                const key = switch (key_ev.kind) {
                    .scalar => blk: {
                        var resolved_key = try Schema.resolveScalar(
                            allocator,
                            key_ev.data.scalar.value,
                            key_ev.data.scalar.style,
                            options.resolve_core_schema,
                        );
                        defer resolved_key.deinit(allocator);
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
                errdefer allocator.free(key);
                index.* += 1;
                const key_anchor = if (key_ev.kind == .scalar) key_ev.data.scalar.anchor else null;
                var value = try composeNode(allocator, events, index, options, anchors, key_anchor);
                errdefer value.deinit(allocator);

                if (findMapKey(map.items, key)) |existing_idx| {
                    switch (options.duplicate_keys) {
                        .reject => {
                            return Error.Parse.DuplicateKey;
                        },
                        .keep_last => {
                            allocator.free(map.items[existing_idx].key);
                            map.items[existing_idx].value.deinit(allocator);
                            map.items[existing_idx] = .{ .key = key, .value = value };
                            continue;
                        },
                    }
                }

                try map.append(allocator, .{ .key = key, .value = value });
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
            return aliased.clone(allocator);
        },
        else => return Error.Parse.UnexpectedToken,
    }
}

fn freeEvents(allocator: std.mem.Allocator, events: []Event) void {
    for (events) |ev| {
        switch (ev.kind) {
            .scalar => {
                allocator.free(ev.data.scalar.value);
                if (ev.data.scalar.anchor) |anchor| allocator.free(anchor);
            },
            .alias => allocator.free(ev.data.alias.name),
            .sequence_start => if (ev.data.sequence_start.anchor) |a| allocator.free(a),
            .mapping_start => if (ev.data.mapping_start.anchor) |a| allocator.free(a),
            else => {},
        }
    }
    allocator.free(events);
}

fn putAnchor(
    allocator: std.mem.Allocator,
    anchors: *std.StringHashMapUnmanaged(Node),
    name: []const u8,
    value: Node,
) !void {
    if (anchors.getEntry(name)) |entry| {
        entry.value_ptr.deinit(allocator);
        entry.value_ptr.* = try value.clone(allocator);
        return;
    }

    try anchors.put(allocator, try allocator.dupe(u8, name), try value.clone(allocator));
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

fn findMapKey(items: []const MapEntry, key: []const u8) ?usize {
    for (items, 0..) |entry, idx| {
        if (std.mem.eql(u8, entry.key, key)) return idx;
    }
    return null;
}
