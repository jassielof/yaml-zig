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

    if (events.len < 4) return Error.Parse.UnexpectedToken;
    if (events[0].kind != .stream_start or events[1].kind != .document_start) {
        return Error.Parse.UnexpectedToken;
    }

    var index: usize = 2;
    const root = try composeNode(allocator, events, &index, options);

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
) !Node {
    if (index.* >= events.len) return Error.Parse.UnexpectedToken;
    const ev = events[index.*];
    switch (ev.kind) {
        .scalar => {
            index.* += 1;
            return Schema.resolveScalar(
                allocator,
                ev.data.scalar.value,
                ev.data.scalar.style,
                options.resolve_core_schema,
            );
        },
        .sequence_start => {
            index.* += 1;
            var seq: std.ArrayListUnmanaged(Node) = .{};
            errdefer {
                for (seq.items) |*item| item.deinit(allocator);
                seq.deinit(allocator);
            }

            while (index.* < events.len and events[index.*].kind != .sequence_end) {
                try seq.append(allocator, try composeNode(allocator, events, index, options));
            }
            if (index.* >= events.len or events[index.*].kind != .sequence_end) return Error.Parse.UnexpectedToken;
            index.* += 1;
            return .{ .sequence = seq };
        },
        .mapping_start => {
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
                if (events[index.*].kind != .scalar) return Error.Parse.InvalidMappingKey;
                const key = try allocator.dupe(u8, events[index.*].data.scalar.value);
                index.* += 1;
                var value = try composeNode(allocator, events, index, options);

                if (findMapKey(map.items, key)) |existing_idx| {
                    switch (options.duplicate_keys) {
                        .reject => {
                            allocator.free(key);
                            value.deinit(allocator);
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
            return .{ .mapping = map };
        },
        .alias => return Error.Parse.UnsupportedFeature,
        else => return Error.Parse.UnexpectedToken,
    }
}

fn freeEvents(allocator: std.mem.Allocator, events: []Event) void {
    for (events) |ev| {
        switch (ev.kind) {
            .scalar => allocator.free(ev.data.scalar.value),
            else => {},
        }
    }
    allocator.free(events);
}

fn findMapKey(items: []const MapEntry, key: []const u8) ?usize {
    for (items, 0..) |entry, idx| {
        if (std.mem.eql(u8, entry.key, key)) return idx;
    }
    return null;
}
