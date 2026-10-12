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
    scan_source: []const u8,
) !Document {
    const docs = try composeStream(allocator, events, options, scan_source, null);
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
/// Borrowed parser scalars are remapped into a single shared pin of `scan_source`
/// (refcounted across documents) so multi-doc streams do not copy the full input
/// once per document.
pub fn composeStream(
    allocator: std.mem.Allocator,
    events: []Event,
    options: Options.Parse,
    scan_source: []const u8,
    scalar_pool: ?*Document.OwnedScalarPool,
) ![]Document {
    defer freeEvents(allocator, events, scan_source);

    if (events.len < 2 or events[0].kind != .stream_start) return Error.Parse.UnexpectedToken;

    var docs: std.ArrayListUnmanaged(Document) = .empty;
    errdefer {
        for (docs.items) |*doc| doc.deinit();
        docs.deinit(allocator);
    }

    // One pin for the whole stream; each document retains it.
    var shared_pin: ?*Document.SharedPin = null;
    errdefer if (shared_pin) |pin| {
        // Documents already appended own their retains; only drop the creator's
        // leftover if we failed before transferring ownership.
        if (docs.items.len == 0) pin.release();
    };

    var index: usize = 1;
    while (index < events.len and events[index].kind != .stream_end) {
        if (events[index].kind != .document_start) return Error.Parse.UnexpectedToken;
        index += 1;

        if (shared_pin == null) {
            shared_pin = if (options.retain_input)
                try Document.SharedPin.create(allocator, scan_source)
            else
                try Document.SharedPin.createBorrowed(allocator, scan_source);
        }
        const pin = shared_pin.?;
        const pinned_source = pin.backing orelse pin.bytes;

        var doc_storage = try Document.DocumentArena.init(allocator, scan_source.len);
        errdefer doc_storage.deinit();
        const a = doc_storage.allocator();

        // Anchor map and node memory both live in the document arena.
        var anchors: std.StringHashMapUnmanaged(Node) = .empty;

        const root = try composeNode(a, allocator, events, &index, options, &anchors, null, scan_source, pinned_source);
        if (index >= events.len or events[index].kind != .document_end) return Error.Parse.UnexpectedToken;
        index += 1;

        pin.retain();
        if (scalar_pool) |pool| pool.retain();
        docs.append(allocator, Document.init(allocator, doc_storage, root, pin, scalar_pool)) catch |err| {
            pin.release();
            if (scalar_pool) |pool| pool.release();
            return err;
        };
    }

    if (index >= events.len or events[index].kind != .stream_end) return Error.Parse.UnexpectedToken;

    // Created but never retained (empty stream body).
    if (docs.items.len == 0) {
        if (shared_pin) |pin| pin.release();
        if (scalar_pool) |pool| pool.release();
    } else if (scalar_pool) |pool| {
        pool.release(); // drop parseStream's initial ref; documents hold retains
    }

    return docs.toOwnedSlice(allocator);
}

fn composeNode(
    allocator: std.mem.Allocator,
    event_gpa: std.mem.Allocator,
    events: []Event,
    index: *usize,
    options: Options.Parse,
    anchors: *std.StringHashMapUnmanaged(Node),
    skip_anchor: ?[]const u8,
    scan_source: []const u8,
    pinned_source: []const u8,
) !Node {
    if (index.* >= events.len) return Error.Parse.UnexpectedToken;
    const ev = events[index.*];
    switch (ev.kind) {
        .scalar => {
            const event_index = index.*;
            index.* += 1;
            const resolved = try resolveEventScalar(
                allocator,
                event_gpa,
                &events[event_index].data.scalar,
                options,
                scan_source,
                pinned_source,
            );

            if (events[event_index].data.scalar.anchor) |anchor_name| {
                if (skip_anchor == null or !std.mem.eql(u8, anchor_name, skip_anchor.?)) {
                    try putAnchor(allocator, scan_source, anchors, anchor_name, resolved);
                }
            }

            return resolved;
        },
        .sequence_start => {
            const seq_anchor = ev.data.sequence_start.anchor;
            index.* += 1;
            var seq: std.ArrayListUnmanaged(Node) = .empty;

            while (index.* < events.len and events[index.*].kind != .sequence_end) {
                try seq.append(allocator, try composeNode(allocator, event_gpa, events, index, options, anchors, null, scan_source, pinned_source));
            }
            if (index.* >= events.len or events[index.*].kind != .sequence_end) return Error.Parse.UnexpectedToken;
            index.* += 1;
            const result: Node = .{ .sequence = seq };
            if (seq_anchor) |anchor_name| {
                try putAnchor(allocator, scan_source, anchors, anchor_name, result);
            }
            return result;
        },
        .mapping_start => {
            const map_anchor = ev.data.mapping_start.anchor;
            index.* += 1;
            var entries: std.ArrayListUnmanaged(MapEntry) = .empty;
            var dedup: MappingDedup = .{};
            errdefer {
                dedup.deinit(allocator);
                entries.deinit(allocator);
            }

            while (index.* < events.len and events[index.*].kind != .mapping_end) {
                const key_ev = events[index.*];
                const key = switch (key_ev.kind) {
                    .scalar => try composeMappingKey(
                        allocator,
                        event_gpa,
                        &events[index.*].data.scalar,
                        options,
                        anchors,
                        scan_source,
                        pinned_source,
                    ),
                    .alias => blk: {
                        const alias_name = events[index.*].data.alias.name;
                        const aliased = anchors.get(alias_name) orelse return Error.Parse.InvalidAlias;
                        break :blk try mappingKeyBytes(allocator, aliased);
                    },
                    else => return Error.Parse.InvalidMappingKey,
                };
                index.* += 1;
                const key_anchor = if (key_ev.kind == .scalar) key_ev.data.scalar.anchor else null;
                const value = try composeNode(allocator, event_gpa, events, index, options, anchors, key_anchor, scan_source, pinned_source);

                try insertMappingEntry(allocator, &entries, &dedup, options, key, value);
            }
            if (index.* >= events.len or events[index.*].kind != .mapping_end) return Error.Parse.UnexpectedToken;
            index.* += 1;
            dedup.deinit(allocator);
            const result: Node = .{ .mapping = entries };
            if (map_anchor) |anchor_name| {
                try putAnchor(allocator, scan_source, anchors, anchor_name, result);
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

/// Build mapping key bytes; fast-path plain CRD-style identifiers (skip Core resolution).
fn composeMappingKey(
    allocator: std.mem.Allocator,
    event_gpa: std.mem.Allocator,
    scalar: *Event.Scalar,
    options: Options.Parse,
    anchors: *std.StringHashMapUnmanaged(Node),
    scan_source: []const u8,
    pinned_source: []const u8,
) ![]u8 {
    const needs_full_resolve = scalar.tag != null or
        scalar.style != .plain or
        (options.resolve_core_schema and Schema.plainScalarNeedsCoreResolution(scalar.value));

    if (needs_full_resolve) {
        const resolved_key = try resolveEventScalar(
            allocator,
            event_gpa,
            scalar,
            options,
            scan_source,
            pinned_source,
        );
        if (scalar.anchor) |anchor_name| {
            try putAnchor(allocator, scan_source, anchors, anchor_name, resolved_key);
        }
        return mappingKeyBytes(allocator, resolved_key);
    }

    const trimmed = std.mem.trim(u8, scalar.value, " \t");
    const key_bytes: []const u8 = blk: {
        if (!scalar.value_owned) {
            if (remapPinned(scan_source, pinned_source, trimmed)) |pin| break :blk pin;
            if (isSubslice(pinned_source, trimmed)) break :blk trimmed;
        }
        break :blk try allocator.dupe(u8, trimmed);
    };

    if (scalar.anchor) |anchor_name| {
        const anchored: Node = .{ .string = @constCast(key_bytes) };
        try putAnchor(allocator, scan_source, anchors, anchor_name, anchored);
    }
    return @constCast(key_bytes);
}

fn resolveEventScalar(
    doc_allocator: std.mem.Allocator,
    event_gpa: std.mem.Allocator,
    scalar: *Event.Scalar,
    options: Options.Parse,
    scan_source: []const u8,
    pinned_source: []const u8,
) !Node {
    if (scalar.tag) |tag| {
        defer if (scalar.value_owned and !scalar.pool_backed) {
            event_gpa.free(scalar.value);
            scalar.value_owned = false;
        };
        return resolveTaggedScalar(doc_allocator, tag, scalar.value);
    }

    const pinned = if (!scalar.value_owned) blk: {
        if (remapPinned(scan_source, pinned_source, scalar.value)) |p| break :blk p;
        if (isSubslice(pinned_source, scalar.value)) break :blk scalar.value;
        break :blk null;
    } else null;

    const node = try Schema.resolveScalarInDocument(
        doc_allocator,
        scalar.value,
        scalar.value_owned,
        scalar.pool_backed,
        event_gpa,
        pinned,
        scalar.style,
        options.resolve_core_schema,
    );
    if (scalar.value_owned and !scalar.pool_backed) {
        scalar.value_owned = false;
    }
    return node;
}

fn remapPinned(scan_source: []const u8, pinned_source: []const u8, value: []const u8) ?[]const u8 {
    if (value.len == 0) return pinned_source[0..0];
    if (!isSubslice(scan_source, value)) return null;
    const base = @intFromPtr(scan_source.ptr);
    const start = @intFromPtr(value.ptr);
    if (start < base) return null;
    const off = start - base;
    if (off + value.len > pinned_source.len) return null;
    return pinned_source[off .. off + value.len];
}

fn isSubslice(haystack: []const u8, needle: []const u8) bool {
    if (needle.len == 0) return true;
    const n_start = @intFromPtr(needle.ptr);
    const h_start = @intFromPtr(haystack.ptr);
    const h_end = h_start + haystack.len;
    return n_start >= h_start and n_start + needle.len <= h_end;
}

pub fn freeEvents(allocator: std.mem.Allocator, events: []Event, scan_source: []const u8) void {
    for (events) |ev| {
        releaseOneEvent(allocator, scan_source, ev);
    }
    allocator.free(events);
}

/// Record an anchor without cloning. The node already lives in the document
/// arena; clones happen on first alias use.
fn putAnchor(
    allocator: std.mem.Allocator,
    scan_source: []const u8,
    anchors: *std.StringHashMapUnmanaged(Node),
    name: []const u8,
    value: Node,
) !void {
    const gop = try anchors.getOrPut(allocator, name);
    if (!gop.found_existing) {
        gop.key_ptr.* = try persistName(allocator, scan_source, name);
    }
    gop.value_ptr.* = value;
}

fn persistName(allocator: std.mem.Allocator, scan_source: []const u8, name: []const u8) ![]const u8 {
    if (name.len == 0) return name;
    if (isSubslice(scan_source, name)) return @constCast(name);
    return try allocator.dupe(u8, name);
}

fn freeNameIfOwned(allocator: std.mem.Allocator, scan_source: []const u8, name: []const u8) void {
    if (name.len == 0) return;
    if (isSubslice(scan_source, name)) return;
    allocator.free(name);
}

const MappingDedup = struct {
    index: ?std.StringHashMapUnmanaged(usize) = null,

    fn deinit(self: *MappingDedup, allocator: std.mem.Allocator) void {
        if (self.index) |*map| {
            map.deinit(allocator);
            self.index = null;
        }
    }

    fn ensureIndex(self: *MappingDedup, allocator: std.mem.Allocator, entries: []const MapEntry) !void {
        if (self.index != null) return;
        var map: std.StringHashMapUnmanaged(usize) = .empty;
        try map.ensureTotalCapacity(allocator, @intCast(entries.len));
        for (entries, 0..) |entry, i| {
            try map.put(allocator, entry.key, i);
        }
        self.index = map;
    }
};

fn insertMappingEntry(
    allocator: std.mem.Allocator,
    entries: *std.ArrayListUnmanaged(MapEntry),
    dedup: *MappingDedup,
    options: Options.Parse,
    key: []const u8,
    value: Node,
) !void {
    if (dedup.index) |*map| {
        if (map.get(key)) |idx| {
            switch (options.duplicate_keys) {
                .reject => return Error.Parse.DuplicateKey,
                .keep_last => {
                    entries.items[idx].value = value;
                    return;
                },
            }
        }
        const idx = entries.items.len;
        try entries.append(allocator, .{ .key = @constCast(key), .value = value });
        try map.put(allocator, key, idx);
        return;
    }

    for (entries.items) |*entry| {
        if (!std.mem.eql(u8, entry.key, key)) continue;
        switch (options.duplicate_keys) {
            .reject => return Error.Parse.DuplicateKey,
            .keep_last => {
                entry.value = value;
                return;
            },
        }
    }
    try entries.append(allocator, .{ .key = @constCast(key), .value = value });
    if (entries.items.len >= 16) try dedup.ensureIndex(allocator, entries.items);
}

fn resolveTaggedScalar(allocator: std.mem.Allocator, tag: []const u8, value: []const u8) !Node {
    if (std.mem.eql(u8, tag, "tag:yaml.org,2002:null")) return .null;
    if (std.mem.eql(u8, tag, "tag:yaml.org,2002:str")) {
        return .{ .string = try allocator.dupe(u8, value) };
    }
    if (std.mem.eql(u8, tag, "tag:yaml.org,2002:bool")) {
        if (std.ascii.eqlIgnoreCase(value, "true")) return .{ .bool = true };
        if (std.ascii.eqlIgnoreCase(value, "false")) return .{ .bool = false };
    }
    if (std.mem.eql(u8, tag, "tag:yaml.org,2002:int")) {
        if (std.fmt.parseInt(i64, value, 10)) |n| return .{ .int = n } else |_| {}
    }
    return .{ .string = try allocator.dupe(u8, value) };
}

/// Mapping lookup key bytes. Reuses string node storage when possible (Patch 3).
fn mappingKeyBytes(allocator: std.mem.Allocator, node: Node) ![]u8 {
    return switch (node) {
        .null => try allocator.dupe(u8, "null"),
        .bool => |v| try allocator.dupe(u8, if (v) "true" else "false"),
        .int => |v| try std.fmt.allocPrint(allocator, "{d}", .{v}),
        .float => |v| try std.fmt.allocPrint(allocator, "{d}", .{v}),
        .string => |v| v,
        else => Error.Parse.InvalidMappingKey,
    };
}

fn releaseOneEvent(allocator: std.mem.Allocator, scan_source: []const u8, ev: Event) void {
    switch (ev.kind) {
        .scalar => {
            if (ev.data.scalar.value_owned and !ev.data.scalar.pool_backed) {
                allocator.free(ev.data.scalar.value);
            }
            if (ev.data.scalar.anchor) |anchor| freeNameIfOwned(allocator, scan_source, anchor);
            if (ev.data.scalar.tag) |tag| allocator.free(tag);
        },
        .alias => freeNameIfOwned(allocator, scan_source, ev.data.alias.name),
        .sequence_start => {
            if (ev.data.sequence_start.anchor) |a| freeNameIfOwned(allocator, scan_source, a);
            if (ev.data.sequence_start.tag) |t| allocator.free(t);
        },
        .mapping_start => {
            if (ev.data.mapping_start.anchor) |a| freeNameIfOwned(allocator, scan_source, a);
            if (ev.data.mapping_start.tag) |t| allocator.free(t);
        },
        else => {},
    }
}

/// Incrementally compose while the parser emits events (no full-stream `Event[]`).
pub const Stream = struct {
    const Frame = union(enum) {
        sequence: struct {
            nodes: std.ArrayListUnmanaged(Node) = .empty,
            anchor: ?[]const u8 = null,
        },
        mapping: struct {
            entries: std.ArrayListUnmanaged(MapEntry) = .empty,
            dedup: MappingDedup = .{},
            awaiting_key: bool = true,
            pending_key: ?[]const u8 = null,
            key_anchor: ?[]const u8 = null,
            anchor: ?[]const u8 = null,
        },
    };

    gpa: std.mem.Allocator,
    options: Options.Parse,
    scan_source: []const u8,
    scalar_pool: ?*Document.OwnedScalarPool,
    shared_pin: ?*Document.SharedPin = null,
    docs: std.ArrayListUnmanaged(Document) = .empty,
    doc_storage: Document.DocumentArena,
    doc_a: std.mem.Allocator,
    anchors: std.StringHashMapUnmanaged(Node) = .empty,
    stack: std.ArrayListUnmanaged(Frame) = .empty,
    skip_anchor: ?[]const u8 = null,
    root: Node = .null,
    in_document: bool = false,

    pub fn init(
        gpa: std.mem.Allocator,
        options: Options.Parse,
        scan_source: []const u8,
        scalar_pool: ?*Document.OwnedScalarPool,
    ) Stream {
        return .{
            .gpa = gpa,
            .options = options,
            .scan_source = scan_source,
            .scalar_pool = scalar_pool,
            .doc_storage = Document.DocumentArena.initDirect(gpa),
            .doc_a = undefined,
        };
    }

    pub fn deinit(self: *Stream) void {
        for (self.docs.items) |*doc| doc.deinit();
        self.docs.deinit(self.gpa);
        if (self.in_document) {
            self.stack.deinit(self.doc_a);
            self.anchors.deinit(self.doc_a);
        }
        self.doc_storage.deinit();
        if (self.shared_pin) |pin| pin.release();
    }

    pub fn feed(self: *Stream, event_gpa: std.mem.Allocator, ev_in: Event) !void {
        var ev = ev_in;
        errdefer releaseOneEvent(event_gpa, self.scan_source, ev);
        try self.feedInner(event_gpa, &ev);
        releaseOneEvent(event_gpa, self.scan_source, ev);
    }

    fn feedInner(self: *Stream, event_gpa: std.mem.Allocator, ev: *Event) !void {
        switch (ev.kind) {
            .stream_start, .stream_end => {},
            .document_start => try self.beginDocument(),
            .document_end => try self.endDocument(),
            .sequence_start => try self.pushSequence(ev.data.sequence_start.anchor),
            .sequence_end => try self.endSequence(),
            .mapping_start => try self.pushMapping(ev.data.mapping_start.anchor),
            .mapping_end => try self.endMapping(),
            .scalar, .alias => try self.feedValue(event_gpa, ev),
        }
    }

    fn beginDocument(self: *Stream) !void {
        if (self.shared_pin == null) {
            self.shared_pin = if (self.options.retain_input)
                try Document.SharedPin.create(self.gpa, self.scan_source)
            else
                try Document.SharedPin.createBorrowed(self.gpa, self.scan_source);
        }
        self.doc_storage.deinit();
        self.doc_storage = try Document.DocumentArena.init(self.gpa, self.scan_source.len);
        self.doc_a = self.doc_storage.allocator();
        self.anchors = .{};
        self.stack.clearRetainingCapacity();
        self.root = .null;
        self.skip_anchor = null;
        self.in_document = true;
    }

    fn endDocument(self: *Stream) !void {
        if (!self.in_document) return Error.Parse.UnexpectedToken;
        if (self.stack.items.len != 0) return Error.Parse.UnexpectedToken;
        const pin = self.shared_pin orelse return Error.Parse.UnexpectedToken;
        pin.retain();
        if (self.scalar_pool) |pool| pool.retain();
        const doc_storage = self.doc_storage;
        try self.docs.append(self.gpa, Document.init(self.gpa, doc_storage, self.root, pin, self.scalar_pool));
        self.in_document = false;
        self.doc_storage = Document.DocumentArena.initDirect(self.gpa);
    }

    fn pinnedSource(self: *Stream) []const u8 {
        const pin = self.shared_pin.?;
        return pin.backing orelse pin.bytes;
    }

    /// Writable copy of the input when `retain_input` is true (block scalars compact in-place).
    pub fn mutablePinnedBytes(self: *Stream) ?[]u8 {
        const pin = self.shared_pin orelse return null;
        return pin.mutableBacking();
    }

    fn keepAnchorName(self: *Stream, anchor: ?[]const u8) !?[]const u8 {
        if (anchor) |a| return try persistName(self.doc_a, self.scan_source, a);
        return null;
    }

    fn pushSequence(self: *Stream, anchor: ?[]const u8) !void {
        try self.stack.append(self.doc_a, .{ .sequence = .{ .anchor = try self.keepAnchorName(anchor) } });
    }

    fn endSequence(self: *Stream) !void {
        const frame = self.stack.pop() orelse return Error.Parse.UnexpectedToken;
        const seq_frame = switch (frame) {
            .sequence => |s| s,
            else => return Error.Parse.UnexpectedToken,
        };
        const result: Node = .{ .sequence = seq_frame.nodes };
        if (seq_frame.anchor) |anchor_name| {
            try putAnchor(self.doc_a, self.scan_source, &self.anchors, anchor_name, result);
        }
        try self.completeNode(result);
    }

    fn pushMapping(self: *Stream, anchor: ?[]const u8) !void {
        try self.stack.append(self.doc_a, .{ .mapping = .{ .anchor = try self.keepAnchorName(anchor) } });
    }

    fn endMapping(self: *Stream) !void {
        const frame = self.stack.pop() orelse return Error.Parse.UnexpectedToken;
        var map_frame = switch (frame) {
            .mapping => |m| m,
            else => return Error.Parse.UnexpectedToken,
        };
        map_frame.dedup.deinit(self.doc_a);
        const result: Node = .{ .mapping = map_frame.entries };
        if (map_frame.anchor) |anchor_name| {
            try putAnchor(self.doc_a, self.scan_source, &self.anchors, anchor_name, result);
        }
        try self.completeNode(result);
    }

    fn mappingOnTop(self: *Stream) ?*Frame {
        if (self.stack.items.len == 0) return null;
        const top = &self.stack.items[self.stack.items.len - 1];
        return switch (top.*) {
            .mapping => top,
            else => null,
        };
    }

    fn feedValue(self: *Stream, event_gpa: std.mem.Allocator, ev: *Event) !void {
        if (self.mappingOnTop()) |m| {
            if (m.mapping.awaiting_key) {
                try self.ingestMappingKey(event_gpa, ev);
                return;
            }
        }
        const node = try self.nodeFromValueEvent(event_gpa, ev);
        try self.completeNode(node);
    }

    fn ingestMappingKey(self: *Stream, event_gpa: std.mem.Allocator, ev: *Event) !void {
        const frame = self.mappingOnTop() orelse return Error.Parse.UnexpectedToken;
        const m = &frame.mapping;
        const key: []const u8 = switch (ev.kind) {
            .scalar => try composeMappingKey(
                self.doc_a,
                event_gpa,
                &ev.data.scalar,
                self.options,
                &self.anchors,
                self.scan_source,
                self.pinnedSource(),
            ),
            .alias => try mappingKeyBytes(
                self.doc_a,
                self.anchors.get(ev.data.alias.name) orelse return Error.Parse.InvalidAlias,
            ),
            else => return Error.Parse.InvalidMappingKey,
        };
        m.pending_key = key;
        m.key_anchor = if (ev.kind == .scalar) try self.keepAnchorName(ev.data.scalar.anchor) else null;
        m.awaiting_key = false;
        self.skip_anchor = m.key_anchor;
    }

    fn nodeFromValueEvent(self: *Stream, event_gpa: std.mem.Allocator, ev: *Event) !Node {
        return switch (ev.kind) {
            .scalar => {
                const resolved = try resolveEventScalar(
                    self.doc_a,
                    event_gpa,
                    &ev.data.scalar,
                    self.options,
                    self.scan_source,
                    self.pinnedSource(),
                );
                if (ev.data.scalar.anchor) |anchor_name| {
                    if (self.skip_anchor == null or !std.mem.eql(u8, anchor_name, self.skip_anchor.?)) {
                        try putAnchor(self.doc_a, self.scan_source, &self.anchors, anchor_name, resolved);
                    }
                }
                self.skip_anchor = null;
                return resolved;
            },
            .alias => {
                const aliased = self.anchors.get(ev.data.alias.name) orelse return Error.Parse.InvalidAlias;
                return aliased.clone(self.doc_a);
            },
            else => Error.Parse.UnexpectedToken,
        };
    }

    fn completeNode(self: *Stream, node: Node) !void {
        if (self.stack.items.len == 0) {
            self.root = node;
            return;
        }
        const top = &self.stack.items[self.stack.items.len - 1];
        switch (top.*) {
            .sequence => |*seq| try seq.nodes.append(self.doc_a, node),
            .mapping => |*m| {
                const key = m.pending_key orelse return Error.Parse.UnexpectedToken;
                m.pending_key = null;
                m.key_anchor = null;
                m.awaiting_key = true;
                self.skip_anchor = null;
                try insertMappingEntry(self.doc_a, &m.entries, &m.dedup, self.options, key, node);
            },
        }
    }

    pub fn finish(self: *Stream, gpa: std.mem.Allocator) ![]Document {
        if (self.docs.items.len == 0) {
            if (self.shared_pin) |pin| pin.release();
            if (self.scalar_pool) |pool| pool.release();
            self.doc_storage.deinit();
            return &.{};
        }
        if (self.scalar_pool) |pool| pool.release();
        const out = try self.docs.toOwnedSlice(gpa);
        self.docs = .empty;
        self.doc_storage.deinit();
        self.shared_pin = null;
        return out;
    }
};
