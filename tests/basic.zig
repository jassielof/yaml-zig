const std = @import("std");
const testing = std.testing;
const yaml = @import("yaml");

fn parseDoc(source: []const u8) !yaml.Document {
    return yaml.parseDocument(testing.allocator, source, .{});
}

fn findMapValue(map: []const yaml.Node.MapEntry, key: []const u8) ?*const yaml.Node.Node {
    for (map) |*entry| {
        if (std.mem.eql(u8, entry.key, key)) return &entry.value;
    }
    return null;
}

test "parse block mapping and sequence" {
    const source =
        \\language: zig
        \\versions:
        \\  - 0.15
        \\  - 0.16
        \\features:
        \\  parser: true
        \\  serializer: true
    ;

    var doc = try parseDoc(source);
    defer doc.deinit();

    switch (doc.root) {
        .mapping => |map| {
            const language = findMapValue(map.items, "language") orelse return error.TestUnexpectedResult;
            const versions = findMapValue(map.items, "versions") orelse return error.TestUnexpectedResult;
            const features = findMapValue(map.items, "features") orelse return error.TestUnexpectedResult;

            try testing.expectEqualStrings("zig", switch (language.*) {
                .string => |s| s,
                else => return error.TestUnexpectedResult,
            });

            switch (versions.*) {
                .sequence => |seq| {
                    try testing.expectEqual(@as(usize, 2), seq.items.len);
                },
                else => return error.TestUnexpectedResult,
            }

            switch (features.*) {
                .mapping => |nested| {
                    try testing.expectEqual(@as(usize, 2), nested.items.len);
                },
                else => return error.TestUnexpectedResult,
            }
        },
        else => return error.TestUnexpectedResult,
    }
}

test "core scalar resolution keeps quoted values as strings" {
    const source =
        \\plain_bool: true
        \\plain_null: null
        \\plain_int: 42
        \\quoted_hex: "0xFFEEBB"
        \\quoted_true: 'true'
    ;

    var doc = try parseDoc(source);
    defer doc.deinit();

    const map = switch (doc.root) {
        .mapping => |m| m,
        else => return error.TestUnexpectedResult,
    };

    const plain_bool = findMapValue(map.items, "plain_bool") orelse return error.TestUnexpectedResult;
    const plain_null = findMapValue(map.items, "plain_null") orelse return error.TestUnexpectedResult;
    const plain_int = findMapValue(map.items, "plain_int") orelse return error.TestUnexpectedResult;
    const quoted_hex = findMapValue(map.items, "quoted_hex") orelse return error.TestUnexpectedResult;
    const quoted_true = findMapValue(map.items, "quoted_true") orelse return error.TestUnexpectedResult;

    try testing.expect(switch (plain_bool.*) { .bool => true, else => false });
    try testing.expect(switch (plain_null.*) { .null => true, else => false });
    try testing.expectEqual(@as(i64, 42), switch (plain_int.*) {
        .int => |v| v,
        else => return error.TestUnexpectedResult,
    });
    try testing.expectEqualStrings("0xFFEEBB", switch (quoted_hex.*) {
        .string => |v| v,
        else => return error.TestUnexpectedResult,
    });
    try testing.expectEqualStrings("true", switch (quoted_true.*) {
        .string => |v| v,
        else => return error.TestUnexpectedResult,
    });
}

test "stringify then parse preserves node shape" {
    const source =
        \\project:
        \\  name: weld
        \\  modules:
        \\    - yaml
        \\    - json
    ;

    var doc = try parseDoc(source);
    defer doc.deinit();

    const rendered = try yaml.stringifyDocument(testing.allocator, &doc, .{});
    defer testing.allocator.free(rendered);

    var reparsed = try parseDoc(rendered);
    defer reparsed.deinit();

    const root = switch (reparsed.root) {
        .mapping => |m| m,
        else => return error.TestUnexpectedResult,
    };
    const project = findMapValue(root.items, "project") orelse return error.TestUnexpectedResult;
    switch (project.*) {
        .mapping => |inner| {
            const modules = findMapValue(inner.items, "modules") orelse return error.TestUnexpectedResult;
            try testing.expect(switch (modules.*) { .sequence => true, else => false });
        },
        else => return error.TestUnexpectedResult,
    }
}
