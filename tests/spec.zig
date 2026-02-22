const std = @import("std");
const testing = std.testing;
const yaml = @import("yaml");

const smoke_ids = [_][]const u8{
    "TE2A",
    "PBJ2",
    "MXS3",
    "Q88A",
    "SYW4",
    "JQ4R",
};

test "yaml-test-suite smoke fixtures semantic match" {
    for (smoke_ids) |id| {
        try runFixtureSemanticCheck(id);
    }
}

fn runFixtureSemanticCheck(id: []const u8) !void {
    const in_yaml_path = try std.fmt.allocPrint(testing.allocator, "tests/fixtures/{s}/in.yaml", .{id});
    defer testing.allocator.free(in_yaml_path);
    const in_json_path = try std.fmt.allocPrint(testing.allocator, "tests/fixtures/{s}/in.json", .{id});
    defer testing.allocator.free(in_json_path);

    const in_yaml = try readFileAlloc(testing.allocator, in_yaml_path);
    defer testing.allocator.free(in_yaml);
    const in_json = try readFileAlloc(testing.allocator, in_json_path);
    defer testing.allocator.free(in_json);

    var doc = try yaml.parseDocument(testing.allocator, in_yaml, .{});
    defer doc.deinit();

    var parsed_json = try std.json.parseFromSlice(std.json.Value, testing.allocator, in_json, .{});
    defer parsed_json.deinit();

    try expectNodeMatchesJson(&doc.root, parsed_json.value);
}

fn readFileAlloc(allocator: std.mem.Allocator, path: []const u8) ![]u8 {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();
    return file.readToEndAlloc(allocator, std.math.maxInt(u32));
}

fn expectNodeMatchesJson(node: *const yaml.Node.Node, value: std.json.Value) !void {
    switch (value) {
        .null => try testing.expect(switch (node.*) { .null => true, else => false }),
        .bool => |v| try testing.expectEqual(v, switch (node.*) {
            .bool => |n| n,
            else => return error.TestUnexpectedResult,
        }),
        .integer => |v| try testing.expectEqual(v, switch (node.*) {
            .int => |n| n,
            else => return error.TestUnexpectedResult,
        }),
        .float => |v| {
            const actual = switch (node.*) {
                .float => |n| n,
                else => return error.TestUnexpectedResult,
            };
            try testing.expectApproxEqAbs(v, actual, 1e-9);
        },
        .number_string => |v| {
            if (std.fmt.parseInt(i64, v, 10)) |as_int| {
                try testing.expectEqual(as_int, switch (node.*) {
                    .int => |n| n,
                    else => return error.TestUnexpectedResult,
                });
            } else |_| {
                const as_float = try std.fmt.parseFloat(f64, v);
                const actual = switch (node.*) {
                    .float => |n| n,
                    else => return error.TestUnexpectedResult,
                };
                try testing.expectApproxEqAbs(as_float, actual, 1e-9);
            }
        },
        .string => |v| try testing.expectEqualStrings(v, switch (node.*) {
            .string => |n| n,
            else => return error.TestUnexpectedResult,
        }),
        .array => |arr| {
            const seq = switch (node.*) {
                .sequence => |s| s,
                else => return error.TestUnexpectedResult,
            };
            try testing.expectEqual(arr.items.len, seq.items.len);
            for (arr.items, 0..) |child, i| {
                try expectNodeMatchesJson(&seq.items[i], child);
            }
        },
        .object => |obj| {
            const map = switch (node.*) {
                .mapping => |m| m,
                else => return error.TestUnexpectedResult,
            };
            try testing.expectEqual(obj.count(), map.items.len);
            var it = obj.iterator();
            while (it.next()) |entry| {
                const map_value = findMapValue(map.items, entry.key_ptr.*) orelse return error.TestUnexpectedResult;
                try expectNodeMatchesJson(map_value, entry.value_ptr.*);
            }
        },
    }
}

fn findMapValue(map: []const yaml.Node.MapEntry, key: []const u8) ?*const yaml.Node.Node {
    for (map) |*entry| {
        if (std.mem.eql(u8, entry.key, key)) return &entry.value;
    }
    return null;
}
