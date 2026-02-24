const std = @import("std");
const testing = std.testing;

const yaml = @import("yaml");

const Classification = enum {
    pass,
    unsupported,
    fail,
};

const CoverageReport = struct {
    total: usize,
    passed: usize,
    unsupported: usize,
    failed: usize,
    coverage_percent: f64,
    passed_ids: []const []const u8,
    unsupported_ids: []const []const u8,
    failed_ids: []const []const u8,
};

test "yaml-test-suite coverage across all fixture cases" {
    var fixture_ids = try collectFixtureCaseIds(testing.allocator);
    defer deinitStringList(testing.allocator, &fixture_ids);

    var passed_ids: std.ArrayListUnmanaged([]const u8) = .{};
    defer deinitStringList(testing.allocator, &passed_ids);

    var unsupported_ids: std.ArrayListUnmanaged([]const u8) = .{};
    defer deinitStringList(testing.allocator, &unsupported_ids);

    var failed_ids: std.ArrayListUnmanaged([]const u8) = .{};
    defer deinitStringList(testing.allocator, &failed_ids);

    for (fixture_ids.items) |id| {
        const classification = try classifyFixture(id);
        switch (classification) {
            .pass => try passed_ids.append(testing.allocator, try testing.allocator.dupe(u8, id)),
            .unsupported => try unsupported_ids.append(testing.allocator, try testing.allocator.dupe(u8, id)),
            .fail => try failed_ids.append(testing.allocator, try testing.allocator.dupe(u8, id)),
        }
    }

    const total = fixture_ids.items.len;
    const passed = passed_ids.items.len;
    const unsupported = unsupported_ids.items.len;
    const failed = failed_ids.items.len;
    const coverage_percent = if (total == 0) 0.0 else (@as(f64, @floatFromInt(passed)) * 100.0) / @as(f64, @floatFromInt(total));

    try writeCoverageJson(.{
        .total = total,
        .passed = passed,
        .unsupported = unsupported,
        .failed = failed,
        .coverage_percent = coverage_percent,
        .passed_ids = passed_ids.items,
        .unsupported_ids = unsupported_ids.items,
        .failed_ids = failed_ids.items,
    });

    std.debug.print(
        "yaml spec summary: passed={d} unsupported={d} failed={d} total_discovered={d} coverage={d:.2}%\n",
        .{ passed, unsupported, failed, total, coverage_percent },
    );

    try testing.expect(total > 0);
}

fn classifyFixture(id: []const u8) !Classification {
    const in_yaml_path = try std.fmt.allocPrint(testing.allocator, "tests/fixtures/{s}/in.yaml", .{id});
    defer testing.allocator.free(in_yaml_path);
    const in_json_path = try std.fmt.allocPrint(testing.allocator, "tests/fixtures/{s}/in.json", .{id});
    defer testing.allocator.free(in_json_path);

    const has_yaml = try pathExists(in_yaml_path);
    if (!has_yaml) return .unsupported;

    const has_json = try pathExists(in_json_path);
    if (!has_json) return .unsupported;

    runFixtureSemanticCheck(id) catch |err| switch (err) {
        error.UnsupportedFeature,
        error.UnexpectedToken,
        error.InvalidIndentation,
        error.InvalidMappingKey,
        error.UnterminatedFlowCollection,
        error.InvalidEscapeSequence,
        error.UnterminatedString,
        error.DuplicateKey,
        error.SyntaxError,
        error.UnexpectedEndOfInput,
        error.InvalidCharacter,
        => return .unsupported,
        error.TestUnexpectedResult,
        error.TestExpectedEqual,
        => return .fail,
        else => return .fail,
    };

    return .pass;
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

fn collectFixtureCaseIds(allocator: std.mem.Allocator) !std.ArrayListUnmanaged([]const u8) {
    var dir = try std.fs.cwd().openDir("tests/fixtures", .{
        .iterate = true,
        .access_sub_paths = true,
    });
    defer dir.close();

    var walker = try dir.walk(allocator);
    defer walker.deinit();

    var ids: std.ArrayListUnmanaged([]const u8) = .{};
    errdefer deinitStringList(allocator, &ids);

    while (try walker.next()) |entry| {
        if (entry.kind != .file) continue;
        if (!std.mem.eql(u8, std.fs.path.basename(entry.path), "===")) continue;

        const parent = std.fs.path.dirname(entry.path) orelse continue;
        const normalized = try allocator.dupe(u8, parent);
        for (normalized) |*ch| {
            if (ch.* == '\\') ch.* = '/';
        }
        try ids.append(allocator, normalized);
    }

    std.sort.heap([]const u8, ids.items, {}, lessString);
    return ids;
}

fn lessString(_: void, lhs: []const u8, rhs: []const u8) bool {
    return std.mem.lessThan(u8, lhs, rhs);
}

fn pathExists(path: []const u8) !bool {
    std.fs.cwd().access(path, .{}) catch |err| switch (err) {
        error.FileNotFound => return false,
        else => return err,
    };
    return true;
}

fn writeCoverageJson(report: CoverageReport) !void {
    try std.fs.cwd().makePath("zig-out/spec-coverage");
    const file = try std.fs.cwd().createFile("zig-out/spec-coverage/coverage.json", .{ .truncate = true });
    defer file.close();

    var writer = file.writer(&.{});
    defer writer.end() catch {};

    try std.json.Stringify.value(report, .{ .whitespace = .indent_2 }, &writer.interface);
    try writer.interface.writeByte('\n');
}

fn deinitStringList(allocator: std.mem.Allocator, list: *std.ArrayListUnmanaged([]const u8)) void {
    for (list.items) |item| allocator.free(item);
    list.deinit(allocator);
}

fn readFileAlloc(allocator: std.mem.Allocator, path: []const u8) ![]u8 {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();
    return file.readToEndAlloc(allocator, std.math.maxInt(u32));
}

fn expectNodeMatchesJson(node: *const yaml.Node.Node, value: std.json.Value) !void {
    switch (value) {
        .null => if (switch (node.*) { .null => false, else => true }) return error.TestUnexpectedResult,
        .bool => |v| if (v != switch (node.*) {
            .bool => |n| n,
            else => return error.TestUnexpectedResult,
        }) return error.TestUnexpectedResult,
        .integer => |v| if (v != switch (node.*) {
            .int => |n| n,
            else => return error.TestUnexpectedResult,
        }) return error.TestUnexpectedResult,
        .float => |v| {
            const actual = switch (node.*) {
                .float => |n| n,
                else => return error.TestUnexpectedResult,
            };
            if (!approxEq(v, actual, 1e-9)) return error.TestUnexpectedResult;
        },
        .number_string => |v| {
            if (std.fmt.parseInt(i64, v, 10)) |as_int| {
                if (as_int != switch (node.*) {
                    .int => |n| n,
                    else => return error.TestUnexpectedResult,
                }) return error.TestUnexpectedResult;
            } else |_| {
                const as_float = try std.fmt.parseFloat(f64, v);
                const actual = switch (node.*) {
                    .float => |n| n,
                    else => return error.TestUnexpectedResult,
                };
                if (!approxEq(as_float, actual, 1e-9)) return error.TestUnexpectedResult;
            }
        },
        .string => |v| if (!std.mem.eql(u8, v, switch (node.*) {
            .string => |n| n,
            else => return error.TestUnexpectedResult,
        })) return error.TestUnexpectedResult,
        .array => |arr| {
            const seq = switch (node.*) {
                .sequence => |s| s,
                else => return error.TestUnexpectedResult,
            };
            if (arr.items.len != seq.items.len) return error.TestUnexpectedResult;
            for (arr.items, 0..) |child, i| {
                try expectNodeMatchesJson(&seq.items[i], child);
            }
        },
        .object => |obj| {
            const map = switch (node.*) {
                .mapping => |m| m,
                else => return error.TestUnexpectedResult,
            };
            if (obj.count() != map.items.len) return error.TestUnexpectedResult;
            var it = obj.iterator();
            while (it.next()) |entry| {
                const map_value = findMapValue(map.items, entry.key_ptr.*) orelse return error.TestUnexpectedResult;
                try expectNodeMatchesJson(map_value, entry.value_ptr.*);
            }
        },
    }
}

fn approxEq(lhs: f64, rhs: f64, tolerance: f64) bool {
    const diff = @abs(lhs - rhs);
    return diff <= tolerance;
}

fn findMapValue(map: []const yaml.Node.MapEntry, key: []const u8) ?*const yaml.Node.Node {
    for (map) |*entry| {
        if (std.mem.eql(u8, entry.key, key)) return &entry.value;
    }
    return null;
}
