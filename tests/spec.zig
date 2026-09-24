const std = @import("std");
const testing = std.testing;
const build_options = @import("build_options");

const fy = @import("fy");
const yaml = @import("yaml");

const Backend = enum { yaml, fy };

const Classification = enum { pass, unsupported, fail };

const FixtureOutcome = struct {
    id: []const u8,
    class: Classification,
    err_name: []const u8 = "",
};

const CoverageReport = struct {
    backend: []const u8,
    total: usize,
    passed: usize,
    unsupported: usize,
    failed: usize,
    coverage_percent: f64,
    passed_ids: []const []const u8,
    unsupported_ids: []const []const u8,
    failed_ids: []const []const u8,
    failures: []const FailureDetail,
    unsupported_groups: []const ReasonGroup,
};

const FailureDetail = struct {
    id: []const u8,
    detail: []const u8,
};

const ReasonGroup = struct {
    reason: []const u8,
    count: usize,
    ids: []const []const u8,
};

test "yaml-test-suite coverage across all fixture cases" {
    try runSpecCoverage(.yaml);
}

test "yaml-test-suite coverage across all fixture cases via fy" {
    try runSpecCoverage(.fy);
}

fn runSpecCoverage(backend: Backend) !void {
    const summary_only: bool = build_options.summary_only;

    var fixture_ids = try collectFixtureCaseIds(testing.allocator);
    defer deinitStringList(testing.allocator, &fixture_ids);

    var outcomes: std.ArrayListUnmanaged(FixtureOutcome) = .empty;
    defer {
        for (outcomes.items) |o| testing.allocator.free(o.id);
        outcomes.deinit(testing.allocator);
    }

    for (fixture_ids.items) |id| {
        try outcomes.append(testing.allocator, classifyFixture(backend, id));
    }

    var passed_ids: std.ArrayListUnmanaged([]const u8) = .empty;
    defer passed_ids.deinit(testing.allocator);
    var unsupported_ids: std.ArrayListUnmanaged([]const u8) = .empty;
    defer unsupported_ids.deinit(testing.allocator);
    var failed_ids: std.ArrayListUnmanaged([]const u8) = .empty;
    defer failed_ids.deinit(testing.allocator);

    for (outcomes.items) |o| {
        switch (o.class) {
            .pass => try passed_ids.append(testing.allocator, o.id),
            .unsupported => try unsupported_ids.append(testing.allocator, o.id),
            .fail => try failed_ids.append(testing.allocator, o.id),
        }
    }

    const total = fixture_ids.items.len;
    const passed = passed_ids.items.len;
    const unsupported = unsupported_ids.items.len;
    const failed = failed_ids.items.len;
    const coverage_percent = if (total == 0) 0.0 else (@as(f64, @floatFromInt(passed)) * 100.0) / @as(f64, @floatFromInt(total));

    var groups = try collectReasonGroups(testing.allocator, outcomes.items);
    defer {
        for (groups.items) |group| testing.allocator.free(group.ids);
        groups.deinit(testing.allocator);
    }

    var failure_details: std.ArrayListUnmanaged(FailureDetail) = .empty;
    defer {
        for (failure_details.items) |detail| testing.allocator.free(detail.detail);
        failure_details.deinit(testing.allocator);
    }
    for (outcomes.items) |outcome| {
        if (outcome.class != .fail) continue;
        const diag = diagnoseFixture(backend, outcome.id);
        const detail = if (diag.allocated) diag.text else try testing.allocator.dupe(u8, diag.text);
        try failure_details.append(testing.allocator, .{ .id = outcome.id, .detail = detail });
    }

    try writeCoverageJson(backend, .{
        .backend = backendName(backend),
        .total = total,
        .passed = passed,
        .unsupported = unsupported,
        .failed = failed,
        .coverage_percent = coverage_percent,
        .passed_ids = passed_ids.items,
        .unsupported_ids = unsupported_ids.items,
        .failed_ids = failed_ids.items,
        .failures = failure_details.items,
        .unsupported_groups = groups.items,
    });

    printCoverageSummary(backend, total, passed, failed, unsupported, coverage_percent, failure_details.items, groups.items, summary_only);

    try testing.expect(total > 0);
    // Hard gate: both backends must stay at full yaml-test-suite coverage.
    try testing.expectEqual(total, passed);
    try testing.expectEqual(@as(usize, 0), unsupported);
    try testing.expectEqual(@as(usize, 0), failed);
}

// ---------------------------------------------------------------------------
// Classification
// ---------------------------------------------------------------------------

fn classifyFixture(backend: Backend, id: []const u8) FixtureOutcome {
    const owned_id = testing.allocator.dupe(u8, id) catch return .{ .id = "", .class = .fail, .err_name = "OutOfMemory" };

    const in_yaml_path = std.fmt.allocPrint(testing.allocator, "tests/fixtures/{s}/in.yaml", .{id}) catch
        return .{ .id = owned_id, .class = .fail, .err_name = "OutOfMemory" };
    defer testing.allocator.free(in_yaml_path);

    const in_json_path = std.fmt.allocPrint(testing.allocator, "tests/fixtures/{s}/in.json", .{id}) catch
        return .{ .id = owned_id, .class = .fail, .err_name = "OutOfMemory" };
    defer testing.allocator.free(in_json_path);
    const test_event_path = std.fmt.allocPrint(testing.allocator, "tests/fixtures/{s}/test.event", .{id}) catch
        return .{ .id = owned_id, .class = .fail, .err_name = "OutOfMemory" };
    defer testing.allocator.free(test_event_path);
    const error_path = std.fmt.allocPrint(testing.allocator, "tests/fixtures/{s}/error", .{id}) catch
        return .{ .id = owned_id, .class = .fail, .err_name = "OutOfMemory" };
    defer testing.allocator.free(error_path);

    const has_yaml = pathExists(in_yaml_path) catch
        return .{ .id = owned_id, .class = .unsupported, .err_name = "FileAccessError" };
    if (!has_yaml) return .{ .id = owned_id, .class = .unsupported, .err_name = "no in.yaml" };

    const has_error = pathExists(error_path) catch
        return .{ .id = owned_id, .class = .unsupported, .err_name = "FileAccessError" };
    const has_json = pathExists(in_json_path) catch
        return .{ .id = owned_id, .class = .unsupported, .err_name = "FileAccessError" };
    const has_events = pathExists(test_event_path) catch
        return .{ .id = owned_id, .class = .unsupported, .err_name = "FileAccessError" };

    const has_expected = switch (backend) {
        .yaml => has_error or has_json or has_events,
        .fy => if (has_error) true else has_events,
    };
    if (!has_expected) return .{ .id = owned_id, .class = .unsupported, .err_name = if (backend == .yaml) "no oracle" else "no test.event" };

    runFixtureSemanticCheck(backend, id) catch |err| {
        const tag = @errorName(err);
        if (backend == .fy) return .{ .id = owned_id, .class = .fail, .err_name = tag };

        return switch (err) {
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
            => .{ .id = owned_id, .class = .unsupported, .err_name = tag },
            else => .{ .id = owned_id, .class = .fail, .err_name = tag },
        };
    };

    return .{ .id = owned_id, .class = .pass };
}

fn runFixtureSemanticCheck(backend: Backend, id: []const u8) !void {
    const in_yaml_path = try std.fmt.allocPrint(testing.allocator, "tests/fixtures/{s}/in.yaml", .{id});
    defer testing.allocator.free(in_yaml_path);

    const in_yaml = try readFileAlloc(testing.allocator, in_yaml_path);
    defer testing.allocator.free(in_yaml);

    switch (backend) {
        .yaml => {
            const error_path = try std.fmt.allocPrint(testing.allocator, "tests/fixtures/{s}/error", .{id});
            defer testing.allocator.free(error_path);
            if (try pathExists(error_path)) {
                const documents = yaml.parseStream(testing.allocator, in_yaml, .{}) catch return;
                defer {
                    for (documents) |*document| document.deinit();
                    testing.allocator.free(documents);
                }
                return error.TestUnexpectedResult;
            }

            const in_json_path = try std.fmt.allocPrint(testing.allocator, "tests/fixtures/{s}/in.json", .{id});
            defer testing.allocator.free(in_json_path);
            if (!try pathExists(in_json_path)) {
                const event_path = try std.fmt.allocPrint(testing.allocator, "tests/fixtures/{s}/test.event", .{id});
                defer testing.allocator.free(event_path);
                const expected_events = try readFileAlloc(testing.allocator, event_path);
                defer testing.allocator.free(expected_events);
                const actual_events = try yaml.formatTestsuiteEvents(testing.allocator, in_yaml);
                defer testing.allocator.free(actual_events);
                const normalized_expected = try stripCarriageReturnsAlloc(testing.allocator, expected_events);
                defer testing.allocator.free(normalized_expected);
                const normalized_actual = try stripCarriageReturnsAlloc(testing.allocator, actual_events);
                defer testing.allocator.free(normalized_actual);
                if (!std.mem.eql(u8, normalized_actual, normalized_expected)) return error.TestUnexpectedResult;
                return;
            }
            const in_json = try readFileAlloc(testing.allocator, in_json_path);
            defer testing.allocator.free(in_json);

            const docs = try yaml.parseStream(testing.allocator, in_yaml, .{});
            defer {
                for (docs) |*doc| doc.deinit();
                testing.allocator.free(docs);
            }

            var parsed_json = try parseJsonValues(testing.allocator, in_json);
            defer {
                for (parsed_json.items) |*parsed| parsed.deinit();
                parsed_json.deinit(testing.allocator);
            }

            if (docs.len != parsed_json.items.len) return error.TestUnexpectedResult;
            for (docs, parsed_json.items) |*doc, parsed| {
                try expectNodeMatchesJson(&doc.root, parsed.value);
            }
        },
        .fy => {
            const error_path = try std.fmt.allocPrint(testing.allocator, "tests/fixtures/{s}/error", .{id});
            defer testing.allocator.free(error_path);
            const expect_error = try pathExists(error_path);

            const result = try fy.parseTestsuiteEventsDetailedAlloc(testing.allocator, in_yaml);
            defer testing.allocator.free(result.events);

            if (expect_error) {
                if (!result.had_stream_error) return error.TestUnexpectedResult;
                return;
            }

            if (result.had_stream_error) return error.ParseFailed;

            const expected_event_path = try std.fmt.allocPrint(testing.allocator, "tests/fixtures/{s}/test.event", .{id});
            defer testing.allocator.free(expected_event_path);
            const expected_events = try readFileAlloc(testing.allocator, expected_event_path);
            defer testing.allocator.free(expected_events);

            const normalized_expected = try stripCarriageReturnsAlloc(testing.allocator, expected_events);
            defer testing.allocator.free(normalized_expected);
            const normalized_actual = try stripCarriageReturnsAlloc(testing.allocator, result.events);
            defer testing.allocator.free(normalized_actual);

            if (!std.mem.eql(u8, normalized_actual, normalized_expected)) return error.TestUnexpectedResult;
        },
    }
}

fn printCoverageSummary(
    backend: Backend,
    total: usize,
    passed: usize,
    failed: usize,
    unsupported: usize,
    coverage_percent: f64,
    failures: []const FailureDetail,
    groups: []const ReasonGroup,
    summary_only: bool,
) void {
    var bar: [24]u8 = undefined;
    std.debug.print("\n{s} spec summary\n", .{backendName(backend)});
    std.debug.print("  {s}  {d:.2}%\n", .{ coverageBar(passed, total, &bar), coverage_percent });
    std.debug.print("  passed {d:>4}   failed {d:>4}   unsupported {d:>4}   discovered {d}\n", .{
        passed, failed, unsupported, total,
    });

    if (failures.len > 0) {
        std.debug.print("  failures\n", .{});
        for (failures) |failure| {
            std.debug.print("    {s:<16} {s}\n", .{ failure.id, failure.detail });
        }
    }

    if (groups.len > 0) {
        std.debug.print("  unsupported\n", .{});
        for (groups) |group| {
            std.debug.print("    {s:<24} {d:>4}", .{ group.reason, group.count });
            if (!summary_only and group.ids.len <= 8) {
                std.debug.print("  ", .{});
                for (group.ids, 0..) |id, i| {
                    if (i > 0) std.debug.print(", ", .{});
                    std.debug.print("{s}", .{id});
                }
            }
            std.debug.print("\n", .{});
        }
    }
}

fn coverageBar(passed: usize, total: usize, bar: *[24]u8) []const u8 {
    @memset(bar, '-');
    if (total == 0) return bar;
    const filled = (passed * bar.len) / total;
    @memset(bar[0..filled], '#');
    return bar;
}

fn collectReasonGroups(
    allocator: std.mem.Allocator,
    outcomes: []const FixtureOutcome,
) !std.ArrayListUnmanaged(ReasonGroup) {
    var map: std.StringHashMapUnmanaged(std.ArrayListUnmanaged([]const u8)) = .empty;
    defer {
        var it = map.valueIterator();
        while (it.next()) |list| list.deinit(allocator);
        map.deinit(allocator);
    }

    for (outcomes) |outcome| {
        if (outcome.class != .unsupported) continue;
        const result = try map.getOrPut(allocator, outcome.err_name);
        if (!result.found_existing) result.value_ptr.* = .empty;
        try result.value_ptr.append(allocator, outcome.id);
    }

    var groups: std.ArrayListUnmanaged(ReasonGroup) = .empty;
    errdefer {
        for (groups.items) |group| allocator.free(group.ids);
        groups.deinit(allocator);
    }

    var it = map.iterator();
    while (it.next()) |entry| {
        const ids = try allocator.dupe([]const u8, entry.value_ptr.items);
        try groups.append(allocator, .{
            .reason = entry.key_ptr.*,
            .count = ids.len,
            .ids = ids,
        });
    }

    std.sort.heap(ReasonGroup, groups.items, {}, struct {
        fn less(_: void, lhs: ReasonGroup, rhs: ReasonGroup) bool {
            if (lhs.count != rhs.count) return lhs.count > rhs.count;
            return std.mem.lessThan(u8, lhs.reason, rhs.reason);
        }
    }.less);

    return groups;
}

const DiagResult = struct { text: []const u8, allocated: bool };

fn diagnoseFixture(backend: Backend, id: []const u8) DiagResult {
    const yaml_path = std.fmt.allocPrint(testing.allocator, "tests/fixtures/{s}/in.yaml", .{id}) catch
        return .{ .text = "?", .allocated = false };
    defer testing.allocator.free(yaml_path);

    const yaml_src = readFileAlloc(testing.allocator, yaml_path) catch return .{ .text = "cannot read yaml", .allocated = false };
    defer testing.allocator.free(yaml_src);

    switch (backend) {
        .yaml => {
            const error_path = std.fmt.allocPrint(testing.allocator, "tests/fixtures/{s}/error", .{id}) catch
                return .{ .text = "?", .allocated = false };
            defer testing.allocator.free(error_path);
            if (pathExists(error_path) catch false) {
                const documents = yaml.parseStream(testing.allocator, yaml_src, .{}) catch
                    return .{ .text = "rejected invalid input", .allocated = false };
                defer {
                    for (documents) |*document| document.deinit();
                    testing.allocator.free(documents);
                }
                return .{ .text = "accepted a document the suite marks invalid", .allocated = false };
            }

            const json_path = std.fmt.allocPrint(testing.allocator, "tests/fixtures/{s}/in.json", .{id}) catch
                return .{ .text = "?", .allocated = false };
            defer testing.allocator.free(json_path);
            if (!(pathExists(json_path) catch false)) {
                const event_path = std.fmt.allocPrint(testing.allocator, "tests/fixtures/{s}/test.event", .{id}) catch
                    return .{ .text = "cannot read test.event", .allocated = false };
                defer testing.allocator.free(event_path);
                const expected_events = readFileAlloc(testing.allocator, event_path) catch
                    return .{ .text = "cannot read test.event", .allocated = false };
                defer testing.allocator.free(expected_events);
                const actual_events = yaml.formatTestsuiteEvents(testing.allocator, yaml_src) catch |e|
                    return .{ .text = @errorName(e), .allocated = false };
                defer testing.allocator.free(actual_events);
                const normalized_expected = stripCarriageReturnsAlloc(testing.allocator, expected_events) catch
                    return .{ .text = "OutOfMemory", .allocated = false };
                defer testing.allocator.free(normalized_expected);
                const normalized_actual = stripCarriageReturnsAlloc(testing.allocator, actual_events) catch
                    return .{ .text = "OutOfMemory", .allocated = false };
                defer testing.allocator.free(normalized_actual);
                return describeEventOutputMismatch(normalized_actual, normalized_expected);
            }
            const json_src = readFileAlloc(testing.allocator, json_path) catch return .{ .text = "cannot read json", .allocated = false };
            defer testing.allocator.free(json_src);

            const docs = yaml.parseStream(testing.allocator, yaml_src, .{}) catch |e|
                return .{ .text = @errorName(e), .allocated = false };
            defer {
                for (docs) |*doc| doc.deinit();
                testing.allocator.free(docs);
            }

            var parsed = parseJsonValues(testing.allocator, json_src) catch
                return .{ .text = "invalid expected json", .allocated = false };
            defer {
                for (parsed.items) |*item| item.deinit();
                parsed.deinit(testing.allocator);
            }

            if (docs.len != parsed.items.len) {
                const text = std.fmt.allocPrint(testing.allocator, "expected {d} documents, got {d}", .{ parsed.items.len, docs.len }) catch
                    return .{ .text = "document count mismatch", .allocated = false };
                return .{ .text = text, .allocated = true };
            }
            for (docs, parsed.items) |*doc, item| {
                const mismatch = describeMismatch(&doc.root, item.value);
                if (!std.mem.eql(u8, mismatch.text, "ok")) return mismatch;
            }
            return .{ .text = "ok", .allocated = false };
        },
        .fy => {
            const error_path = std.fmt.allocPrint(testing.allocator, "tests/fixtures/{s}/error", .{id}) catch
                return .{ .text = "cannot read error marker", .allocated = false };
            defer testing.allocator.free(error_path);
            const expect_error = pathExists(error_path) catch false;

            const expected_event_path = std.fmt.allocPrint(testing.allocator, "tests/fixtures/{s}/test.event", .{id}) catch
                return .{ .text = "cannot read test.event", .allocated = false };
            defer testing.allocator.free(expected_event_path);
            const actual = fy.parseTestsuiteEventsDetailedAlloc(testing.allocator, yaml_src) catch |e|
                return .{ .text = @errorName(e), .allocated = false };
            defer testing.allocator.free(actual.events);

            if (expect_error) {
                return .{ .text = if (actual.had_stream_error) "ok" else "expected parse failure", .allocated = false };
            }

            const expected_events = readFileAlloc(testing.allocator, expected_event_path) catch
                return .{ .text = "cannot read test.event", .allocated = false };
            defer testing.allocator.free(expected_events);

            const normalized_expected = stripCarriageReturnsAlloc(testing.allocator, expected_events) catch
                return .{ .text = "OutOfMemory", .allocated = false };
            defer testing.allocator.free(normalized_expected);
            const normalized_actual = stripCarriageReturnsAlloc(testing.allocator, actual.events) catch
                return .{ .text = "OutOfMemory", .allocated = false };
            defer testing.allocator.free(normalized_actual);

            return describeEventOutputMismatch(normalized_actual, normalized_expected);
        },
    }
}

fn describeEventOutputMismatch(actual: []const u8, expected: []const u8) DiagResult {
    if (std.mem.eql(u8, actual, expected)) return .{ .text = "ok", .allocated = false };

    var expected_lines = std.mem.splitScalar(u8, expected, '\n');
    var actual_lines = std.mem.splitScalar(u8, actual, '\n');
    var line_no: usize = 1;

    while (true) {
        const expected_line = expected_lines.next();
        const actual_line = actual_lines.next();

        if (expected_line == null and actual_line == null) break;
        if (expected_line == null or actual_line == null or !std.mem.eql(u8, expected_line.?, actual_line.?)) {
            const text = std.fmt.allocPrint(
                testing.allocator,
                "line {d}: expected \"{s}\", got \"{s}\"",
                .{ line_no, previewLine(expected_line orelse "<eof>"), previewLine(actual_line orelse "<eof>") },
            ) catch return .{ .text = "event output mismatch", .allocated = false };
            return .{ .text = text, .allocated = true };
        }

        line_no += 1;
    }

    return .{ .text = "event output mismatch", .allocated = false };
}

fn describeJsonMismatch(actual: std.json.Value, expected: std.json.Value) DiagResult {
    if (coerceJsonNumber(actual)) |actual_number| {
        if (coerceJsonNumber(expected)) |expected_number| {
            if (jsonNumbersEqual(actual_number, expected_number)) return .{ .text = "ok", .allocated = false };

            const text = std.fmt.allocPrint(testing.allocator, "expected {s}, got {s}", .{ jsonValuePreview(expected), jsonValuePreview(actual) }) catch
                return .{ .text = "numeric value mismatch", .allocated = false };
            return .{ .text = text, .allocated = true };
        }
    }

    const got = jsonTypeName(actual);
    const exp = jsonTypeName(expected);
    if (!std.mem.eql(u8, got, exp)) {
        const text = std.fmt.allocPrint(testing.allocator, "expected {s}, got {s}", .{ exp, got }) catch
            return .{ .text = "type mismatch", .allocated = false };
        return .{ .text = text, .allocated = true };
    }

    switch (expected) {
        .null => return .{ .text = "ok", .allocated = false },
        .bool => |v| {
            const actual_bool = switch (actual) {
                .bool => |n| n,
                else => return .{ .text = "type mismatch (bool)", .allocated = false },
            };
            if (v == actual_bool) return .{ .text = "ok", .allocated = false };
            return .{ .text = "bool value mismatch", .allocated = false };
        },
        .string => |s| {
            const actual_string = switch (actual) {
                .string => |n| n,
                else => return .{ .text = "type mismatch (string)", .allocated = false },
            };
            if (std.mem.eql(u8, s, actual_string)) return .{ .text = "ok", .allocated = false };
            const text = std.fmt.allocPrint(testing.allocator, "expected \"{s}\", got \"{s}\"", .{ previewString(s), previewString(actual_string) }) catch
                return .{ .text = "string value mismatch", .allocated = false };
            return .{ .text = text, .allocated = true };
        },
        .array => |expected_array| {
            const actual_array = switch (actual) {
                .array => |n| n,
                else => return .{ .text = "type mismatch (sequence)", .allocated = false },
            };
            if (expected_array.items.len != actual_array.items.len) {
                const text = std.fmt.allocPrint(testing.allocator, "sequence: expected {d} items, got {d}", .{ expected_array.items.len, actual_array.items.len }) catch
                    return .{ .text = "sequence size mismatch", .allocated = false };
                return .{ .text = text, .allocated = true };
            }
            for (expected_array.items, 0..) |child, i| {
                const child_diag = describeJsonMismatch(actual_array.items[i], child);
                if (!std.mem.eql(u8, child_diag.text, "ok")) {
                    const text = std.fmt.allocPrint(testing.allocator, "[{d}]: {s}", .{ i, child_diag.text }) catch {
                        return child_diag;
                    };
                    if (child_diag.allocated) testing.allocator.free(child_diag.text);
                    return .{ .text = text, .allocated = true };
                }
                if (child_diag.allocated) testing.allocator.free(child_diag.text);
            }
            return .{ .text = "ok", .allocated = false };
        },
        .object => |expected_object| {
            const actual_object = switch (actual) {
                .object => |n| n,
                else => return .{ .text = "type mismatch (mapping)", .allocated = false },
            };
            if (expected_object.count() != actual_object.count()) {
                const text = std.fmt.allocPrint(testing.allocator, "mapping: expected {d} keys, got {d}", .{ expected_object.count(), actual_object.count() }) catch
                    return .{ .text = "mapping size mismatch", .allocated = false };
                return .{ .text = text, .allocated = true };
            }
            var it = expected_object.iterator();
            while (it.next()) |entry| {
                const actual_value = actual_object.get(entry.key_ptr.*) orelse {
                    const text = std.fmt.allocPrint(testing.allocator, "mapping: missing key \"{s}\"", .{entry.key_ptr.*}) catch
                        return .{ .text = "mapping: missing key", .allocated = false };
                    return .{ .text = text, .allocated = true };
                };
                const child_diag = describeJsonMismatch(actual_value, entry.value_ptr.*);
                if (!std.mem.eql(u8, child_diag.text, "ok")) {
                    const text = std.fmt.allocPrint(testing.allocator, ".{s}: {s}", .{ entry.key_ptr.*, child_diag.text }) catch {
                        return child_diag;
                    };
                    if (child_diag.allocated) testing.allocator.free(child_diag.text);
                    return .{ .text = text, .allocated = true };
                }
                if (child_diag.allocated) testing.allocator.free(child_diag.text);
            }
            return .{ .text = "ok", .allocated = false };
        },
        .integer, .float, .number_string => unreachable,
    }
}

fn describeMismatch(node: *const yaml.Node.Node, json_val: std.json.Value) DiagResult {
    const got = nodeTypeName(node);
    const exp = jsonTypeName(json_val);

    if (!std.mem.eql(u8, got, exp)) {
        const text = std.fmt.allocPrint(testing.allocator, "expected {s}, got {s}", .{ exp, got }) catch
            return .{ .text = "type mismatch", .allocated = false };
        return .{ .text = text, .allocated = true };
    }

    switch (json_val) {
        .string => |s| {
            const actual = switch (node.*) {
                .string => |n| n,
                else => return .{ .text = "type mismatch (string)", .allocated = false },
            };
            if (std.mem.eql(u8, s, actual)) return .{ .text = "ok", .allocated = false };
            const s_prev = if (s.len > 32) s[0..32] else s;
            const a_prev = if (actual.len > 32) actual[0..32] else actual;
            const text = std.fmt.allocPrint(testing.allocator, "expected \"{s}\", got \"{s}\"", .{ s_prev, a_prev }) catch
                return .{ .text = "string value mismatch", .allocated = false };
            return .{ .text = text, .allocated = true };
        },
        .object => |obj| {
            const map = switch (node.*) {
                .mapping => |m| m,
                else => return .{ .text = "type mismatch (mapping)", .allocated = false },
            };
            if (obj.count() != map.items.len) {
                const text = std.fmt.allocPrint(testing.allocator, "mapping: expected {d} keys, got {d}", .{ obj.count(), map.items.len }) catch
                    return .{ .text = "mapping size mismatch", .allocated = false };
                return .{ .text = text, .allocated = true };
            }
            var it = obj.iterator();
            while (it.next()) |entry| {
                const map_val = findMapValue(map.items, entry.key_ptr.*);
                if (map_val == null) {
                    const text = std.fmt.allocPrint(testing.allocator, "mapping: missing key \"{s}\"", .{entry.key_ptr.*}) catch
                        return .{ .text = "mapping: missing key", .allocated = false };
                    return .{ .text = text, .allocated = true };
                }
                const child_diag = describeMismatch(map_val.?, entry.value_ptr.*);
                if (!std.mem.eql(u8, child_diag.text, "ok")) {
                    const text = std.fmt.allocPrint(testing.allocator, ".{s}: {s}", .{ entry.key_ptr.*, child_diag.text }) catch {
                        return child_diag;
                    };
                    if (child_diag.allocated) testing.allocator.free(child_diag.text);
                    return .{ .text = text, .allocated = true };
                }
                if (child_diag.allocated) testing.allocator.free(child_diag.text);
            }
            return .{ .text = "ok", .allocated = false };
        },
        .array => |arr| {
            const seq = switch (node.*) {
                .sequence => |s| s,
                else => return .{ .text = "type mismatch (sequence)", .allocated = false },
            };
            if (arr.items.len != seq.items.len) {
                const text = std.fmt.allocPrint(testing.allocator, "sequence: expected {d} items, got {d}", .{ arr.items.len, seq.items.len }) catch
                    return .{ .text = "sequence size mismatch", .allocated = false };
                return .{ .text = text, .allocated = true };
            }
            for (arr.items, 0..) |child, i| {
                const child_diag = describeMismatch(&seq.items[i], child);
                if (!std.mem.eql(u8, child_diag.text, "ok")) {
                    const text = std.fmt.allocPrint(testing.allocator, "[{d}]: {s}", .{ i, child_diag.text }) catch {
                        return child_diag;
                    };
                    if (child_diag.allocated) testing.allocator.free(child_diag.text);
                    return .{ .text = text, .allocated = true };
                }
                if (child_diag.allocated) testing.allocator.free(child_diag.text);
            }
            return .{ .text = "ok", .allocated = false };
        },
        .integer => |v| {
            const actual = switch (node.*) {
                .int => |n| n,
                else => return .{ .text = "type mismatch (int)", .allocated = false },
            };
            if (v != actual) {
                const text = std.fmt.allocPrint(testing.allocator, "int: expected {d}, got {d}", .{ v, actual }) catch
                    return .{ .text = "int value mismatch", .allocated = false };
                return .{ .text = text, .allocated = true };
            }
        },
        .bool => |v| {
            const actual = switch (node.*) {
                .bool => |n| n,
                else => return .{ .text = "type mismatch (bool)", .allocated = false },
            };
            if (v != actual) {
                return .{ .text = "bool value mismatch", .allocated = false };
            }
        },
        .null => {
            if (node.* != .null) return .{ .text = "expected null", .allocated = false };
        },
        .float => |v| {
            const actual = switch (node.*) {
                .float => |n| n,
                else => return .{ .text = "type mismatch (float)", .allocated = false },
            };
            if (!approxEq(v, actual, 1e-9)) {
                return .{ .text = "float value mismatch", .allocated = false };
            }
        },
        .number_string => return .{ .text = "number_string comparison", .allocated = false },
    }

    return .{ .text = "ok", .allocated = false };
}

fn nodeTypeName(node: *const yaml.Node.Node) []const u8 {
    return switch (node.*) {
        .null => "null",
        .bool => "bool",
        .int => "int",
        .float => "float",
        .string => "string",
        .sequence => "sequence",
        .mapping => "mapping",
    };
}

fn jsonTypeName(val: std.json.Value) []const u8 {
    return switch (val) {
        .null => "null",
        .bool => "bool",
        .integer => "int",
        .float => "float",
        .number_string => "number",
        .string => "string",
        .array => "sequence",
        .object => "mapping",
    };
}

const JsonNumber = union(enum) {
    int: i64,
    float: f64,
};

fn coerceJsonNumber(val: std.json.Value) ?JsonNumber {
    return switch (val) {
        .integer => |v| .{ .int = v },
        .float => |v| .{ .float = v },
        .number_string => |v| parseJsonNumber(v) catch null,
        else => null,
    };
}

fn parseJsonNumber(text: []const u8) !JsonNumber {
    if (std.fmt.parseInt(i64, text, 10)) |value| {
        return .{ .int = value };
    } else |_| {
        return .{ .float = try std.fmt.parseFloat(f64, text) };
    }
}

fn jsonNumbersEqual(lhs: JsonNumber, rhs: JsonNumber) bool {
    return switch (lhs) {
        .int => |lhs_int| switch (rhs) {
            .int => |rhs_int| lhs_int == rhs_int,
            .float => |rhs_float| approxEq(@as(f64, @floatFromInt(lhs_int)), rhs_float, 1e-9),
        },
        .float => |lhs_float| switch (rhs) {
            .int => |rhs_int| approxEq(lhs_float, @as(f64, @floatFromInt(rhs_int)), 1e-9),
            .float => |rhs_float| approxEq(lhs_float, rhs_float, 1e-9),
        },
    };
}

fn jsonValuePreview(val: std.json.Value) []const u8 {
    return switch (val) {
        .null => "null",
        .bool => |v| if (v) "true" else "false",
        .integer => |v| std.fmt.allocPrint(testing.allocator, "{d}", .{v}) catch "<int>",
        .float => |v| std.fmt.allocPrint(testing.allocator, "{d}", .{v}) catch "<float>",
        .number_string => |v| v,
        .string => |v| previewString(v),
        .array => "<array>",
        .object => "<object>",
    };
}

fn previewString(value: []const u8) []const u8 {
    return if (value.len > 32) value[0..32] else value;
}

fn previewLine(value: []const u8) []const u8 {
    return if (value.len > 64) value[0..64] else value;
}

fn backendName(backend: Backend) []const u8 {
    return switch (backend) {
        .yaml => "yaml",
        .fy => "fy",
    };
}

// Helpers

fn collectFixtureCaseIds(allocator: std.mem.Allocator) !std.ArrayListUnmanaged([]const u8) {
    var io_impl: std.Io.Threaded = .init(allocator, .{});
    defer io_impl.deinit();

    const io = io_impl.io();
    const cwd = std.Io.Dir.cwd();
    var dir = try cwd.openDir(
        io,
        "tests/fixtures",
        .{
            .iterate = true,
            .access_sub_paths = true,
        },
    );
    defer dir.close(io);

    var walker = try dir.walk(allocator);
    defer walker.deinit();

    var ids: std.ArrayListUnmanaged([]const u8) = .empty;
    errdefer deinitStringList(allocator, &ids);

    while (try walker.next(io)) |entry| {
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
    var io_impl: std.Io.Threaded = .init(testing.allocator, .{});
    defer io_impl.deinit();

    const io = io_impl.io();
    const cwd = std.Io.Dir.cwd();
    cwd.access(io, path, .{}) catch |err| switch (err) {
        error.FileNotFound => return false,
        else => return err,
    };
    return true;
}

fn writeCoverageJson(backend: Backend, report: CoverageReport) !void {
    var io_impl: std.Io.Threaded = .init(testing.allocator, .{});
    defer io_impl.deinit();

    const io = io_impl.io();
    const cwd = std.Io.Dir.cwd();
    try cwd.createDirPath(io, "zig-out/spec-coverage");
    const path = switch (backend) {
        .yaml => "zig-out/spec-coverage/coverage.json",
        .fy => "zig-out/spec-coverage/coverage-fy.json",
    };
    const file = try cwd.createFile(io, path, .{ .truncate = true });
    defer file.close(io);

    var writer_buffer: [4096]u8 = undefined;
    var writer = file.writer(io, &writer_buffer);
    defer writer.end() catch {};

    try std.json.Stringify.value(report, .{ .whitespace = .indent_2 }, &writer.interface);
    try writer.interface.writeByte('\n');
}

fn expectJsonMatchesJson(actual: std.json.Value, expected: std.json.Value) !void {
    if (coerceJsonNumber(actual)) |actual_number| {
        if (coerceJsonNumber(expected)) |expected_number| {
            if (!jsonNumbersEqual(actual_number, expected_number)) return error.TestUnexpectedResult;
            return;
        }
    }

    switch (expected) {
        .null => if (actual != .null) return error.TestUnexpectedResult,
        .bool => |v| if (v != switch (actual) {
            .bool => |n| n,
            else => return error.TestUnexpectedResult,
        }) return error.TestUnexpectedResult,
        .string => |v| if (!std.mem.eql(u8, v, switch (actual) {
            .string => |n| n,
            else => return error.TestUnexpectedResult,
        })) return error.TestUnexpectedResult,
        .array => |expected_array| {
            const actual_array = switch (actual) {
                .array => |n| n,
                else => return error.TestUnexpectedResult,
            };
            if (expected_array.items.len != actual_array.items.len) return error.TestUnexpectedResult;
            for (expected_array.items, 0..) |child, i| {
                try expectJsonMatchesJson(actual_array.items[i], child);
            }
        },
        .object => |expected_object| {
            const actual_object = switch (actual) {
                .object => |n| n,
                else => return error.TestUnexpectedResult,
            };
            if (expected_object.count() != actual_object.count()) return error.TestUnexpectedResult;
            var it = expected_object.iterator();
            while (it.next()) |entry| {
                const actual_value = actual_object.get(entry.key_ptr.*) orelse return error.TestUnexpectedResult;
                try expectJsonMatchesJson(actual_value, entry.value_ptr.*);
            }
        },
        .integer, .float, .number_string => unreachable,
    }
}

fn deinitStringList(allocator: std.mem.Allocator, list: *std.ArrayListUnmanaged([]const u8)) void {
    for (list.items) |item| allocator.free(item);
    list.deinit(allocator);
}

fn readFileAlloc(allocator: std.mem.Allocator, path: []const u8) ![]u8 {
    var io_impl: std.Io.Threaded = .init(allocator, .{});
    defer io_impl.deinit();

    const io = io_impl.io();
    const cwd = std.Io.Dir.cwd();
    return cwd.readFileAlloc(io, path, allocator, .limited(std.math.maxInt(u32)));
}

fn stripCarriageReturnsAlloc(allocator: std.mem.Allocator, input: []const u8) ![]u8 {
    var output: std.ArrayListUnmanaged(u8) = .empty;
    errdefer output.deinit(allocator);

    for (input) |ch| {
        if (ch == '\r') continue;
        try output.append(allocator, ch);
    }

    return output.toOwnedSlice(allocator);
}

fn parseJsonValues(allocator: std.mem.Allocator, src: []const u8) !std.ArrayListUnmanaged(std.json.Parsed(std.json.Value)) {
    var values: std.ArrayListUnmanaged(std.json.Parsed(std.json.Value)) = .empty;
    errdefer {
        for (values.items) |*parsed| parsed.deinit();
        values.deinit(allocator);
    }

    var i: usize = 0;
    while (i < src.len) {
        while (i < src.len and (src[i] == ' ' or src[i] == '\t' or src[i] == '\n' or src[i] == '\r')) : (i += 1) {}
        if (i >= src.len) break;
        const end = try jsonValueEnd(src, i);
        const parsed = try std.json.parseFromSlice(std.json.Value, allocator, src[i..end], .{});
        try values.append(allocator, parsed);
        i = end;
    }
    return values;
}

fn jsonValueEnd(src: []const u8, start: usize) !usize {
    if (start >= src.len) return error.SyntaxError;
    switch (src[start]) {
        '"' => {
            var i = start + 1;
            var escaped = false;
            while (i < src.len) : (i += 1) {
                if (escaped) {
                    escaped = false;
                    continue;
                }
                if (src[i] == '\\') {
                    escaped = true;
                    continue;
                }
                if (src[i] == '"') return i + 1;
            }
            return error.SyntaxError;
        },
        '{', '[' => {
            var depth: usize = 0;
            var in_string = false;
            var escaped = false;
            var i = start;
            while (i < src.len) : (i += 1) {
                const ch = src[i];
                if (in_string) {
                    if (escaped) {
                        escaped = false;
                        continue;
                    }
                    if (ch == '\\') {
                        escaped = true;
                        continue;
                    }
                    if (ch == '"') in_string = false;
                    continue;
                }
                switch (ch) {
                    '"' => in_string = true,
                    '{', '[' => depth += 1,
                    '}', ']' => {
                        if (depth == 0) return error.SyntaxError;
                        depth -= 1;
                        if (depth == 0) return i + 1;
                    },
                    else => {},
                }
            }
            return error.SyntaxError;
        },
        else => {
            var i = start;
            while (i < src.len) : (i += 1) {
                switch (src[i]) {
                    ' ', '\t', '\n', '\r' => break,
                    else => {},
                }
            }
            if (i == start) return error.SyntaxError;
            return i;
        },
    }
}

fn expectNodeMatchesJson(node: *const yaml.Node.Node, value: std.json.Value) !void {
    switch (value) {
        .null => if (switch (node.*) {
            .null => false,
            else => true,
        }) return error.TestUnexpectedResult,
        .bool => |v| if (v != switch (node.*) {
            .bool => |n| n,
            else => return error.TestUnexpectedResult,
        }) return error.TestUnexpectedResult,
        .integer => |v| switch (node.*) {
            .int => |n| if (v != n) return error.TestUnexpectedResult,
            .float => |n| if (!approxEq(@as(f64, @floatFromInt(v)), n, 1e-9)) return error.TestUnexpectedResult,
            else => return error.TestUnexpectedResult,
        },
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
