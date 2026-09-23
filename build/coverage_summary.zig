//! Render YAML spec coverage JSON into a GitHub Actions job summary (or stdout).
const std = @import("std");
const Io = std.Io;

const CoverageReport = struct {
    backend: []const u8 = "",
    total: usize = 0,
    passed: usize = 0,
    unsupported: usize = 0,
    failed: usize = 0,
    coverage_percent: f64 = 0,
    passed_ids: []const []const u8 = &.{},
    unsupported_ids: []const []const u8 = &.{},
    failed_ids: []const []const u8 = &.{},
    failures: []const FailureDetail = &.{},
    unsupported_groups: []const ReasonGroup = &.{},
};

const FailureDetail = struct {
    id: []const u8 = "",
    detail: []const u8 = "",
};

const ReasonGroup = struct {
    reason: []const u8 = "",
    count: usize = 0,
    ids: []const []const u8 = &.{},
};

pub fn main(init: std.process.Init) !void {
    const gpa = init.gpa;
    const io = init.io;
    const arena = init.arena.allocator();

    const matrix_os = init.environ_map.get("MATRIX_OS") orelse "unknown-os";

    var out: std.ArrayListUnmanaged(u8) = .empty;
    defer out.deinit(gpa);
    try out.print(gpa, "## YAML spec coverage — `{s}`\n\n", .{matrix_os});

    try appendReport(gpa, &out, "Pure Zig (`yaml`)", try loadReport(gpa, io, arena, "zig-out/spec-coverage/coverage.json"));
    try appendReport(gpa, &out, "libfyaml (`fy`)", try loadReport(gpa, io, arena, "zig-out/spec-coverage/coverage-fy.json"));

    const text = out.items;
    if (init.environ_map.get("GITHUB_STEP_SUMMARY")) |summary_path| {
        try appendFile(io, summary_path, text);
    } else {
        try Io.File.stdout().writeStreamingAll(io, text);
    }
}

fn loadReport(gpa: std.mem.Allocator, io: Io, arena: std.mem.Allocator, path: []const u8) !?CoverageReport {
    const cwd = Io.Dir.cwd();
    const bytes = cwd.readFileAlloc(io, path, gpa, .limited(16 * 1024 * 1024)) catch |err| switch (err) {
        error.FileNotFound => return null,
        else => return err,
    };
    defer gpa.free(bytes);

    const parsed = try std.json.parseFromSlice(CoverageReport, arena, bytes, .{
        .ignore_unknown_fields = true,
        .allocate = .alloc_always,
    });
    return parsed.value;
}

fn appendReport(gpa: std.mem.Allocator, out: *std.ArrayListUnmanaged(u8), title: []const u8, report: ?CoverageReport) !void {
    try out.print(gpa, "### {s}\n\n", .{title});
    const r = report orelse {
        try out.appendSlice(gpa, "Coverage report was not generated.\n\n");
        return;
    };

    try out.print(gpa, "`{s}` **{d:.2}%** ({d}/{d})\n\n", .{
        coverageBar(r.passed, r.total),
        r.coverage_percent,
        r.passed,
        r.total,
    });
    try out.appendSlice(gpa, "| Passed | Failed | Unsupported | Discovered |\n");
    try out.appendSlice(gpa, "|-------:|-------:|------------:|-----------:|\n");
    try out.print(gpa, "| {d} | {d} | {d} | {d} |\n\n", .{ r.passed, r.failed, r.unsupported, r.total });

    if (r.failures.len > 0) {
        try out.appendSlice(gpa, "#### Failures\n\n| Case | Detail |\n| --- | --- |\n");
        for (r.failures) |failure| {
            const detail = try escapeTableCell(gpa, failure.detail);
            defer gpa.free(detail);
            try out.print(gpa, "| `{s}` | {s} |\n", .{ failure.id, detail });
        }
        try out.appendSlice(gpa, "\n");
    }

    if (r.unsupported_groups.len > 0) {
        try out.appendSlice(gpa,
            \\#### Gaps
            \\
            \\Cases with no JSON oracle are valid or event-only fixtures this harness does not score yet. Other rows are parser errors on documents that do have an `in.json`.
            \\
            \\| Reason | Count | Cases |
            \\| --- | ---: | --- |
            \\
        );
        for (r.unsupported_groups) |group| {
            const ids = try formatIdList(gpa, group.ids, 12);
            defer gpa.free(ids);
            try out.print(gpa, "| `{s}` | {d} | {s} |\n", .{ group.reason, group.count, ids });
        }
        try out.appendSlice(gpa, "\n");

        for (r.unsupported_groups) |group| {
            if (group.ids.len <= 12) continue;
            try out.print(gpa, "<details><summary>{s} ({d})</summary>\n\n", .{ group.reason, group.ids.len });
            for (group.ids, 0..) |id, i| {
                if (i > 0) try out.append(gpa, ' ');
                try out.print(gpa, "`{s}`", .{id});
            }
            try out.appendSlice(gpa, "\n\n</details>\n\n");
        }
    }
}

fn coverageBar(passed: usize, total: usize) []const u8 {
    const width: usize = 24;
    const Static = struct {
        var buf: [72]u8 = undefined;
    };
    if (total == 0) return "————————————————————————";
    const filled: usize = @min(width, (width * passed + total / 2) / total);
    const full = "████████████████████████";
    const empty = "░░░░░░░░░░░░░░░░░░░░░░░░";
    const full_bytes = filled * 3;
    const empty_bytes = (width - filled) * 3;
    @memcpy(Static.buf[0..full_bytes], full[0..full_bytes]);
    @memcpy(Static.buf[full_bytes .. full_bytes + empty_bytes], empty[0..empty_bytes]);
    return Static.buf[0 .. full_bytes + empty_bytes];
}

fn formatIdList(gpa: std.mem.Allocator, ids: []const []const u8, limit: usize) ![]u8 {
    if (ids.len == 0) return gpa.dupe(u8, "_none_");
    var out: std.ArrayListUnmanaged(u8) = .empty;
    errdefer out.deinit(gpa);
    const shown = @min(ids.len, limit);
    for (ids[0..shown], 0..) |id, i| {
        if (i > 0) try out.append(gpa, ' ');
        try out.print(gpa, "`{s}`", .{id});
    }
    if (ids.len > shown) try out.print(gpa, " +{d} more", .{ids.len - shown});
    return out.toOwnedSlice(gpa);
}

fn escapeTableCell(gpa: std.mem.Allocator, text: []const u8) ![]u8 {
    var out: std.ArrayListUnmanaged(u8) = .empty;
    errdefer out.deinit(gpa);
    for (text) |c| {
        switch (c) {
            '|' => try out.appendSlice(gpa, "\\|"),
            '\n', '\r' => try out.append(gpa, ' '),
            else => try out.append(gpa, c),
        }
    }
    return out.toOwnedSlice(gpa);
}

fn appendFile(io: Io, path: []const u8, text: []const u8) !void {
    const file = Io.Dir.openFileAbsolute(io, path, .{ .mode = .read_write }) catch |err| switch (err) {
        error.FileNotFound => try Io.Dir.createFileAbsolute(io, path, .{}),
        else => return err,
    };
    defer file.close(io);
    const end = try file.length(io);
    try file.writePositionalAll(io, text, end);
    if (text.len == 0 or text[text.len - 1] != '\n') {
        try file.writePositionalAll(io, "\n", end + text.len);
    }
}
