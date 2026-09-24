//! Compare parse throughput of the pure Zig YAML module against vendored libfyaml.
//!
//! Always runs deterministic synthetic workloads. Extra file paths on the command
//! line are also timed (used in CI with prometheus-operator `bundle.yaml`).
const std = @import("std");
const Io = std.Io;

const yaml = @import("yaml");
const fy = @import("fy");

const Backend = enum { yaml, fy };

const Stats = struct {
    median_ns: u64,
    min_ns: u64,
    mb_per_s: f64,
};

const Workload = struct {
    name: []const u8,
    source: []const u8,
    multi: bool = false,
};

pub fn main(init: std.process.Init) !void {
    const gpa = init.gpa;
    const io = init.io;
    const arena = init.arena.allocator();

    var stdout_buf: [4096]u8 = undefined;
    var stdout_writer = Io.File.stdout().writer(io, &stdout_buf);
    const stdout = &stdout_writer.interface;

    var summary: std.ArrayListUnmanaged(u8) = .empty;
    defer summary.deinit(gpa);

    const warmup: usize = 3;
    const runs: usize = 9;

    try writeHeader(gpa, &summary, stdout, builtinMode());

    // Synthetic workloads sized near the published libfyaml reference docs.
    const small = try generateDoc(arena, .{ .entries = 500, .fields = 4, .multi_docs = 1 });
    const medium = try generateDoc(arena, .{ .entries = 4_000, .fields = 6, .multi_docs = 1 });
    const large = try generateDoc(arena, .{ .entries = 12_000, .fields = 8, .multi_docs = 1 });
    const multi = try generateDoc(arena, .{ .entries = 800, .fields = 5, .multi_docs = 24 });

    const synthetics = [_]Workload{
        .{ .name = "synthetic-64kib", .source = small },
        .{ .name = "synthetic-1mib", .source = medium },
        .{ .name = "synthetic-4mib", .source = large },
        .{ .name = "synthetic-multi-24", .source = multi, .multi = true },
    };

    for (synthetics) |wl| {
        try benchWorkload(gpa, io, &summary, stdout, wl, warmup, runs);
    }

    var argv = try init.minimal.args.iterateAllocator(arena);
    defer argv.deinit();
    _ = argv.next(); // argv[0]
    while (argv.next()) |path| {
        const source = try readFile(gpa, io, path);
        defer gpa.free(source);
        const name = std.fs.path.basename(path);
        try benchWorkload(gpa, io, &summary, stdout, .{
            .name = name,
            .source = source,
            .multi = std.mem.indexOf(u8, source, "\n---") != null,
        }, warmup, runs);
    }

    try stdout.flush();

    if (init.environ_map.get("GITHUB_STEP_SUMMARY")) |summary_path| {
        try appendFile(io, summary_path, summary.items);
    }
}

fn builtinMode() []const u8 {
    return switch (@import("builtin").mode) {
        .Debug => "Debug",
        .ReleaseSafe => "ReleaseSafe",
        .ReleaseFast => "ReleaseFast",
        .ReleaseSmall => "ReleaseSmall",
    };
}

fn writeHeader(
    gpa: std.mem.Allocator,
    summary: *std.ArrayListUnmanaged(u8),
    stdout: *Io.Writer,
    mode: []const u8,
) !void {
    const line = try std.fmt.allocPrint(gpa, "## Parse benchmark (`{s}`)\n\n", .{mode});
    defer gpa.free(line);
    try summary.appendSlice(gpa, line);
    try stdout.writeAll(line);

    const table_hdr =
        \\| Workload | Size | Backend | Median | Min | Throughput |
        \\| --- | ---: | --- | ---: | ---: | ---: |
        \\
    ;
    try summary.appendSlice(gpa, table_hdr);
    try stdout.writeAll(
        \\Workload                  Size       Backend   Median        Min      Throughput
        \\------------------------- ---------- --------- ------------ ---------- ------------
        \\
    );
}

fn benchWorkload(
    gpa: std.mem.Allocator,
    io: Io,
    summary: *std.ArrayListUnmanaged(u8),
    stdout: *Io.Writer,
    wl: Workload,
    warmup: usize,
    runs: usize,
) !void {
    const yaml_stats = try measure(gpa, io, wl, .yaml, warmup, runs);
    const fy_stats = try measure(gpa, io, wl, .fy, warmup, runs);

    try printRow(gpa, summary, stdout, wl, .yaml, yaml_stats);
    try printRow(gpa, summary, stdout, wl, .fy, fy_stats);
}

fn printRow(
    gpa: std.mem.Allocator,
    summary: *std.ArrayListUnmanaged(u8),
    stdout: *Io.Writer,
    wl: Workload,
    backend: Backend,
    stats: Stats,
) !void {
    const backend_name: []const u8 = switch (backend) {
        .yaml => "yaml",
        .fy => "fy",
    };
    const size_label = try formatSize(gpa, wl.source.len);
    defer gpa.free(size_label);
    const med = try formatDuration(gpa, stats.median_ns);
    defer gpa.free(med);
    const min = try formatDuration(gpa, stats.min_ns);
    defer gpa.free(min);

    try stdout.print("{s:<25} {s:>10} {s:<9} {s:>12} {s:>10} {d:>8.1} MB/s\n", .{
        wl.name,
        size_label,
        backend_name,
        med,
        min,
        stats.mb_per_s,
    });
    try summary.print(gpa, "| `{s}` | {s} | `{s}` | {s} | {s} | {d:.1} MB/s |\n", .{
        wl.name,
        size_label,
        backend_name,
        med,
        min,
        stats.mb_per_s,
    });
}

fn measure(
    gpa: std.mem.Allocator,
    io: Io,
    wl: Workload,
    backend: Backend,
    warmup: usize,
    runs: usize,
) !Stats {
    var i: usize = 0;
    while (i < warmup) : (i += 1) {
        try parseOnce(gpa, wl, backend);
    }

    var samples: std.ArrayListUnmanaged(u64) = .empty;
    defer samples.deinit(gpa);
    try samples.ensureTotalCapacity(gpa, runs);

    i = 0;
    while (i < runs) : (i += 1) {
        const start = Io.Clock.Timestamp.now(io, .awake);
        try parseOnce(gpa, wl, backend);
        const end = Io.Clock.Timestamp.now(io, .awake);
        const ns: u64 = @intCast(start.durationTo(end).raw.toNanoseconds());
        samples.appendAssumeCapacity(ns);
    }

    std.sort.heap(u64, samples.items, {}, std.sort.asc(u64));
    const median_ns = samples.items[samples.items.len / 2];
    const min_ns = samples.items[0];
    const seconds = @as(f64, @floatFromInt(median_ns)) / @as(f64, @floatFromInt(std.time.ns_per_s));
    const megabytes = @as(f64, @floatFromInt(wl.source.len)) / (1024.0 * 1024.0);
    const mb_per_s = if (seconds > 0) megabytes / seconds else 0;

    return .{
        .median_ns = median_ns,
        .min_ns = min_ns,
        .mb_per_s = mb_per_s,
    };
}

fn parseOnce(gpa: std.mem.Allocator, wl: Workload, backend: Backend) !void {
    switch (backend) {
        .yaml => {
            if (wl.multi) {
                const docs = try yaml.parseStream(gpa, wl.source, .{});
                defer {
                    for (docs) |*doc| doc.deinit();
                    gpa.free(docs);
                }
            } else {
                var doc = try yaml.parseDocument(gpa, wl.source, .{});
                defer doc.deinit();
            }
        },
        .fy => {
            if (wl.multi) {
                try fy.parseStreamDiscard(wl.source);
            } else {
                var doc = try fy.parseDocument(wl.source);
                defer doc.deinit();
            }
        },
    }
}

const GenerateOpts = struct {
    entries: usize,
    fields: usize,
    multi_docs: usize,
};

fn generateDoc(allocator: std.mem.Allocator, opts: GenerateOpts) ![]u8 {
    var out: std.ArrayListUnmanaged(u8) = .empty;
    errdefer out.deinit(allocator);

    var doc_i: usize = 0;
    while (doc_i < opts.multi_docs) : (doc_i += 1) {
        if (doc_i > 0) try out.appendSlice(allocator, "---\n");
        try out.print(allocator, "document: {d}\nitems:\n", .{doc_i});
        var entry: usize = 0;
        while (entry < opts.entries) : (entry += 1) {
            try out.print(allocator, "  - id: {d}\n    name: item-{d}-{d}\n", .{ entry, doc_i, entry });
            var field: usize = 0;
            while (field < opts.fields) : (field += 1) {
                try out.print(allocator, "    field_{d}: value-{d}-{d}-{d}\n", .{ field, doc_i, entry, field });
            }
            try out.appendSlice(allocator,
                \\    tags:
                \\      - alpha
                \\      - beta
                \\    nested:
                \\      enabled: true
                \\      count: 3
                \\
            );
        }
    }
    return out.toOwnedSlice(allocator);
}

fn formatSize(gpa: std.mem.Allocator, bytes: usize) ![]u8 {
    const kb = @as(f64, @floatFromInt(bytes)) / 1024.0;
    if (kb < 1024.0) return std.fmt.allocPrint(gpa, "{d:.1} KiB", .{kb});
    return std.fmt.allocPrint(gpa, "{d:.2} MiB", .{kb / 1024.0});
}

fn formatDuration(gpa: std.mem.Allocator, ns: u64) ![]u8 {
    if (ns >= std.time.ns_per_s) {
        return std.fmt.allocPrint(gpa, "{d:.2} s", .{@as(f64, @floatFromInt(ns)) / @as(f64, @floatFromInt(std.time.ns_per_s))});
    }
    if (ns >= std.time.ns_per_ms) {
        return std.fmt.allocPrint(gpa, "{d:.2} ms", .{@as(f64, @floatFromInt(ns)) / @as(f64, @floatFromInt(std.time.ns_per_ms))});
    }
    if (ns >= std.time.ns_per_us) {
        return std.fmt.allocPrint(gpa, "{d:.1} us", .{@as(f64, @floatFromInt(ns)) / @as(f64, @floatFromInt(std.time.ns_per_us))});
    }
    return std.fmt.allocPrint(gpa, "{d} ns", .{ns});
}

fn readFile(gpa: std.mem.Allocator, io: Io, path: []const u8) ![]u8 {
    return Io.Dir.cwd().readFileAlloc(io, path, gpa, .limited(64 * 1024 * 1024));
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
