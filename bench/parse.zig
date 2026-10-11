//! YAML parse benchmarks via Sierra — yaml-zig vs libfyaml.
//!
//! Workloads compare parse-and-destroy for both backends. libfyaml allocation
//! metrics are reported as unavailable (C malloc), not zero.
const std = @import("std");
const sierra = @import("sierra");
const yaml = @import("yaml");
const fy = @import("fy");
const cases = @import("cases.zig");

var file_workloads: std.ArrayList(cases.Workload) = .empty;
var file_sources: std.ArrayList([]u8) = .empty;

/// Called by Sierra before running benchmarks.
pub fn sierraPrepare(init: std.process.Init, positionals: []const []const u8) !void {
    cases.arena_storage = std.heap.ArenaAllocator.init(init.gpa);
    const arena_alloc = cases.arena_storage.?.allocator();

    const synthetics = try cases.prepareSynthetics(arena_alloc);
    file_workloads = .empty;
    file_sources = .empty;

    try file_workloads.appendSlice(init.gpa, synthetics);

    for (positionals) |path| {
        const source = try cases.readFile(init.gpa, init.io, path);
        try file_sources.append(init.gpa, source);
        const name = std.fs.path.basename(path);
        try file_workloads.append(init.gpa, .{
            .name = try arena_alloc.dupe(u8, name),
            .source = source,
            .multi = std.mem.indexOf(u8, source, "\n---") != null,
        });
    }
    cases.workloads = file_workloads.items;
}

pub fn sierraFinish(init: std.process.Init, results: []const sierra.Result) !void {
    _ = results;
    for (file_sources.items) |src| init.gpa.free(src);
    file_sources.deinit(init.gpa);
    file_workloads.deinit(init.gpa);
    if (cases.arena_storage) |*arena| arena.deinit();
    cases.arena_storage = null;
    cases.workloads = &.{};
}

fn parseYaml(gpa: std.mem.Allocator, wl: cases.Workload) !void {
    if (wl.multi) {
        const docs = try yaml.parseStream(gpa, wl.source, .{});
        defer {
            for (docs) |*doc| doc.deinit();
            gpa.free(docs);
        }
        sierra.keepAlive(docs.len);
    } else {
        var doc = try yaml.parseDocument(gpa, wl.source, .{});
        defer doc.deinit();
        sierra.keepAlive(@intFromPtr(&doc));
    }
}

fn parseFy(wl: cases.Workload) !void {
    if (wl.multi) {
        try fy.parseStreamDiscard(wl.source);
    } else {
        var doc = try fy.parseDocument(wl.source);
        defer doc.deinit();
        sierra.keepAlive(@intFromPtr(doc.raw));
    }
}

fn runNamed(b: *sierra.Bench, backend: enum { yaml, fy }, wl: cases.Workload) !void {
    b.setBytes(wl.source.len);
    // Zig allocator tracking only for yaml-zig. libfyaml uses C malloc —
    // leave allocation metrics unavailable rather than reporting zeros.
    if (backend == .yaml) b.enableAllocationTracking();

    while (try b.next()) {
        switch (backend) {
            .yaml => try parseYaml(b.allocator, wl),
            .fy => try parseFy(wl),
        }
    }
}

fn findWorkload(name: []const u8) ?cases.Workload {
    for (cases.workloads) |wl| {
        if (std.mem.eql(u8, wl.name, name)) return wl;
    }
    return null;
}

fn requireWorkload(b: *sierra.Bench, name: []const u8) ?cases.Workload {
    return findWorkload(name) orelse {
        b.markSkipped();
        return null;
    };
}

pub fn bench_yaml_synthetic_tiny(b: *sierra.Bench) !void {
    const wl = requireWorkload(b, "synthetic-tiny") orelse return;
    try runNamed(b, .yaml, wl);
}
pub fn bench_fy_synthetic_tiny(b: *sierra.Bench) !void {
    const wl = requireWorkload(b, "synthetic-tiny") orelse return;
    try runNamed(b, .fy, wl);
}
pub fn bench_yaml_synthetic_64kib(b: *sierra.Bench) !void {
    const wl = requireWorkload(b, "synthetic-64kib") orelse return;
    try runNamed(b, .yaml, wl);
}
pub fn bench_fy_synthetic_64kib(b: *sierra.Bench) !void {
    const wl = requireWorkload(b, "synthetic-64kib") orelse return;
    try runNamed(b, .fy, wl);
}
pub fn bench_yaml_synthetic_1mib(b: *sierra.Bench) !void {
    const wl = requireWorkload(b, "synthetic-1mib") orelse return;
    try runNamed(b, .yaml, wl);
}
pub fn bench_fy_synthetic_1mib(b: *sierra.Bench) !void {
    const wl = requireWorkload(b, "synthetic-1mib") orelse return;
    try runNamed(b, .fy, wl);
}
pub fn bench_yaml_synthetic_4mib(b: *sierra.Bench) !void {
    const wl = requireWorkload(b, "synthetic-4mib") orelse return;
    try runNamed(b, .yaml, wl);
}
pub fn bench_fy_synthetic_4mib(b: *sierra.Bench) !void {
    const wl = requireWorkload(b, "synthetic-4mib") orelse return;
    try runNamed(b, .fy, wl);
}
pub fn bench_yaml_synthetic_multi_24(b: *sierra.Bench) !void {
    const wl = requireWorkload(b, "synthetic-multi-24") orelse return;
    try runNamed(b, .yaml, wl);
}
pub fn bench_fy_synthetic_multi_24(b: *sierra.Bench) !void {
    const wl = requireWorkload(b, "synthetic-multi-24") orelse return;
    try runNamed(b, .fy, wl);
}
pub fn bench_yaml_synthetic_deep_64(b: *sierra.Bench) !void {
    const wl = requireWorkload(b, "synthetic-deep-64") orelse return;
    try runNamed(b, .yaml, wl);
}
pub fn bench_yaml_synthetic_block_200(b: *sierra.Bench) !void {
    const wl = requireWorkload(b, "synthetic-block-200") orelse return;
    try runNamed(b, .yaml, wl);
}
pub fn bench_fy_synthetic_block_200(b: *sierra.Bench) !void {
    const wl = requireWorkload(b, "synthetic-block-200") orelse return;
    try runNamed(b, .fy, wl);
}

fn fileAt(index: usize) ?cases.Workload {
    const synthetic_count: usize = 7;
    const idx = synthetic_count + index;
    if (idx >= cases.workloads.len) return null;
    return cases.workloads[idx];
}

pub fn bench_yaml_file_0(b: *sierra.Bench) !void {
    const wl = fileAt(0) orelse {
        b.markSkipped();
        return;
    };
    try runNamed(b, .yaml, wl);
}
pub fn bench_fy_file_0(b: *sierra.Bench) !void {
    const wl = fileAt(0) orelse {
        b.markSkipped();
        return;
    };
    try runNamed(b, .fy, wl);
}

pub const benchmarks = .{
    .yaml_synthetic_tiny = bench_yaml_synthetic_tiny,
    .fy_synthetic_tiny = bench_fy_synthetic_tiny,
    .yaml_synthetic_64kib = bench_yaml_synthetic_64kib,
    .fy_synthetic_64kib = bench_fy_synthetic_64kib,
    .yaml_synthetic_1mib = bench_yaml_synthetic_1mib,
    .fy_synthetic_1mib = bench_fy_synthetic_1mib,
    .yaml_synthetic_4mib = bench_yaml_synthetic_4mib,
    .fy_synthetic_4mib = bench_fy_synthetic_4mib,
    .yaml_synthetic_multi_24 = bench_yaml_synthetic_multi_24,
    .fy_synthetic_multi_24 = bench_fy_synthetic_multi_24,
    .yaml_synthetic_deep_64 = bench_yaml_synthetic_deep_64,
    .yaml_synthetic_block_200 = bench_yaml_synthetic_block_200,
    .fy_synthetic_block_200 = bench_fy_synthetic_block_200,
    .yaml_file_0 = bench_yaml_file_0,
    .fy_file_0 = bench_fy_file_0,
};
