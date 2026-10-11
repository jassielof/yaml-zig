//! Shared YAML benchmark workloads and input generation.
const std = @import("std");
const Io = std.Io;

pub const Workload = struct {
    name: []const u8,
    source: []const u8,
    multi: bool = false,
    /// Skip libfyaml when the shape hits known fy limits (e.g. deep nesting).
    yaml_only: bool = false,
};

pub const GenerateOpts = struct {
    entries: usize,
    fields: usize,
    multi_docs: usize,
};

pub fn generateDoc(allocator: std.mem.Allocator, opts: GenerateOpts) ![]u8 {
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

pub fn generateDeep(allocator: std.mem.Allocator, depth: usize) ![]u8 {
    var out: std.ArrayListUnmanaged(u8) = .empty;
    errdefer out.deinit(allocator);
    var i: usize = 0;
    while (i < depth) : (i += 1) {
        var j: usize = 0;
        while (j < i) : (j += 1) try out.appendSlice(allocator, "  ");
        try out.print(allocator, "n{d}:\n", .{i});
    }
    var j: usize = 0;
    while (j < depth) : (j += 1) try out.appendSlice(allocator, "  ");
    try out.appendSlice(allocator, "leaf: true\n");
    return out.toOwnedSlice(allocator);
}

pub fn generateBlockHeavy(allocator: std.mem.Allocator, blocks: usize) ![]u8 {
    var out: std.ArrayListUnmanaged(u8) = .empty;
    errdefer out.deinit(allocator);
    try out.appendSlice(allocator, "entries:\n");
    var i: usize = 0;
    while (i < blocks) : (i += 1) {
        try out.print(allocator,
            \\  - id: {d}
            \\    note: |
            \\      line-a-{d} with some prose that is long enough to matter
            \\      line-b-{d} and another line for the literal block
            \\      line-c-{d} trailing content
            \\
        , .{ i, i, i, i });
    }
    return out.toOwnedSlice(allocator);
}

pub fn readFile(gpa: std.mem.Allocator, io: Io, path: []const u8) ![]u8 {
    return Io.Dir.cwd().readFileAlloc(io, path, gpa, .limited(64 * 1024 * 1024));
}

/// Mutable registry populated by `sierraPrepare` before benchmarks run.
pub var workloads: []Workload = &.{};
pub var arena_storage: ?std.heap.ArenaAllocator = null;

pub fn prepareSynthetics(arena: std.mem.Allocator) ![]Workload {
    const tiny = try generateDoc(arena, .{ .entries = 2, .fields = 2, .multi_docs = 1 });
    const small = try generateDoc(arena, .{ .entries = 500, .fields = 4, .multi_docs = 1 });
    const medium = try generateDoc(arena, .{ .entries = 4_000, .fields = 6, .multi_docs = 1 });
    const large = try generateDoc(arena, .{ .entries = 12_000, .fields = 8, .multi_docs = 1 });
    const multi = try generateDoc(arena, .{ .entries = 800, .fields = 5, .multi_docs = 24 });
    const deep = try generateDeep(arena, 64);
    const blocky = try generateBlockHeavy(arena, 200);

    var list: std.ArrayList(Workload) = .empty;
    try list.appendSlice(arena, &.{
        .{ .name = "synthetic-tiny", .source = tiny },
        .{ .name = "synthetic-64kib", .source = small },
        .{ .name = "synthetic-1mib", .source = medium },
        .{ .name = "synthetic-4mib", .source = large },
        .{ .name = "synthetic-multi-24", .source = multi, .multi = true },
        .{ .name = "synthetic-deep-64", .source = deep, .yaml_only = true },
        .{ .name = "synthetic-block-200", .source = blocky },
    });
    return list.items;
}
