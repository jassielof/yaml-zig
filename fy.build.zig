const std = @import("std");

const include_paths = [_][]const u8{
    "modules/libfyaml/include",
    "modules/libfyaml/src/lib",
    "modules/libfyaml/src/util",
    "modules/libfyaml/src/xxhash",
    "modules/libfyaml/src/thread",
    "modules/libfyaml/src/allocator",
    "modules/libfyaml/src/blake3",
    "src/lib/fy_config",
};

const c_sources = [_][]const u8{
    "src/lib/fy-accel.c",
    "src/lib/fy-atom.c",
    "src/lib/fy-composer.c",
    "src/lib/fy-diag.c",
    "src/lib/fy-doc.c",
    "src/lib/fy-docbuilder.c",
    "src/lib/fy-docstate.c",
    "src/lib/fy-dump.c",
    "src/lib/fy-emit.c",
    "src/lib/fy-event.c",
    "src/lib/fy-input.c",
    "src/lib/fy-parse.c",
    "src/lib/fy-path.c",
    "src/lib/fy-token.c",
    "src/lib/fy-types.c",
    "src/lib/fy-walk.c",
    "src/util/fy-blob.c",
    "src/util/fy-ctype.c",
    "src/util/fy-utf8.c",
    "src/util/fy-utils.c",
    "src/xxhash/xxhash.c",
    "src/thread/fy-thread.c",
    "src/allocator/fy-allocator.c",
    "src/allocator/fy-allocator-linear.c",
    "src/allocator/fy-allocator-malloc.c",
    "src/allocator/fy-allocator-mremap.c",
    "src/allocator/fy-allocator-dedup.c",
    "src/allocator/fy-allocator-auto.c",
    "src/blake3/blake3_host_state.c",
    "src/blake3/blake3_backend.c",
    "src/blake3/blake3_be_cpusimd.c",
    "src/blake3/fy-blake3.c",
    "src/lib/fy-composer-diag.c",
    "src/lib/fy-doc-diag.c",
    "src/lib/fy-docbuilder-diag.c",
    "src/lib/fy-input-diag.c",
    "src/lib/fy-parse-diag.c",
};

const blake3_sources = [_][]const u8{
    "src/blake3/blake3_portable.c",
    "src/blake3/blake3.c",
};

pub const Options = struct {
    target: std.Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode,
    module_name: []const u8 = "fy",
};

pub const Dependency = struct {
    c_lib: *std.Build.Step.Compile,
    module: *std.Build.Module,

    pub fn link(self: Dependency, module: *std.Build.Module) void {
        module.linkLibrary(self.c_lib);
        module.addIncludePath(self.c_lib.step.owner.path("modules/libfyaml/include"));
        module.addIncludePath(self.c_lib.step.owner.path("src/lib/fy_config"));
    }
};

pub fn create(b: *std.Build, options: Options) Dependency {
    const c_lib = b.addLibrary(.{
        .name = "fyaml",
        .linkage = .static,
        .root_module = b.createModule(.{
            .target = options.target,
            .optimize = options.optimize,
            .link_libc = true,
        }),
    });
    c_lib.root_module.link_libc = true;
    addIncludePaths(c_lib.root_module, options.target);
    addCSourceFiles(b, c_lib, options.target);

    if (options.target.result.os.tag != .windows) {
        c_lib.root_module.linkSystemLibrary("pthread", .{});
    }
    switch (options.target.result.os.tag) {
        .linux, .freebsd, .netbsd, .openbsd, .dragonfly => c_lib.root_module.linkSystemLibrary("m", .{}),
        else => {},
    }

    const translate_c = b.addTranslateC(.{
        .root_source_file = b.path("src/lib/fy_c.h"),
        .target = options.target,
        .optimize = options.optimize,
    });
    addTranslateIncludePaths(translate_c, options.target);

    const module = b.addModule(
        options.module_name,
        .{
            .root_source_file = b.path("src/lib/fy.zig"),
            .target = options.target,
            .optimize = options.optimize,
            .link_libc = true,
            .imports = &.{
                .{
                    .name = "fy_c",
                    .module = translate_c.createModule(),
                },
            },
        },
    );

    return .{
        .c_lib = c_lib,
        .module = module,
    };
}

fn addIncludePaths(module: *std.Build.Module, target: std.Build.ResolvedTarget) void {
    const b = module.owner;
    inline for (include_paths) |path| {
        module.addIncludePath(b.path(path));
    }
    if (target.result.os.tag == .windows) {
        module.addIncludePath(b.path("src/lib/fy_windows"));
    }
}

fn addTranslateIncludePaths(translate_c: *std.Build.Step.TranslateC, target: std.Build.ResolvedTarget) void {
    const b = translate_c.step.owner;
    inline for (include_paths) |path| {
        translate_c.addIncludePath(b.path(path));
    }
    if (target.result.os.tag == .windows) {
        translate_c.addIncludePath(b.path("src/lib/fy_windows"));
    }
}

fn addCSourceFiles(b: *std.Build, c_lib: *std.Build.Step.Compile, target: std.Build.ResolvedTarget) void {
    const common_flags: []const []const u8 = if (target.result.os.tag == .windows)
        &.{ "-std=c11", "-DHAVE_CONFIG_H", "-DWIN32_LEAN_AND_MEAN", "-D_CRT_SECURE_NO_WARNINGS" }
    else
        &.{ "-std=c11", "-DHAVE_CONFIG_H", "-D_GNU_SOURCE" };

    c_lib.root_module.addCSourceFiles(.{
        .root = b.path("modules/libfyaml"),
        .files = &c_sources,
        .flags = common_flags,
    });
    c_lib.root_module.addCSourceFiles(.{
        .root = b.path("."),
        .files = &.{"src/lib/fy_diag_shim.c"},
        .flags = common_flags,
    });

    const blake3_flags: []const []const u8 = if (target.result.os.tag == .windows)
        &.{ "-std=c11", "-DHAVE_CONFIG_H", "-DWIN32_LEAN_AND_MEAN", "-D_CRT_SECURE_NO_WARNINGS", "-DHASHER_SUFFIX=portable", "-DSIMD_DEGREE=1" }
    else
        &.{ "-std=c11", "-DHAVE_CONFIG_H", "-D_GNU_SOURCE", "-DHASHER_SUFFIX=portable", "-DSIMD_DEGREE=1" };

    c_lib.root_module.addCSourceFiles(.{
        .root = b.path("modules/libfyaml"),
        .files = &blake3_sources,
        .flags = blake3_flags,
    });
}
