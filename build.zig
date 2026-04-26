const std = @import("std");

pub fn build(b: *std.Build) void {
    const mod_name = "yaml";
    const fy_mod_name = "fy";

    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const summary_only = b.option(bool, "summary", "Only print short coverage summary (useful for CI)") orelse false;

    const fy_c_lib = b.addLibrary(.{
        .name = "fyaml",
        .linkage = .static,
        .root_module = b.createModule(.{
            .target = target,
            .optimize = optimize,
            .link_libc = true,
        }),
    });
    fy_c_lib.root_module.link_libc = true;
    fy_c_lib.root_module.addIncludePath(b.path("modules/libfyaml/include"));
    fy_c_lib.root_module.addIncludePath(b.path("modules/libfyaml/src/lib"));
    fy_c_lib.root_module.addIncludePath(b.path("modules/libfyaml/src/util"));
    fy_c_lib.root_module.addIncludePath(b.path("modules/libfyaml/src/xxhash"));
    fy_c_lib.root_module.addIncludePath(b.path("modules/libfyaml/src/thread"));
    fy_c_lib.root_module.addIncludePath(b.path("modules/libfyaml/src/allocator"));
    fy_c_lib.root_module.addIncludePath(b.path("modules/libfyaml/src/blake3"));
    fy_c_lib.root_module.addIncludePath(b.path("src/lib/fy_config"));

    if (target.result.os.tag == .windows) {
        fy_c_lib.root_module.addCSourceFiles(.{
            .root = b.path("modules/libfyaml"),
            .files = &.{
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
            },
            .flags = &.{ "-std=c11", "-DWIN32_LEAN_AND_MEAN", "-D_CRT_SECURE_NO_WARNINGS" },
        });
        fy_c_lib.root_module.addCSourceFiles(.{
            .root = b.path("."),
            .files = &.{"src/lib/fy_diag_shim.c"},
            .flags = &.{ "-std=c11", "-DWIN32_LEAN_AND_MEAN", "-D_CRT_SECURE_NO_WARNINGS" },
        });
        fy_c_lib.root_module.addCSourceFiles(.{
            .root = b.path("modules/libfyaml"),
            .files = &.{
                "src/blake3/blake3_portable.c",
                "src/blake3/blake3.c",
            },
            .flags = &.{ "-std=c11", "-DWIN32_LEAN_AND_MEAN", "-D_CRT_SECURE_NO_WARNINGS", "-DHASHER_SUFFIX=portable", "-DSIMD_DEGREE=1" },
        });
    } else {
        fy_c_lib.root_module.addCSourceFiles(.{
            .root = b.path("modules/libfyaml"),
            .files = &.{
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
            },
            .flags = &.{ "-std=c11", "-D_GNU_SOURCE" },
        });
        fy_c_lib.root_module.addCSourceFiles(.{
            .root = b.path("."),
            .files = &.{"src/lib/fy_diag_shim.c"},
            .flags = &.{ "-std=c11", "-D_GNU_SOURCE" },
        });
        fy_c_lib.root_module.addCSourceFiles(.{
            .root = b.path("modules/libfyaml"),
            .files = &.{
                "src/blake3/blake3_portable.c",
                "src/blake3/blake3.c",
            },
            .flags = &.{ "-std=c11", "-D_GNU_SOURCE", "-DHASHER_SUFFIX=portable", "-DSIMD_DEGREE=1" },
        });
    }

    if (target.result.os.tag != .windows) {
        fy_c_lib.root_module.linkSystemLibrary("pthread", .{});
    }

    const lib_mod = b.addModule(
        mod_name,
        .{
            .root_source_file = b.path("src/lib/yaml.zig"),
            .target = target,
            .optimize = optimize,
        },
    );

    const fy_mod = b.addModule(
        fy_mod_name,
        .{
            .root_source_file = b.path("src/lib/fy.zig"),
            .target = target,
            .optimize = optimize,
            .link_libc = true,
        },
    );

    fy_mod.addIncludePath(b.path("modules/libfyaml/include"));
    fy_mod.addIncludePath(b.path("modules/libfyaml/src/lib"));
    fy_mod.addIncludePath(b.path("modules/libfyaml/src/util"));
    fy_mod.addIncludePath(b.path("modules/libfyaml/src/xxhash"));
    fy_mod.addIncludePath(b.path("modules/libfyaml/src/thread"));
    fy_mod.addIncludePath(b.path("modules/libfyaml/src/allocator"));
    fy_mod.addIncludePath(b.path("modules/libfyaml/src/blake3"));
    fy_mod.addIncludePath(b.path("src/lib/fy_config"));

    const docs_step = b.step("docs", "Generate the documentation");

    const docs_lib = b.addLibrary(.{
        .name = "yaml_docs",
        .root_module = b.addModule(
            "yaml_docs",
            .{
                .root_source_file = b.path("src/lib/root.zig"),
                .target = target,
                .optimize = optimize,
                .link_libc = true,
                .imports = &.{
                    .{
                        .name = fy_mod_name,
                        .module = fy_mod,
                    },
                },
            },
        ),
    });

    docs_lib.root_module.linkLibrary(fy_c_lib);
    docs_lib.root_module.addIncludePath(b.path("modules/libfyaml/include"));
    docs_lib.root_module.addIncludePath(b.path("src/lib/fy_config"));

    const docs = b.addInstallDirectory(.{
        .source_dir = docs_lib.getEmittedDocs(),
        .install_dir = .prefix,
        .install_subdir = "docs",
    });

    docs_step.dependOn(&docs.step);

    const build_options = b.addOptions();
    build_options.addOption(bool, "summary_only", summary_only);

    const tests_step = b.step("tests", "Run the test suite");

    const integration_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("tests/suite.zig"),
            .optimize = optimize,
            .target = target,
            .link_libc = true,
            .imports = &.{
                .{ .name = mod_name, .module = lib_mod },
                .{ .name = fy_mod_name, .module = fy_mod },
                .{ .name = "build_options", .module = build_options.createModule() },
            },
        }),
    });
    integration_tests.root_module.linkLibrary(fy_c_lib);
    integration_tests.root_module.addIncludePath(b.path("modules/libfyaml/include"));
    integration_tests.root_module.addIncludePath(b.path("src/lib/fy_config"));

    const run_integration_tests = b.addRunArtifact(integration_tests);
    tests_step.dependOn(&run_integration_tests.step);

    // const unit_tests = b.addTest(.{
    //     .root_module = lib_mod,
    // });
    // const run_unit_tests = b.addRunArtifact(unit_tests);
    // tests_step.dependOn(&run_unit_tests.step);
}
