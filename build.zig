const std = @import("std");

const fy = @import("fy.build.zig");

pub fn build(b: *std.Build) void {
    const mod_name = "yaml";
    const fy_mod_name = "fy";

    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const summary_only = b.option(bool, "summary", "Only print short coverage summary (useful for CI)") orelse false;

    const lib_mod = b.addModule(
        mod_name,
        .{
            .root_source_file = b.path("lib/yaml/root.zig"),
            .target = target,
            .optimize = optimize,
        },
    );

    const fy_dep = fy.create(b, .{
        .module_name = fy_mod_name,
        .target = target,
        .optimize = optimize,
    });

    const docs_step = b.step("docs", "Generate the documentation");
    const docs_lib = b.addLibrary(.{
        .name = "yaml",
        .root_module = b.createModule(.{
            .root_source_file = b.path("lib/yaml/root.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    const docs = b.addInstallDirectory(.{
        .source_dir = docs_lib.getEmittedDocs(),
        .install_dir = .prefix,
        .install_subdir = "docs",
    });
    docs_step.dependOn(&docs.step);

    const build_options = b.addOptions();
    build_options.addOption(bool, "summary_only", summary_only);

    const test_step = b.step("test", "Run the test suite");

    const unit_tests = b.addTest(.{
        .name = "yaml",
        .root_module = b.createModule(.{
            .root_source_file = b.path("lib/yaml/root.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    test_step.dependOn(&b.addRunArtifact(unit_tests).step);

    const spec_tests = b.addTest(.{
        .name = "spec",
        .root_module = b.createModule(.{
            .root_source_file = b.path("tests/spec.zig"),
            .optimize = optimize,
            .target = target,
            .link_libc = true,
            .imports = &.{
                .{ .name = mod_name, .module = lib_mod },
                .{ .name = fy_mod_name, .module = fy_dep.module },
                .{ .name = "build_options", .module = build_options.createModule() },
            },
        }),
    });
    fy_dep.link(spec_tests.root_module);
    test_step.dependOn(&b.addRunArtifact(spec_tests).step);
}
