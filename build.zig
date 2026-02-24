const std = @import("std");

pub fn build(b: *std.Build) void {
    const mod_name = "yaml";

    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const summary_only = b.option(bool, "summary", "Only print short coverage summary (useful for CI)") orelse false;

    const lib_mod = b.addModule(mod_name, .{
        .root_source_file = b.path("src/lib/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    const lib = b.addLibrary(.{
        .name = mod_name,
        .root_module = lib_mod,
    });

    const docs = b.addInstallDirectory(.{
        .source_dir = lib.getEmittedDocs(),
        .install_dir = .prefix,
        .install_subdir = "docs",
    });

    const docs_step = b.step("docs", "Generate the documentation");
    docs_step.dependOn(&docs.step);

    const build_options = b.addOptions();
    build_options.addOption(bool, "summary_only", summary_only);

    const tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("tests/suite.zig"),
            .optimize = optimize,
            .target = target,
            .imports = &.{
                .{ .name = mod_name, .module = lib_mod },
                .{ .name = "build_options", .module = build_options.createModule() },
            },
        }),
    });

    const run_tests = b.addRunArtifact(tests);
    const tests_step = b.step("tests", "Run the test suite");
    tests_step.dependOn(&run_tests.step);
}
