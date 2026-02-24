const std = @import("std");

pub fn build(b: *std.Build) void {
    const mod_name = "yaml";

    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const fangz_dep = b.dependency(
        "fangz",
        .{ .target = target, .optimize = optimize },
    );

    const lib_mod = b.addModule(mod_name, .{
        .root_source_file = b.path("src/lib/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    const lib = b.addLibrary(.{
        .name = mod_name,
        .root_module = lib_mod,
    });

    const cli = b.addExecutable(.{
        .name = mod_name ++ "-cli",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/cli//main.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{
                    .name = mod_name,
                    .module = lib_mod,
                },
                .{
                    .name = "fangz",
                    .module = fangz_dep.module("fangz"),
                },
            },
        }),
    });

    b.installArtifact(cli);

    const cli_step = b.step("cli", "Build the CLI");
    const run_cli = b.addRunArtifact(cli);
    cli_step.dependOn(&run_cli.step);

    const docs = b.addInstallDirectory(.{
        .source_dir = lib.getEmittedDocs(),
        .install_dir = .prefix,
        .install_subdir = "docs",
    });

    const docs_step = b.step("docs", "Generate the documentation");
    docs_step.dependOn(&docs.step);

    const tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("tests/suite.zig"),
            .optimize = optimize,
            .target = target,
            .imports = &.{.{
                .name = mod_name,
                .module = lib_mod,
            }},
        }),
    });

    const run_tests = b.addRunArtifact(tests);
    const tests_step = b.step("tests", "Run the test suite");
    tests_step.dependOn(&run_tests.step);
}
