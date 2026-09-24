const std = @import("std");

const fy = @import("build/fy.zig");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const summary_only = b.option(bool, "summary", "Only print short coverage summary (useful for CI)") orelse false;

    const yaml_mod = b.addModule(
        "yaml",
        .{
            .root_source_file = b.path("lib/yaml/root.zig"),
            .target = target,
            .optimize = optimize,
        },
    );

    const fy_dep = fy.create(b, .{
        .module_name = "fy",
        .target = target,
        .optimize = optimize,
    });

    const docs_step = b.step("docs", "Generate the documentation");
    const yaml_lib = b.addLibrary(.{
        .name = "yaml",
        .root_module = yaml_mod,
    });
    const yaml_docs = b.addInstallDirectory(.{
        .source_dir = yaml_lib.getEmittedDocs(),
        .install_dir = .prefix,
        .install_subdir = "docs",
    });
    docs_step.dependOn(&yaml_docs.step);

    const build_options = b.addOptions();
    build_options.addOption(bool, "summary_only", summary_only);

    const test_step = b.step("test", "Run the test suite");

    const yaml_test = b.addTest(.{
        .name = "YAML",
        .root_module = b.createModule(.{
            .root_source_file = b.path("lib/yaml/root.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    test_step.dependOn(&b.addRunArtifact(yaml_test).step);

    const spec_tests = b.addTest(.{
        .name = "Specification Compliance",
        .root_module = b.createModule(.{
            .root_source_file = b.path("tests/spec.zig"),
            .optimize = optimize,
            .target = target,
            .link_libc = true,
            .imports = &.{
                .{ .name = "yaml", .module = yaml_mod },
                .{ .name = "fy", .module = fy_dep.module },
                .{ .name = "build_options", .module = build_options.createModule() },
            },
        }),
    });
    fy_dep.link(spec_tests.root_module);
    test_step.dependOn(&b.addRunArtifact(spec_tests).step);

    const coverage_summary = b.addExecutable(.{
        .name = "coverage-summary",
        .root_module = b.createModule(.{
            .root_source_file = b.path("build/coverage_summary.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    const run_coverage_summary = b.addRunArtifact(coverage_summary);
    const coverage_summary_step = b.step("coverage-summary", "Render YAML spec coverage into GITHUB_STEP_SUMMARY or stdout");
    coverage_summary_step.dependOn(&run_coverage_summary.step);

    const check_step = b.step("check", "Run code quality checks");
    const fmt = b.addFmt(.{ .check = true, .paths = &.{
        "lib",
    } });
    check_step.dependOn(&fmt.step);
}
