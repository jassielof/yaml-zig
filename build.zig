const std = @import("std");

const fy = @import("fy.build.zig");

pub fn build(b: *std.Build) void {
    const mod_name = "yaml";
    const fy_mod_name = "fy";

    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const summary_only = b.option(bool, "summary", "Only print short coverage summary (useful for CI)") orelse false;

    const fy_dep = fy.create(b, .{
        .module_name = fy_mod_name,
        .target = target,
        .optimize = optimize,
    });

    const lib_mod = b.addModule(
        mod_name,
        .{
            .root_source_file = b.path("src/lib/yaml.zig"),
            .target = target,
            .optimize = optimize,
        },
    );

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
                        .module = fy_dep.module,
                    },
                },
            },
        ),
    });

    fy_dep.link(docs_lib.root_module);

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
                .{ .name = fy_mod_name, .module = fy_dep.module },
                .{ .name = "build_options", .module = build_options.createModule() },
            },
        }),
    });

    fy_dep.link(integration_tests.root_module);

    const run_integration_tests = b.addRunArtifact(integration_tests);
    tests_step.dependOn(&run_integration_tests.step);

    // const unit_tests = b.addTest(.{
    //     .root_module = lib_mod,
    // });

    // const run_unit_tests = b.addRunArtifact(unit_tests);
    // tests_step.dependOn(&run_unit_tests.step);
}
