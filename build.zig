const std = @import("std");
const Build = std.Build;

pub fn build(b: *Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const linenoise = b.dependency("linenoise", .{
        .target = target,
        .optimize = optimize,
    });

    const bog_module = b.createModule(.{
        .root_source_file = b.path("src/bog.zig"),
        .imports = &.{
            .{
                .name = "linenoise",
                .module = linenoise.module("linenoise"),
            },
        },
    });

    const lib_options = b.addOptions();
    lib_options.addOption(
        bool,
        "no_std",
        b.option(bool, "NO_ADD_STD", "Do not export bog_Vm_addStd to reduce binary size") orelse false,
    );
    lib_options.addOption(
        bool,
        "no_std_no_io",
        b.option(bool, "NO_ADD_STD_NO_IO", "Do not export bog_Vm_addStd to reduce binary size") orelse false,
    );

    const lib = b.addStaticLibrary(.{
        .name = "bog",
        .optimize = optimize,
        .target = target,
        .root_source_file = b.path("src/lib.zig"),
        .link_libc = true,
    });
    lib.root_module.addOptions("build_options", lib_options);

    const lib_step = b.step("lib", "Build C library");
    lib_step.dependOn(&b.addInstallArtifact(lib, .{}).step);

    // c library usage example
    const c_example = b.addExecutable(.{
        .name = "bog_from_c",
        .optimize = optimize,
        .target = target,
        .link_libc = true,
    });
    c_example.addCSourceFile(.{ .file = b.path("examples/bog_from_c.c") });

    c_example.addIncludePath(b.path("include"));
    c_example.linkLibrary(lib);
    c_example.step.dependOn(lib_step);

    // calling zig from bog example
    const zig_from_bog = b.addExecutable(.{
        .name = "zig_from_bog",
        .optimize = optimize,
        .target = target,
        .root_source_file = b.path("examples/zig_from_bog.zig"),
    });
    zig_from_bog.root_module.addImport("bog", bog_module);

    const examples_step = b.step("examples", "Build all examples");
    examples_step.dependOn(&b.addInstallArtifact(c_example, .{ .dest_dir = .{ .override = .{ .custom = "examples/bin" } } }).step);
    examples_step.dependOn(&b.addInstallArtifact(zig_from_bog, .{ .dest_dir = .{ .override = .{ .custom = "examples/bin" } } }).step);

    addTests(b, examples_step, bog_module, .{
        "src/main.zig",
        "tests/fmt.zig",
        "tests/behavior.zig",
        "tests/error.zig",
    });

    const exe = b.addExecutable(.{
        .name = "bog",
        .optimize = optimize,
        .target = target,
        .root_source_file = b.path("src/main.zig"),
    });
    exe.root_module.addImport("bog", bog_module);
    b.installArtifact(exe);

    const fmt_step = b.step("fmt", "Format all source files");
    fmt_step.dependOn(&b.addFmt(.{
        .paths = &.{
            "build.zig",
            "src",
            "examples",
            "tests",
        },
    }).step);

    const clean_step = b.step("clean", "Delete all artifacts created by zig build");
    const rm_zig_cache = b.addRemoveDirTree(b.path(".zig-cache"));
    const rm_examples_bing = b.addRemoveDirTree(b.path("examples/bin"));
    clean_step.dependOn(&rm_zig_cache.step);
    clean_step.dependOn(&rm_examples_bing.step);
}

fn addTests(b: *Build, examples_step: *std.Build.Step, bog_module: *std.Build.Module, tests: anytype) void {
    const tests_step = b.step("test", "Run all tests");
    tests_step.dependOn(examples_step);
    inline for (tests) |t| {
        var test_step = b.addTest(.{ .root_source_file = b.path(t) });
        test_step.root_module.addImport("bog", bog_module);
        tests_step.dependOn(&test_step.step);
    }
}
