const std = @import("std");
const Builder = std.build.Builder;

pub fn build(b: *Builder) void {
    const mode = b.standardOptimizeOption(.{});

    const lib = b.addStaticLibrary(.{
        .name = "bog",
        .optimize = mode,
        .target = .{},
        .root_source_file = .{ .path = "src/lib.zig" },
    });
    lib.linkLibC();

    const lib_options = b.addOptions();
    lib.addOptions("build_options", lib_options);

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

    const lib_step = b.step("lib", "Build C library");
    lib_step.dependOn(&b.addInstallArtifact(lib).step);

    // c library usage example
    const c_example = b.addExecutable(.{
        .name = "bog_from_c",
        .optimize = mode,
        .target = .{},
    });
    c_example.addCSourceFile("examples/bog_from_c.c", &[_][]const u8{});
    c_example.addIncludePath("include");
    c_example.linkLibrary(lib);
    c_example.linkLibC();
    c_example.addLibraryPath("zig-cache/lib");
    c_example.step.dependOn(lib_step);
    c_example.override_dest_dir = .{ .custom = "examples/bin" };

    // calling zig from bog example
    const zig_from_bog = b.addExecutable(.{
        .name = "zig_from_bog",
        .optimize = mode,
        .target = .{},
        .root_source_file = .{ .path = "examples/zig_from_bog.zig" },
    });
    zig_from_bog.addAnonymousModule("bog", .{
        .source_file = .{ .path = "src/bog.zig" },
    });
    zig_from_bog.override_dest_dir = .{ .custom = "examples/bin" };

    const examples_step = b.step("examples", "Build all examples");
    examples_step.dependOn(&b.addInstallArtifact(c_example).step);
    examples_step.dependOn(&b.addInstallArtifact(zig_from_bog).step);

    addTests(b, examples_step, .{
        "src/main.zig",
        "tests/fmt.zig",
        "tests/behavior.zig",
        "tests/error.zig",
    });

    var exe = b.addExecutable(.{
        .name = "bog",
        .optimize = mode,
        .target = .{},
        .root_source_file = .{ .path = "src/main.zig" },
    });
    exe.addAnonymousModule("linenoize", .{
        .source_file = .{ .path = "lib/linenoize/src/main.zig" },
    });
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
    const rm_zig_cache = b.addRemoveDirTree("zig-cache");
    const rm_examples_bing = b.addRemoveDirTree("examples/bin");
    clean_step.dependOn(&rm_zig_cache.step);
    clean_step.dependOn(&rm_examples_bing.step);
}

fn addTests(b: *Builder, examples_step: *std.build.Step, tests: anytype) void {
    const tests_step = b.step("test", "Run all tests");
    // tests_step.dependOn(examples_step);
    _ = examples_step;
    inline for (tests) |t| {
        var test_step = b.addTest(.{ .root_source_file = .{ .path = t } });
        test_step.addAnonymousModule("bog", .{
            .source_file = .{ .path = "src/bog.zig" },
        });
        test_step.addAnonymousModule("linenoize", .{
            .source_file = .{ .path = "lib/linenoize/src/main.zig" },
        });
        tests_step.dependOn(&test_step.step);
    }
}
