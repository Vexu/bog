const Builder = @import("std").build.Builder;

pub fn build(b: *Builder) void {
    const mode = b.standardReleaseOptions();
    // const lib = b.addStaticLibrary("bog", "src/lib.zig");
    // lib.setBuildMode(mode);
    // lib.linkSystemLibrary("c");
    // lib.install();

    addTests(b, mode, .{
        "src/main.zig",
        "tests/fmt.zig",
        "tests/behavior.zig",
    });

    var exe = b.addExecutable("bog", "src/main.zig");
    exe.setBuildMode(mode);
    exe.install();

    // repl doesn't work with this
    // const run_step = b.step("run", "Run");
    // run_step.dependOn(&exe.run().step);

    const fmt_step = b.step("fmt", "Format all source files");
    fmt_step.dependOn(&b.addFmt(&[_][]const u8{
        "build.zig",
        "src",
    }).step);
}

fn addTests(b: *Builder, mode: var, tests: var) void {
    const tests_step = b.step("test", "Run all tests");
    tests_step.dependOn(b.getInstallStep());
    inline for (tests) |t| {
        var test_step = b.addTest(t);
        test_step.setBuildMode(mode);
        test_step.addPackagePath("bog", "src/bog.zig");
        tests_step.dependOn(&test_step.step);
    }
}
