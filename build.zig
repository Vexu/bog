const Builder = @import("std").build.Builder;

pub fn build(b: *Builder) void {
    const mode = b.standardReleaseOptions();
    // const lib = b.addStaticLibrary("lang", "src/lib.zig");
    // lib.setBuildMode(mode);
    // lib.linkSystemLibrary("c");
    // lib.install();

    var tests = b.addTest("src/main.zig");
    tests.setBuildMode(mode);
    var fmt_test = b.addTest("tests/fmt.zig");
    fmt_test.setBuildMode(mode);
    fmt_test.addPackagePath("lang", "src/lang.zig");

    const test_step = b.step("test", "Run all tests");
    test_step.dependOn(&tests.step);
    test_step.dependOn(&fmt_test.step);

    var exe = b.addExecutable("lang", "src/main.zig");
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
