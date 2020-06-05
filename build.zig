const Builder = @import("std").build.Builder;

pub fn build(b: *Builder) void {
    b.setPreferredReleaseMode(.ReleaseSafe);

    const lib = b.addStaticLibrary("bog", "src/lib.zig");
    lib.linkLibC();
    lib.bundle_compiler_rt = true;
    const lib_step = b.step("lib", "Build C library");
    lib_step.dependOn(&b.addInstallArtifact(lib).step);

    addTests(b, .{
        "src/main.zig",
        "tests/fmt.zig",
        "tests/behavior.zig",
        "tests/error.zig",
    });

    var exe = b.addExecutable("bog", "src/main.zig");
    exe.install();
    exe.linkLibC();

    const fmt_step = b.step("fmt", "Format all source files");
    fmt_step.dependOn(&b.addFmt(&[_][]const u8{
        "build.zig",
        "src",
    }).step);
}

fn addTests(b: *Builder, tests: var) void {
    const tests_step = b.step("test", "Run all tests");
    tests_step.dependOn(b.getInstallStep());
    inline for (tests) |t| {
        var test_step = b.addTest(t);
        test_step.addPackagePath("bog", "src/bog.zig");
        tests_step.dependOn(&test_step.step);
    }
}
