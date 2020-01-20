const Builder = @import("std").build.Builder;

pub fn build(b: *Builder) void {
    const mode = b.standardReleaseOptions();
    const lib = b.addStaticLibrary("bog", "src/lib.zig");
    lib.setBuildMode(mode);
    lib.linkSystemLibrary("c");
    lib.install();

    var main_tests = b.addTest("src/main.zig");
    main_tests.setBuildMode(mode);
    var bog_tests = b.addTest("src/bog.zig");
    bog_tests.setBuildMode(mode);

    const test_step = b.step("test", "Run all tests");
    test_step.dependOn(&bog_tests.step);
    test_step.dependOn(&main_tests.step);

    var exe = b.addExecutable("bog", "src/main.zig");
    exe.setBuildMode(mode);
    exe.install();
    
    const run_step = b.step("run", "Run Bog");
    run_step.dependOn(&exe.run().step);
}
