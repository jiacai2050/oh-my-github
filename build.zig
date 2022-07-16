const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    // Standard target options allows the person running `zig build` to choose
    // what target to build for. Here we do not override the defaults, which
    // means any target is allowed, and the default is native. Other options
    // for restricting supported target set are available.
    const target = b.standardTargetOptions(.{});

    // Standard release options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall.
    const mode = b.standardReleaseOptions();

    const main_tests = b.addTest("tests/main.zig");
    main_tests.addIncludeDir("core");
    main_tests.addCSourceFile("core/omg.c", &[_][]const u8{
        "-std=c99",
        "-DOMG_TEST=1",
        "-DOMG_VERBOSE=1",
    });

    main_tests.setTarget(target);
    main_tests.setBuildMode(mode);
    main_tests.linkLibC();
    main_tests.linkSystemLibrary("sqlite3");
    main_tests.linkSystemLibrary("libcurl");
    main_tests.linkSystemLibrary("jansson");
    main_tests.linkSystemLibrary("libpcre2-posix");

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&main_tests.step);
}
