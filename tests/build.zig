const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const exe = b.addExecutable(.{
        .name = "omg-test",
        .root_source_file = .{ .path = "main.zig" },
        .target = target,
    });
    exe.addIncludePath("../core");
    exe.addCSourceFile("../core/omg.c", &[_][]const u8{
        "-std=gnu99",
        "-DOMG_TEST",
        // "-DVERBOSE",
    });
    exe.linkSystemLibrary("sqlite3");
    exe.linkSystemLibrary("libcurl");
    exe.linkSystemLibrary("jansson");
    exe.linkSystemLibrary("libpcre2-posix");
    exe.linkLibC();
    exe.install();

    const run_cmd = exe.run();
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the test");
    run_step.dependOn(&run_cmd.step);
}
