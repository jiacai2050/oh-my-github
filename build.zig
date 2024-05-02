const std = @import("std");
const CompileStep = std.Build.Step.Compile;
const CrossTarget = std.Build.ResolvedTarget;
const OptimizeMode = std.builtin.OptimizeMode;

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const verbose = b.option(bool, "verbose", "Enable verbose log");
    const quick = b.option(bool, "quick", "Enable quick mode");
    const core_lib = b.addStaticLibrary(.{
        .name = "omg-core",
        .root_source_file = .{ .path = "core/omg.zig" },
        .target = target,
        .optimize = optimize,
    });
    const create_table_header_step = b.addSystemCommand(&[_][]const u8{ "make", "core/create_table.h" });
    core_lib.step.dependOn(&create_table_header_step.step);
    var cflags = std.ArrayList([]const u8).init(b.allocator);
    try cflags.append("-std=gnu99");
    try cflags.append("-Wall");
    try cflags.append("-Wextra");
    try cflags.append("-Werror");
    try cflags.append("-Wno-unused-parameter");
    try cflags.append("-Wno-gnu");
    try cflags.append("-Wimplicit-fallthrough");
    if (verbose) |v| {
        if (v) {
            try cflags.append("-DVERBOSE");
        }
    }
    if (quick) |v| {
        if (v) {
            try cflags.append("-DOMG_TEST");
        }
    }
    core_lib.addIncludePath(b.path("core"));
    core_lib.addCSourceFile(.{
        .file = b.path("core/omg.c"),
        .flags = cflags.items,
    });
    core_lib.linkSystemLibrary("sqlite3");
    core_lib.linkSystemLibrary("libcurl");
    core_lib.linkSystemLibrary("jansson");
    core_lib.linkSystemLibrary("libpcre2-posix");
    core_lib.linkLibC();

    buildEmacsModule(b, core_lib, target, optimize, cflags.items);
    buildCliTool(
        b,
        core_lib,
        target,
        optimize,
    );

    inline for (.{ "core", "discussion" }) |name| {
        buildTest(
            b,
            name,
            core_lib,
            target,
            optimize,
        );
    }
}

fn buildEmacsModule(b: *std.Build, core_lib: *CompileStep, target: CrossTarget, optimize: OptimizeMode, cflags: []const []const u8) void {
    const lib = b.addSharedLibrary(.{
        .name = "omg-dyn",
        .target = target,
        .optimize = optimize,
        .pic = true,
        .link_libc = true,
    });
    lib.addIncludePath(b.path("emacs"));
    lib.linkLibrary(core_lib);
    lib.addCSourceFile(.{
        .file = b.path("emacs/emacs.c"),
        .flags = cflags,
    });
    b.installArtifact(lib);
}

fn buildCliTool(
    b: *std.Build,
    core_lib: *CompileStep,
    target: CrossTarget,
    optimize: OptimizeMode,
) void {
    const exe = b.addExecutable(.{
        .name = "omg-cli",
        .root_source_file = b.path("cli/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const zigcli_dep = b.dependency("zigcli", .{});
    exe.root_module.addImport("simargs", zigcli_dep.module("simargs"));
    exe.root_module.addImport("pretty-table", zigcli_dep.module("pretty-table"));

    exe.addIncludePath(b.path("core"));
    exe.linkLibrary(core_lib);
    exe.linkLibC();
    b.installArtifact(exe);
}

fn buildTest(
    b: *std.Build,
    comptime name: []const u8,
    core_lib: *CompileStep,
    target: CrossTarget,
    optimize: OptimizeMode,
) void {
    const exe = b.addExecutable(.{
        .name = "test-" ++ name,
        .root_source_file = b.path("tests/" ++ name ++ ".zig"),
        .target = target,
        .optimize = optimize,
    });
    exe.addIncludePath(b.path("core"));
    exe.linkLibrary(core_lib);
    exe.linkLibC();
    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("test-" ++ name, "Run the test " ++ name);
    run_step.dependOn(&run_cmd.step);
}
