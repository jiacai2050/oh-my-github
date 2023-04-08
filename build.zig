const std = @import("std");
const CompileStep = std.Build.CompileStep;
const CrossTarget = std.zig.CrossTarget;
const OptimizeMode = std.builtin.OptimizeMode;

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const verbose = b.option(bool, "verbose", "Enable verbose log");
    const quick = b.option(bool, "quick", "Enable quick mode");
    const core_lib = b.addStaticLibrary(.{
        .name = "omg-core",
        .target = target,
        .optimize = optimize,
    });
    core_lib.addIncludePath("core");
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
    core_lib.addCSourceFile("./core/omg.c", cflags.items);
    core_lib.linkSystemLibrary("sqlite3");
    core_lib.linkSystemLibrary("libcurl");
    core_lib.linkSystemLibrary("jansson");
    core_lib.linkSystemLibrary("libpcre2-posix");
    core_lib.linkLibC();
    core_lib.install();

    buildEmacsDyn(b, core_lib, target, optimize, cflags.items);
    buildCli(b, core_lib, target, optimize, cflags.items);
}

fn buildEmacsDyn(b: *std.Build, core_lib: *CompileStep, target: CrossTarget, optimize: OptimizeMode, cflags: []const []const u8) void {
    const lib = b.addSharedLibrary(.{
        .name = "omg-dyn",
        .target = target,
        .optimize = optimize,
    });
    lib.force_pic = true;
    lib.addIncludePath("emacs");
    lib.linkLibrary(core_lib);
    lib.addCSourceFile("emacs/emacs.c", cflags);
    lib.install();
}

fn buildCli(b: *std.Build, core_lib: *CompileStep, target: CrossTarget, optimize: OptimizeMode, cflags: []const []const u8) void {
    const omg_cli_exe = b.addExecutable(.{
        .name = "omg-cli",
        .target = target,
        .optimize = optimize,
    });

    omg_cli_exe.step.dependOn(&b.addSystemCommand(&[_][]const u8{ "make", "cli/help.h" }).step);
    omg_cli_exe.linkLibrary(core_lib);
    omg_cli_exe.linkLibC();
    omg_cli_exe.addCSourceFile("cli/cli.c", cflags);
    omg_cli_exe.install();
}
