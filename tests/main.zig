const clib = @cImport({
    @cInclude("omg.h");
});
const std = @import("std");
const testing = std.testing;
const os = std.os;
const fs = std.fs;
const log = std.log;
const mem = std.mem;

fn check_error(err: clib.omg_error) anyerror!void {
    if (!clib.is_ok(err)) {
        log.err("omg_error code:{d}, msg:{s}", .{ err.code, err.message });
        return error.TestUnexpectedError;
    }
}

fn test_download(ctx: ?*clib.struct_omg_context) anyerror!void {
    const dst_file = "/tmp/dst.json";
    const url = "https://httpbin.org/anything";
    try check_error(clib.omg_download(ctx, url, dst_file));

    const f = try fs.openFileAbsolute(dst_file, fs.File.OpenFlags{ .read = true, .write = false, .lock = fs.File.Lock.None });
    defer f.close();

    var buf: [1024]u8 = undefined;
    const bytes_read = try f.read(buf[0..]);
    log.debug("file read, len:{d}, body:{s}", .{ bytes_read, buf[0..bytes_read] });
    const Payload = struct { url: []const u8 };

    var stream = std.json.TokenStream.init(buf[0..bytes_read]);
    const payload = try std.json.parse(
        Payload,
        &stream,
        .{
            .allocator = testing.allocator,
            .ignore_unknown_fields = true,
        },
    );

    log.info("payload url {s}", .{payload.url});
    try testing.expect(mem.eql(u8, payload.url, url.*[0..]));
    try testing.expectEqualStrings(url.*[0..], payload.url);
}

fn test_created_repos(ctx: ?*clib.struct_omg_context) anyerror!void {
    try check_error(clib.omg_sync_created_repos(ctx));
    var repo_list = clib.omg_repo_list{
        .length = 0,
        .repo_array = null,
    };
    defer clib.omg_free_repo_list(&repo_list);
    try check_error(clib.omg_query_created_repos(ctx, "", "", &repo_list));
    try testing.expect(repo_list.length > 0);
    try testing.expect(repo_list.repo_array != null);
}

fn test_created_gists(ctx: ?*clib.struct_omg_context) anyerror!void {
    try check_error(clib.omg_sync_created_gists(ctx));
    var gist_list = clib.omg_gist_list{
        .length = 0,
        .gist_array = null,
    };
    defer clib.omg_free_gist_list(&gist_list);
    try check_error(clib.omg_query_created_gists(ctx, &gist_list));
    try testing.expect(gist_list.length > 0);
    try testing.expect(gist_list.gist_array != null);
}

pub fn main() anyerror!void {
    log.info("Run omg test...", .{});
    const db_path = os.getenv("DB_PATH").?;
    var ctx = init: {
        const token = os.getenv("GITHUB_TOKEN").?;
        var ctx: ?*clib.struct_omg_context = null;

        const err = clib.omg_setup_context(@ptrCast([*c]const u8, db_path), @ptrCast([*c]const u8, token), &ctx);
        try testing.expect(clib.is_ok(err));
        break :init ctx;
    };
    defer clib.omg_free_context(&ctx);
    defer fs.deleteFileAbsolute(db_path) catch {};

    try test_download(ctx);
    try test_created_repos(ctx);
    try test_created_gists(ctx);
}
