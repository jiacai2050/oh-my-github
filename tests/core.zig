const clib = @cImport({
    @cInclude("omg.h");
});
const util = @import("util.zig");
const std = @import("std");
const testing = std.testing;
const fs = std.fs;
const log = std.log;
const mem = std.mem;
const check_error = util.check_error;

fn test_download(ctx: ?*clib.struct_omg_context) anyerror!void {
    const dst_file = "/tmp/dst.json";
    const url = "https://httpbin.org/anything";
    try check_error(clib.omg_download(ctx, url, dst_file));

    const f = try fs.openFileAbsolute(dst_file, .{});
    defer f.close();

    var buf: [1024]u8 = undefined;
    const bytes_read = try f.readAll(buf[0..]);
    log.debug("file read, len:{d}, body:{s}", .{ bytes_read, buf[0..bytes_read] });
    const Payload = struct { url: []const u8 };

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const payload = try std.json.parseFromSlice(Payload, allocator, buf[0..bytes_read], .{
        .ignore_unknown_fields = true,
    });
    const actual_url = payload.value.url;

    log.info("payload url {s}", .{actual_url});
    try testing.expectEqualStrings(url[0..], actual_url);
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
    const repo_name = repo_list.repo_array[0].full_name;

    // query with repo_name
    var list2 = clib.omg_repo_list{
        .length = 0,
        .repo_array = null,
    };
    defer clib.omg_free_repo_list(&list2);
    try check_error(clib.omg_query_created_repos(ctx, repo_name, "", &list2));
    try testing.expect(list2.length > 0);
    try testing.expect(list2.repo_array != null);

    // query with lang
    var list3 = clib.omg_repo_list{
        .length = 0,
        .repo_array = null,
    };
    defer clib.omg_free_repo_list(&list3);
    try check_error(clib.omg_query_created_repos(ctx, "", "rust", &list3));
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

fn test_create_pull(ctx: ?*clib.struct_omg_context) anyerror!void {
    var resp = clib.omg_pull{
        .number = -1,
        .commits = -1,
        .additions = -1,
        .deletions = -1,
    };
    const full_name = "xigua2023/xigua2023";
    try check_error(clib.omg_create_pull(
        ctx,
        full_name,
        "title: test create PR",
        \\ # hello
        \\ just a test
    ,
        "xigua2023:test",
        "main",
        false, // draft
        &resp,
    ));
    defer check_error(clib.omg_toggle_pull(ctx, full_name, resp.number, true)) catch {};

    try testing.expect(resp.number > 0);
    try testing.expectEqual(@as(i32, 1), resp.commits);
    try testing.expectEqual(@as(i32, 3), resp.additions);
    try testing.expectEqual(@as(i32, 8), resp.deletions);
}

fn test_star_repo(ctx: ?*clib.struct_omg_context) anyerror!void {
    try check_error(clib.omg_star_repo(ctx, "xigua2023/xigua2023"));
}

pub fn main() anyerror!void {
    log.info(
        \\
        \\      ___  __  __  ___
        \\     / _ \|  \/  |/ __|
        \\    | (_) | |\/| | (_ |
        \\     \___/|_|  |_|\___|
        \\
    , .{});
    log.info("Run oh-my-github test...", .{});
    const db_path = std.c.getenv("DB_PATH").?;
    var ctx = try util.init_ctx();
    defer clib.omg_free_context(&ctx);
    defer fs.deleteFileAbsolute(db_path[0..std.mem.len(db_path)]) catch {};

    try test_download(ctx);
    try test_created_repos(ctx);
    try test_created_gists(ctx);
    try test_create_pull(ctx);
    try test_star_repo(ctx);
}
