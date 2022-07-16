const clib = @cImport({
    @cInclude("omg.h");
});
const std = @import("std");
const testing = std.testing;
const os = std.os;

test "omg" {
    var ctx = init: {
        const token = os.getenv("GITHUB_TOKEN").?;
        const db_path = os.getenv("DB_PATH").?;
        var ctx: ?*clib.struct_omg_context = null;

        const err = clib.omg_setup_context(@ptrCast([*c]const u8, db_path), @ptrCast([*c]const u8, token), &ctx);
        try testing.expect(clib.is_ok(err));
        break :init ctx;
    };
    defer clib.omg_free_context(&ctx);

    // download
    try testing.expect(clib.is_ok(clib.omg_download(ctx, "https://httpbin.org/anything", "/tmp/anything.json")));

    // repos
    try testing.expect(clib.is_ok(clib.omg_sync_created_repos(ctx)));
    var repo_list = clib.omg_repo_list{
        .length = 0,
        .repo_array = null,
    };
    try testing.expect(clib.is_ok(clib.omg_query_created_repos(ctx, "", "", &repo_list)));
    try testing.expect(repo_list.length > 0);
    try testing.expect(repo_list.repo_array != null);

    // gists
    try testing.expect(clib.is_ok(clib.omg_sync_created_gists(ctx)));
    var gist_list = clib.omg_gist_list{
        .length = 0,
        .gist_array = null,
    };
    try testing.expect(clib.is_ok(clib.omg_query_created_gists(ctx, &gist_list)));
    try testing.expect(gist_list.length > 0);
    try testing.expect(gist_list.gist_array != null);
}
