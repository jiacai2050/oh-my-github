const std = @import("std");
const util = @import("util.zig");
const c = @cImport({
    @cInclude("omg.h");
});

const testing = std.testing;
const time = std.time;

pub fn main() !void {
    const ctx = try util.init_ctx();

    // https://github.com/xigua2023/test-github-api/discussions
    try create(ctx);
    try query(ctx);
}

fn query(ctx: c.omg_context) !void {
    var out = c.omg_repo_discussion_category{
        .id = null,
        .categories = null,
        .len = 0,
    };
    try util.check_error(c.omg_query_repo_discussion_category(ctx, "xigua2023", "test-github-api", &out));
    defer {
        c.omg_free_repo_discussion_category(&out);
    }
}

fn create(ctx: c.omg_context) !void {
    const repo_id = "R_kgDOJ8AzuQ";
    const category_id = "DIC_kwDOJ8Azuc4CX6mT";
    var buf = std.mem.zeroes([40]u8);
    const title = try std.fmt.bufPrintZ(&buf, "Awesome Title-{d}", .{time.milliTimestamp()});
    const body = "## Test from omg\n Succeed!";
    var out = c.omg_discussion{ .url = null, .id = null };
    try util.check_error(c.omg_create_discusstion(ctx, repo_id, category_id, title, body, &out));
    defer {
        std.c.free(out.url);
        std.c.free(out.id);
    }

    const url = std.mem.span(out.url);
    try testing.expect(std.mem.indexOf(u8, url, "xigua") != null);
    try testing.expect(out.id != null);
    std.debug.print("Success: {s}-{s}\n", .{ out.url, out.id });
}
