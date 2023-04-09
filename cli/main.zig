const c = @cImport({
    @cInclude("omg.h");
});
const Table = @import("table-helper").Table;

const std = @import("std");

fn checkErr(err: c.omg_error) !void {
    if (!c.is_ok(err)) {
        c.print_error(err);
        return error.CoreErr;
    }
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const token = std.os.getenv("GITHUB_TOKEN");
    if (null == token) {
        return error.TokenNotSet;
    }

    const c_token = try allocator.dupeZ(u8, token.?);
    const db_path = "/tmp/omg-test.db";
    var ctx: c.omg_context = null;
    try checkErr(c.omg_setup_context(db_path, c_token, 10, &ctx));
    defer c.omg_free_context(&ctx);

    var lst: c.omg_repo_list = .{
        .length = 0,
        .repo_array = null,
    };
    try checkErr(c.omg_query_trending(ctx, null, null, "daily", &lst));
    defer c.omg_free_repo_list(&lst);

    var i: usize = 0;
    const headers = [_][]const u8{ "Name", "Stars", "Description" };
    var table_data = std.ArrayList([headers.len][]const u8).init(allocator);
    while (i < lst.length) {
        try table_data.append([_][]const u8{
            std.mem.span(lst.repo_array[i].full_name),
            numToString(allocator, lst.repo_array[i].stargazers_count),
            std.mem.span(lst.repo_array[i].description),
        });
        i += 1;
    }
    const TrendingTable = Table(&headers);
    const table = TrendingTable{
        .data = table_data.items,
        .footer = null,
    };
    try std.io.getStdOut().writer().print("{}\n", .{table});
}

fn numToString(
    allocator: std.mem.Allocator,
    n: c_int,
) []const u8 {
    return std.fmt.allocPrint(allocator, "{d}", .{n}) catch unreachable;
}
