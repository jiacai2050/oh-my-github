const c = @cImport({
    @cInclude("omg.h");
});
const Table = @import("table-helper").Table;
const simargs = @import("simargs");

const std = @import("std");

fn checkErr(err: c.omg_error) !void {
    if (!c.is_ok(err)) {
        c.print_error(err);
        return error.CoreErr;
    }
}

const SubCommand = enum {
    sync,
    trend,
};

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var opt = try simargs.parse(allocator, struct {
        mode: SubCommand,
        timeout: i32 = 10,
        help: bool = false,

        pub const __shorts__ = .{
            .mode = .m,
            .timeout = .t,
            .help = .h,
        };

        // This declares option's help message
        pub const __messages__ = .{
            .mode = "Which task to execute",
            .timeout = "HTTP timeout(seconds)",
            .help = "Prints help message",
        };
    }, "[args]", null);

    const token = std.os.getenv("GITHUB_TOKEN");
    if (null == token) {
        return error.TokenNotSet;
    }
    const db_path = std.os.getenv("OMG_DB_PATH");
    if (null == db_path) {
        return error.DBPathNotSet;
    }

    const c_token = try allocator.dupeZ(u8, token.?);
    const c_db_path = try allocator.dupeZ(u8, db_path.?);
    var ctx: c.omg_context = null;
    try checkErr(c.omg_setup_context(c_db_path, c_token, opt.args.timeout, &ctx));
    defer c.omg_free_context(&ctx);

    switch (opt.args.mode) {
        .sync => try processSync(allocator, ctx),
        .trend => try processTrend(allocator, ctx, opt.positional_args.items),
    }
}

fn numToString(
    allocator: std.mem.Allocator,
    n: c_int,
) []const u8 {
    return std.fmt.allocPrint(allocator, "{d}", .{n}) catch unreachable;
}

fn processSync(allocator: std.mem.Allocator, ctx: c.omg_context) !void {
    _ = allocator;
    try checkErr(c.omg_sync_created_repos(ctx));
    try checkErr(c.omg_sync_starred_repos(ctx));
    try checkErr(c.omg_sync_created_gists(ctx));
    try checkErr(c.omg_sync_starred_gists(ctx));
}

fn processTrend(allocator: std.mem.Allocator, ctx: c.omg_context, args: [][]const u8) !void {
    const language = if (args.len > 0)
        args[0]
    else
        "any";
    const since = if (args.len > 1)
        args[1]
    else
        "daily";

    var lst: c.omg_repo_list = .{
        .length = 0,
        .repo_array = null,
    };
    try checkErr(c.omg_query_trending(ctx, null, try allocator.dupeZ(u8, language), try allocator.dupeZ(u8, since), &lst));
    defer c.omg_free_repo_list(&lst);

    var i: usize = 0;
    const headers = [_][]const u8{ "Name", "Stars", "Description" };
    var table_data = std.ArrayList([headers.len][]const u8).init(allocator);
    while (i < lst.length) {
        const repo_name = std.mem.span(lst.repo_array[i].full_name);
        const description = std.mem.span(lst.repo_array[i].description);
        try table_data.append([_][]const u8{
            repo_name[0..@min(30, repo_name.len)],
            numToString(allocator, lst.repo_array[i].stargazers_count),
            description[0..@min(100, description.len)],
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
