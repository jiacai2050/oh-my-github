const std = @import("std");
const util = @import("util.zig");
const testing = std.testing;
const time = std.time;

const c = @cImport({
    @cInclude("omg.h");
});

pub fn main() !void {
    const ctx = try util.init_ctx();

    const repo_id = "R_kgDOAScDVQ";
    const category_id = "DIC_kwDOAScDVc4CSP-y";
    var buf = std.mem.zeroes([40]u8);
    const title = try std.fmt.bufPrintZ(&buf, "Awesome Title-{d}", .{time.milliTimestamp()});
    const body = "## test from omg\n Succeed!";
    var out = c.omg_discussion{ .url = null, .id = null };
    try util.check_error(c.omg_create_discusstion(ctx, repo_id, category_id, title, body, &out));
    defer {
        std.c.free(out.url);
        std.c.free(out.id);
    }

    const url = std.mem.span(out.url);
    try testing.expect(std.mem.indexOf(u8, url, "jiacai2050") != null);
    try testing.expect(out.id != null);
    // std.debug.print("{s}-{s}\n", .{ out.url, out.id });
}
