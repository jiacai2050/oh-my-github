const std = @import("std");
const testing = std.testing;
const c = @cImport({
    @cInclude("omg.h");
});

pub fn init_ctx() !?*c.struct_omg_context {
    const db_path = std.c.getenv("DB_PATH").?;
    const token = std.c.getenv("GITHUB_TOKEN").?;
    var ctx: ?*c.struct_omg_context = null;
    const err = c.omg_setup_context(db_path, token, 10, &ctx);
    try testing.expect(c.is_ok(err));

    return ctx;
}

pub fn check_error(err: c.omg_error) !void {
    if (!c.is_ok(err)) {
        std.log.err("omg_error code:{d}, msg:{s}", .{ err.code, err.message });
        return error.TestUnexpectedError;
    }
}
