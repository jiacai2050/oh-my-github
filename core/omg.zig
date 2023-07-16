const std = @import("std");
const c = @cImport({
    @cInclude("omg.h");
});

const allocator = std.heap.c_allocator;
const ResponseBuffer = std.ArrayList(u8);
const GRAPHQL_API = "https://api.github.com/graphql";

// Copy from https://ziglang.org/learn/samples/#using-curl-from-zig
fn writeToArrayListCallback(data: *anyopaque, size: c_uint, nmemb: c_uint, user_data: *anyopaque) callconv(.C) c_uint {
    var buffer: *ResponseBuffer = @alignCast(@ptrCast(user_data));
    var typed_data: [*]u8 = @ptrCast(data);
    buffer.appendSlice(typed_data[0 .. nmemb * size]) catch return 0;
    return nmemb * size;
}

fn request(ctx: ?*c.struct_omg_context, url: [:0]const u8, method: [:0]const u8, payload: ?[:0]const u8) !ResponseBuffer {
    const handle: ?*c.CURL = c.omg__curl_handler(ctx);
    if (c.curl_easy_setopt(handle, c.CURLOPT_URL, url.ptr) != c.CURLE_OK)
        return error.CouldNotSetURL;
    if (c.curl_easy_setopt(handle, c.CURLOPT_CUSTOMREQUEST, method.ptr) != c.CURLE_OK)
        return error.CouldNotSetMethod;

    if (payload) |p| {
        if (c.curl_easy_setopt(handle, c.CURLOPT_POSTFIELDS, p.ptr) != c.CURLE_OK) {
            return error.CouldNotSetPayload;
        }
    }
    var response_buffer = ResponseBuffer.init(allocator);

    // _ = c.curl_easy_setopt(handle, c.CURLOPT_VERBOSE, @as(c_long, 10));

    if (c.curl_easy_setopt(handle, c.CURLOPT_WRITEFUNCTION, writeToArrayListCallback) != c.CURLE_OK)
        return error.CouldNotSetWriteCallback;
    if (c.curl_easy_setopt(handle, c.CURLOPT_WRITEDATA, &response_buffer) != c.CURLE_OK)
        return error.CouldNotSetWriteCallback;

    // perform
    if (c.curl_easy_perform(handle) != c.CURLE_OK)
        return error.FailedToPerformRequest;

    return response_buffer;
}

fn query_inner(ctx: ?*c.struct_omg_context, owner: []const u8, name: []const u8, diag: *c.omg_error) !c.omg_repo_discussion_category {
    var q = try std.fmt.allocPrintZ(allocator,
        \\ query {{
        \\   repository(name: "{s}", owner: "{s}") {{
        \\     id
        \\     url
        \\     discussionCategories(first:100) {{
        \\       edges {{
        \\         node {{
        \\           id
        \\           name
        \\           slug
        \\         }}
        \\       }}
        \\     }}
        \\   }}
        \\ }}
    , .{ name, owner });
    defer allocator.free(q);

    var payload = std.ArrayList(u8).init(allocator);
    defer payload.deinit();

    try std.json.stringify(.{ .query = q }, .{}, payload.writer());
    try payload.append(0);

    const resp = try request(
        ctx,
        GRAPHQL_API,
        "POST",
        payload.items[0 .. payload.items.len - 1 :0],
    );

    const CategoryList = struct {
        data: struct {
            repository: ?struct {
                id: []const u8,
                discussionCategories: struct {
                    edges: []struct {
                        node: struct {
                            id: []const u8,
                            name: []const u8,
                            slug: []const u8,
                        },
                    },
                },
            } = null,
        },
        errors: ?[]struct {
            message: []const u8,
        } = null,
    };

    const json = try std.json.parseFromSlice(CategoryList, allocator, resp.items, .{
        .ignore_unknown_fields = true,
    });
    defer json.deinit();

    try std.json.stringify(json.value, .{
        .whitespace = .{
            .indent = .{ .space = 1 },
            .separator = false,
        },
    }, std.io.getStdOut().writer());

    if (json.value.errors) |e| {
        const msg = try std.fmt.allocPrintZ(allocator, "{s}", .{e[0].message});
        defer allocator.free(msg);

        diag.* = c.new_error(c.OMG_CODE_GITHUB, msg);
        return error.GitHubError;
    }

    if (json.value.data.repository) |repo| {
        const repo_id = try std.fmt.allocPrintZ(allocator, "{s}", .{repo.id});
        const len = repo.discussionCategories.edges.len;
        var list = try allocator.alloc(c.omg_discussion_category, len);
        for (repo.discussionCategories.edges, 0..) |edge, idx| {
            list[idx] = c.omg_discussion_category{
                .id = try std.fmt.allocPrintZ(allocator, "{s}", .{edge.node.id}),
                .name = try std.fmt.allocPrintZ(allocator, "{s}", .{edge.node.name}),
            };
        }

        return c.omg_repo_discussion_category{
            .id = repo_id.ptr,
            .categories = list.ptr,
            .len = len,
        };
    } else {
        return error.EmptyCategory;
    }
}

export fn omg_query_repo_discussion_category(
    ctx: c.omg_context,
    owner: [*c]const u8,
    name: [*c]const u8,
    out: *c.omg_repo_discussion_category,
) c.omg_error {
    var diag = c.omg_error{ .code = c.OMG_CODE_OK, .message = .{} };
    const r = query_inner(ctx, std.mem.span(owner), std.mem.span(name), &diag) catch |e| {
        if (c.is_ok(diag)) {
            return c.new_error(c.OMG_CODE_INTERNAL, @errorName(e));
        }

        return diag;
    };

    out.* = r;
    return c.omg_error{ .code = c.OMG_CODE_OK, .message = .{} };
}
