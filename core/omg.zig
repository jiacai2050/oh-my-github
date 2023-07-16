const std = @import("std");
const c = @cImport({
    @cInclude("omg.h");
});
const mem = std.mem;
const fmt = std.fmt;

const allocator = std.heap.c_allocator;
const Buffer = std.ArrayList(u8);
const GRAPHQL_API = "https://api.github.com/graphql";

// Copy from https://ziglang.org/learn/samples/#using-curl-from-zig
fn writeToArrayListCallback(data: *anyopaque, size: c_uint, nmemb: c_uint, user_data: *anyopaque) callconv(.C) c_uint {
    var buffer: *Buffer = @alignCast(@ptrCast(user_data));
    var typed_data: [*]u8 = @ptrCast(data);
    buffer.appendSlice(typed_data[0 .. nmemb * size]) catch return 0;
    return nmemb * size;
}

fn request(ctx: ?*c.struct_omg_context, url: [:0]const u8, method: [:0]const u8, payload: ?[:0]const u8) !Buffer {
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
    var response_buffer = Buffer.init(allocator);

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

fn query_inner(
    ctx: ?*c.struct_omg_context,
    owner: [:0]const u8,
    name: [:0]const u8,
    diag: *c.omg_error,
) !c.omg_repo_discussion_category {
    const q =
        \\ query ($owner:String!, $name:String!) {
        \\   repository(owner: $owner, name: $name) {
        \\     id
        \\     url
        \\     discussionCategories(first:100) {
        \\       edges {
        \\         node {
        \\           id
        \\           name
        \\           slug
        \\         }
        \\       }
        \\     }
        \\   }
        \\ }
    ;

    var payload = std.ArrayList(u8).init(allocator);
    defer payload.deinit();

    try std.json.stringify(.{
        .query = q,
        .variables = .{
            .owner = owner,
            .name = name,
        },
    }, .{}, payload.writer());
    try payload.append(0);

    const resp = try request(
        ctx,
        GRAPHQL_API,
        "POST",
        payload.items[0 .. payload.items.len - 1 :0],
    );
    defer resp.deinit();

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
        const msg = try fmt.allocPrintZ(allocator, "{s}", .{e[0].message});
        defer allocator.free(msg);

        diag.* = c.new_error(c.OMG_CODE_GITHUB, msg);
        return error.GitHubError;
    }

    if (json.value.data.repository) |repo| {
        const repo_id = try fmt.allocPrintZ(allocator, "{s}", .{repo.id});
        const len = repo.discussionCategories.edges.len;
        var list = try allocator.alloc(c.omg_discussion_category, len);
        for (repo.discussionCategories.edges, 0..) |edge, idx| {
            list[idx] = c.omg_discussion_category{
                .id = try fmt.allocPrintZ(allocator, "{s}", .{edge.node.id}),
                .name = try fmt.allocPrintZ(allocator, "{s}", .{edge.node.name}),
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
    const r = query_inner(ctx, mem.span(owner), mem.span(name), &diag) catch |e| {
        if (c.is_ok(diag)) {
            return c.new_error(c.OMG_CODE_INTERNAL, @errorName(e));
        }

        return diag;
    };

    out.* = r;
    return c.omg_error{ .code = c.OMG_CODE_OK, .message = .{} };
}

fn create_inner(
    ctx: c.omg_context,
    repo_id: [:0]const u8,
    category_id: [:0]const u8,
    title: [:0]const u8,
    body: [:0]const u8,
    diag: *c.omg_error,
) !c.omg_discussion {
    const mutation =
        \\ mutation($repo_id:ID!, $cat_id:ID!, $title:String!, $body:String!) {
        \\  createDiscussion(
        \\    input: {repositoryId: $repo_id, categoryId: $cat_id, title: $title, body: $body }
        \\  ) {
        \\    discussion {
        \\      id
        \\      title
        \\      body
        \\      createdAt
        \\      url
        \\    }
        \\  }
        \\ }
    ;
    var payload = Buffer.init(allocator);
    defer payload.deinit();

    try std.json.stringify(.{
        .query = mutation,
        .variables = .{
            .repo_id = repo_id,
            .cat_id = category_id,
            .title = title,
            .body = body,
        },
    }, .{}, payload.writer());
    try payload.append(0);

    const resp = try request(
        ctx,
        GRAPHQL_API,
        "POST",
        payload.items[0 .. payload.items.len - 1 :0],
    );
    defer resp.deinit();

    const CreateResult = struct {
        data: struct {
            createDiscussion: ?struct {
                discussion: struct {
                    id: []const u8,
                    url: []const u8,
                },
            } = null,
        },
        errors: ?[]struct {
            message: []const u8,
        } = null,
    };

    // std.debug.print("raw:{s}\n", .{resp.items});
    const json = try std.json.parseFromSlice(CreateResult, allocator, resp.items, .{
        .ignore_unknown_fields = true,
    });
    defer json.deinit();

    if (json.value.errors) |e| {
        const msg = try fmt.allocPrintZ(allocator, "{s}", .{e[0].message});
        defer allocator.free(msg);

        diag.* = c.new_error(c.OMG_CODE_GITHUB, msg);
        return error.GitHubError;
    }

    const dis = json.value.data.createDiscussion.?.discussion;
    return c.omg_discussion{
        .id = try fmt.allocPrintZ(allocator, "{s}", .{dis.id}),
        .url = try fmt.allocPrintZ(allocator, "{s}", .{dis.url}),
    };
}

export fn omg_create_discusstion(
    ctx: c.omg_context,
    repo_id: [*c]const u8,
    category_id: [*c]const u8,
    title: [*c]const u8,
    body: [*c]const u8,
    out: *c.omg_discussion,
) c.omg_error {
    var diag = c.omg_error{ .code = c.OMG_CODE_OK, .message = .{} };
    const r = create_inner(ctx, mem.span(repo_id), mem.span(category_id), mem.span(title), mem.span(body), &diag) catch |e| {
        if (c.is_ok(diag)) {
            return c.new_error(c.OMG_CODE_INTERNAL, @errorName(e));
        }

        return diag;
    };

    out.* = r;
    return diag;
}
