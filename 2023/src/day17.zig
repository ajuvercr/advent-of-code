const std = @import("std");
const utils = @import("./utils.zig");
const Field = utils.Field;
const Point = utils.Point;
const Points = Point.Points;

pub fn main() !void {
    try utils.mainImpl(day);
}

const Status = struct {
    // Target
    point: utils.Point,
    // Total size
    dist: usize,

    // Previous dir
    from: usize,
    // Straigh count
    straight: usize,
};

fn orderer(ctx: void, a: Status, b: Status) std.math.Order {
    _ = ctx;

    if (a.point.eq(b.point)) {
        if (a.dist < b.dist)
            return std.math.Order.lt;

        if (a.dist > b.dist)
            return std.math.Order.gt;

        if (a.straight < b.straight)
            return std.math.Order.lt;
        if (a.straight > b.straight)
            return std.math.Order.gt;

        return std.math.Order.eq;
    }

    if (a.dist < b.dist)
        return std.math.Order.lt;
    if (a.dist > b.dist)
        return std.math.Order.gt;

    return std.math.Order.eq;
}

fn a_star(field: *Field(u8), dists: [4][][]usize, alloc: std.mem.Allocator, minLen: usize, maxLen: usize) !void {
    var queue = std.PriorityDequeue(Status, void, orderer).init(alloc, {});
    defer queue.deinit();

    try queue.add(Status{
        .from = 5,
        .straight = 0,
        .dist = 0,
        .point = Point.new(0, 0),
    });

    while (queue.len > 0) {
        const status = queue.removeMin();
        const this_idx: usize = @bitCast(field._idx(status.point.x, status.point.y));

        if (status.straight >= minLen) {
            if (dists[status.from][status.straight - minLen][this_idx] <= status.dist) continue;
            // for (minLen..status.straight + 1) |i| {
            dists[status.from][status.straight - minLen][this_idx] = status.dist;
            // }
        }
        // std.debug.print("Handling {any}\n", .{status});

        for (0..4) |dir_idx| {
            const dir = Points[@as(usize, dir_idx)];
            // You cannot go back
            if ((dir_idx + 2) % 4 == status.from) continue;

            // If it is the same direction, add straigh 1
            if (status.from == 5 or status.from == dir_idx) {
                const straight = status.straight + 1;
                if (straight >= maxLen) {
                    continue;
                }
                const target = status.point.add(dir);
                if (field.get_p(target)) |c| {
                    const dist = @as(usize, c - '0') + status.dist;

                    try queue.add(Status{
                        .from = dir_idx,
                        .straight = straight,
                        .dist = dist,
                        .point = target,
                    });
                }
            } else {
                // Do min dist

                var target = status.point;
                var dist = status.dist;
                var good = true;
                var straight: usize = 0;

                for (0..minLen) |i| {
                    _ = i;
                    straight += 1;
                    target = target.add(dir);
                    if (field.get_p(target)) |c| {
                        dist += @as(usize, c - '0');
                    } else {
                        good = false;
                        break;
                    }
                }

                if (good) {
                    try queue.add(Status{
                        .from = dir_idx,
                        .straight = straight,
                        .dist = dist,
                        .point = target,
                    });
                }
            }
        }
    }
}

fn part(contents: []const u8, allocator: std.mem.Allocator, min_l: usize, max_l: usize) !usize {
    var field = Field(u8).init(@constCast(contents), true, undefined);
    var dists: [4][][]usize = .{undefined} ** 4;

    const diff = max_l - min_l;
    for (0..4) |i| {
        dists[i] = try allocator.alloc([]usize, diff);
        for (0..diff) |j| {
            dists[i][j] = try allocator.alloc(usize, contents.len);
            @memset(dists[i][j], std.math.maxInt(usize));
        }
    }

    defer {
        for (0..4) |i| {
            for (0..diff) |j| {
                allocator.free(dists[i][j]);
            }
            allocator.free(dists[i]);
        }
    }

    try a_star(&field, dists, allocator, min_l, max_l);

    const idx: usize = @intCast(field._idx(field.row_length - 1, field.col_length() - 1));

    var min: usize = std.math.maxInt(usize);
    for (0..4) |i| {
        for (0..diff) |j| {
            min = @min(min, dists[i][j][idx]);
        }
    }

    return min;
}

fn day(contents: []const u8, allocator: std.mem.Allocator) anyerror!void {
    std.debug.print("Part 1: {}\n", .{try part(contents, allocator, 1, 4)});
    std.debug.print("Part 2: {}\n", .{try part(contents, allocator, 4, 11)});
}
