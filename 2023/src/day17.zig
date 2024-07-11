const std = @import("std");
const utils = @import("./utils.zig");
const Field = utils.Field;
const Point = utils.Point;

pub fn main() !void {
    try utils.mainImpl(day);
}

const Status = struct {
    // Target
    point: utils.Point,
    // Total size
    dist: usize,

    // Previous dir
    from: utils.Point,
    // Straigh count
    straight: usize,
};

fn orderer(ctx: void, a: Status, b: Status) std.math.Order {
    _ = ctx;
    // if (a.dist == b.dist) {
    //     if (a.straight > b.straight)
    //         return std.math.Order.lt;
    //     return std.math.Order.gt;
    // }
    // if (a.dist < b.dist)
    //     return std.math.Order.lt;
    //
    // return std.math.Order.lt;
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

fn a_star(field: *Field(u8), dists: [3][]usize, alloc: std.mem.Allocator) !void {
    var queue = std.PriorityDequeue(Status, void, orderer).init(alloc, {});
    try queue.add(Status{
        .from = Point.new(0, 0),
        .straight = 0,
        .dist = 0,
        .point = Point.new(0, 0),
    });

    while (queue.len > 0) {
        const status = queue.removeMin();
        // const l: usize = @bitCast(field._idx(status.point.x, status.point.y));
        // _ = l;
        // const cb = dists[status.straight][l];
        // if (cb != 0 and cb < status.dist) continue;

        for ([_]Point{ Point.UP, Point.DOWN, Point.RIGHT, Point.LEFT }) |dir| {
            if (dir.x * -1 == status.from.x and dir.y * -1 == status.from.y) continue;

            var straight: usize = 0;
            if (status.from.eq(dir)) {
                straight = status.straight + 1;
                if (straight >= 3) continue;
            }

            const target = status.point.add(dir);
            if (field.get_p(target)) |c| {
                const dist = @as(usize, c - '0') + status.dist;
                const idx: usize = @bitCast(field._idx(target.x, target.y));

                for (straight..3) |i| {
                    if (dists[i][idx] == 0 or dists[i][idx] > dist) {
                        dists[i][idx] = dist;

                        try queue.add(Status{
                            .from = dir,
                            .straight = i,
                            .dist = dist,
                            .point = target,
                        });
                    }
                }
            }
        }
    }
}

fn day(contents: []const u8, allocator: std.mem.Allocator) anyerror!void {
    var field = Field(u8).init(@constCast(contents), true, undefined);
    const dists = [3][]usize{ try allocator.alloc(usize, field.contents.len), try allocator.alloc(usize, field.contents.len), try allocator.alloc(usize, field.contents.len) };
    for (0..3) |i| {
        @memset(dists[i], 0);
    }
    try a_star(&field, dists, allocator);

    // to high : 675
    std.debug.print("Part1 {}\n", .{dists[2][field.contents.len - 2]});
    std.debug.print("Part1 {}\n", .{dists[1][field.contents.len - 2]});
    std.debug.print("Part1 {}\n", .{dists[0][field.contents.len - 2]});
}
