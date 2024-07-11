const std = @import("std");
const utils = @import("./utils.zig");

pub fn main() !void {
    try utils.mainImpl(day);
}

fn spaces(par: *std.fmt.Parser) void {
    while (par.peek(0) == ' ') {
        par.pos += 1;
    }
}

fn calc_wins(time: usize, dist: usize) usize {
    const d: usize = time * time - 4 * dist;
    const df: f64 = @floatFromInt(d);

    const start = (@as(f64, @floatFromInt(time)) - std.math.sqrt(df)) * 0.5;
    const end = (@as(f64, @floatFromInt(time)) + std.math.sqrt(df)) * 0.5;

    const periode = std.math.floor(end - 0.001) - std.math.ceil(start + 0.001) + 1;
    const periode_c: usize = @intFromFloat(periode);

    return periode_c;
}

fn day(contents: []const u8, allocator: std.mem.Allocator) anyerror!void {
    var par = std.fmt.Parser{ .buf = contents };

    var times = std.mem.zeroes([10]usize);
    var time_count: usize = 0;
    par.pos += 5;
    spaces(&par);
    while (par.peek(@as(usize, 0)) != '\n') {
        times[time_count] = par.number().?;
        time_count += 1;
        spaces(&par);
    }

    var dists = std.mem.zeroes([10]usize);
    var dist_count: usize = 0;
    par.pos += 10;
    spaces(&par);
    while (par.peek(0) orelse '\n' != '\n') {
        dists[dist_count] = par.number().?;
        dist_count += 1;
        spaces(&par);
    }

    var total: usize = 1;
    for (0..dist_count) |i| {
        total *= calc_wins(times[i], dists[i]);
    }

    var dist_vec = std.ArrayList(u8).init(allocator);
    var times_vec = std.ArrayList(u8).init(allocator);
    for (0..dist_count) |i| {
        try std.fmt.format(dist_vec.writer(), "{}", .{dists[i]});
        try std.fmt.format(times_vec.writer(), "{}", .{times[i]});
    }

    const big_time = try std.fmt.parseInt(usize, times_vec.items, 10);
    const big_dist = try std.fmt.parseInt(usize, dist_vec.items, 10);

    std.debug.print("Part1 {}\n", .{total});
    std.debug.print("Part2 {}\n", .{calc_wins(big_time, big_dist)});
}
