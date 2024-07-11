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

fn day(contents: []const u8, allocator: std.mem.Allocator) anyerror!void {
    _ = allocator;
    var par = std.fmt.Parser{ .buf = contents };
    var total: usize = 0;

    var values = std.mem.zeroes([215]usize);
    var wins = std.mem.zeroes([215]usize);

    var index: usize = 0;
    while (par.peek(0) != undefined) : (par.pos += 1) {
        var winning = std.mem.zeroes([20]usize);
        var yours = std.mem.zeroes([100]usize);
        var winning_idx: usize = 0;
        var yours_idx: usize = 0;
        par.pos += 4;
        spaces(&par);
        const id = par.number().?;
        _ = id;
        par.pos += 1;
        spaces(&par);
        while (par.peek(0) != '|') {
            winning[winning_idx] = par.number().?;
            spaces(&par);
            winning_idx += 1;
        }
        par.pos += 1;
        spaces(&par);
        while (par.peek(0) != '\n') {
            yours[yours_idx] = par.number().?;
            spaces(&par);
            yours_idx += 1;
        }

        var count: usize = 0;
        for (0..yours_idx) |i| {
            for (0..winning_idx) |j| {
                if (winning[j] == yours[i]) {
                    count += 1;
                }
            }
        }

        if (count > 0) {
            total += std.math.powi(usize, 2, count - 1) catch unreachable;
        }
        wins[index] = count;
        index += 1;
    }

    var i: usize = index - 1;
    while (i >= 0) : (i -= 1) {
        values[i] = 1;
        for (0..wins[i] + 1) |j| {
            if (j != 0)
                values[i] += values[i + j];
        }

        if (i == 0) break;
    }

    var total2: usize = 0;
    for (0..index) |x| {
        total2 += values[x];
    }

    std.debug.print("Part1 {}\n", .{total});
    std.debug.print("Part2 {}\n", .{total2});
}
