const std = @import("std");
const Parser = @import("./parser.zig").Parser;
const utils = @import("./utils.zig");

pub fn main() !void {
    try utils.mainImpl(day);
}

fn calc_deltas(prev: std.ArrayList(isize), alloc: std.mem.Allocator) !std.ArrayList(isize) {
    var nums = std.ArrayList(isize).init(alloc);

    for (1..prev.items.len) |i| {
        try nums.append(prev.items[i] - prev.items[i - 1]);
    }

    return nums;
}

fn print_numss(numss: std.ArrayList(std.ArrayList(isize))) void {
    std.debug.print("\n", .{});
    for (0..numss.items.len) |i| {
        for (0..i) |_| {
            std.debug.print("  ", .{});
        }

        std.debug.print("{any}\n", .{numss.items[i].items});
    }
}

fn ready(items: []const isize) bool {
    for (items) |x| {
        if (x != 0) return false;
    }
    return true;
}

fn calc_next(items: *std.ArrayList(isize), last: isize, back: bool) !isize {
    if (back) {
        const myLast = items.items[0];
        try items.append(myLast - last);
        return myLast - last;
    } else {
        const myLast = items.getLast();
        try items.append(myLast + last);
        return myLast + last;
    }
}

pub fn day(contents: []const u8, allocator: std.mem.Allocator) anyerror!void {
    var par = Parser.init(contents);

    var total: i64 = 0;
    var total2: i64 = 0;
    while (par.peek(0) orelse '\n' != '\n') {
        var numss = std.ArrayList(std.ArrayList(isize)).init(allocator);
        defer numss.deinit();
        var nums = std.ArrayList(isize).init(allocator);
        defer {
            for (numss.items) |x| x.deinit();
        }

        while (par.peek(0) orelse '\n' != '\n') {
            var num: isize = undefined;
            if (par.peek(0) == '-') {
                par.pos += 1;
                num = -1 * par.number(isize).?;
            } else {
                num = par.number(isize).?;
            }
            try nums.append(num);
            if (par.char() != ' ') break;
        }
        try numss.append(nums);

        while (!ready(numss.getLast().items)) {
            const next = try calc_deltas(numss.getLast(), allocator);
            try numss.append(next);
        }

        var last: isize = 0;
        var lastBack: isize = 0;
        for (1..numss.items.len) |i| {
            const idx = numss.items.len - 1 - i;
            last = try calc_next(&numss.items[idx], last, false);
            lastBack = try calc_next(&numss.items[idx], lastBack, true);
        }

        total += last;
        total2 += lastBack;
    }

    std.debug.print("Part1 {}\n", .{total});
    std.debug.print("Part2 {}\n", .{total2});
}
