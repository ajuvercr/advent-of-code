const std = @import("std");
const utils = @import("./utils.zig");

pub fn main() !void {
    try utils.mainImpl(day);
}
const Ball = enum {
    blue,
    red,
    green,

    fn isValid(self: Ball, amount: usize) bool {
        return switch (self) {
            Ball.red => amount <= 12,
            Ball.green => amount <= 13,
            Ball.blue => amount <= 14,
        };
    }
};

fn parse_color(par: *std.fmt.Parser) ?Ball {
    if (par.char()) |c| {
        switch (c) {
            'b' => {
                par.pos += 3;
                return Ball.blue;
            },
            'g' => {
                par.pos += 4;
                return Ball.green;
            },
            'r' => {
                par.pos += 2;
                return Ball.red;
            },
            else => {
                return null;
            },
        }
    }
    return null;
}

const Counts = struct {
    minBlue: usize,
    minRed: usize,
    minGreen: usize,

    fn maybeSet(self: *Counts, ball: Ball, amount: usize) void {
        switch (ball) {
            Ball.blue => self.minBlue = @max(self.minBlue, amount),
            Ball.red => self.minRed = @max(self.minRed, amount),
            Ball.green => self.minGreen = @max(self.minGreen, amount),
        }
    }

    fn val(self: *const Counts) usize {
        return self.minBlue * self.minRed * self.minGreen;
    }

    fn valid(self: *const Counts) bool {
        return self.minRed <= 12 and self.minGreen <= 13 and self.minBlue <= 14;
    }
};

fn maybeSet(val: *usize, amount: usize) void {
    if (val.* < amount) {
        val.* = amount;
    }
}

fn day(contents: []const u8, allocator: std.mem.Allocator) anyerror!void {
    _ = allocator;
    var par = std.fmt.Parser{ .buf = contents };
    var total: usize = 0;
    var total2: usize = 0;

    while (par.peek(0) != undefined) : (par.pos += 1) {
        var counts = Counts{ .minGreen = 0, .minRed = 0, .minBlue = 0 };
        par.pos += 5;
        const id = par.number().?;

        while (par.peek(0) orelse '\n' != '\n') {
            par.pos += 2;
            const amount = par.number().?;
            par.pos += 1;
            const col = parse_color(&par).?;

            counts.maybeSet(col, amount);
        }

        if (counts.valid()) {
            total += id;
        }

        total2 += counts.val();
    }

    std.debug.print("Part1 {}\n", .{total});
    std.debug.print("Part2 {}\n", .{total2});
}
