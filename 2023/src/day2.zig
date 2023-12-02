const std = @import("std");
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

const Game = struct {
    value: usize,
    name: []const u8,
};

fn maybeSet(val: *usize, amount: usize) void {
    if (val.* < amount) {
        val.* = amount;
    }
}

pub fn day(file: []const u8) !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var file2 = try std.fs.cwd().openFile(file, .{});
    const contents = try file2.readToEndAlloc(allocator, 200000);
    defer allocator.free(contents);

    var par = std.fmt.Parser{ .buf = contents };
    var total: usize = 0;
    var total2: usize = 0;

    while (par.peek(0) != undefined) : (par.pos += 1) {
        par.pos += 5;
        const id = par.number().?;
        total += id;

        var valid = true;

        var minBlue: usize = 0;
        var minRed: usize = 0;
        var minGreen: usize = 0;

        while (par.peek(0) orelse '\n' != '\n') {
            par.pos += 2;
            const amount = par.number().?;
            par.pos += 1;
            const col = parse_color(&par).?;

            switch (col) {
                Ball.blue => maybeSet(&minBlue, amount),
                Ball.red => maybeSet(&minRed, amount),
                Ball.green => maybeSet(&minGreen, amount),
            }

            if (!col.isValid(amount) and valid) {
                valid = false;
                total -= id;
            }
        }

        total2 += minGreen * minRed * minBlue;
    }

    std.debug.print("Part1 {}\n", .{total});
    std.debug.print("Part2 {}\n", .{total2});
}
