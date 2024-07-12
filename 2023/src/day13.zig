const std = @import("std");
const Parser = @import("./parser.zig").Parser;
const utils = @import("./utils.zig");

pub fn main() !void {
    try utils.mainImpl(day);
}

const Square = struct {
    line: std.ArrayList([50]u8),
    height: usize,
    width: usize,

    fn init(alloc: std.mem.Allocator) Square {
        return Square{ .line = std.ArrayList([50]u8).init(alloc), .height = 0, .width = 0 };
    }
    fn deinit(self: *Square) void {
        self.line.deinit();
    }

    fn add_line(self: *Square, line: []const u8) !void {
        self.width = line.len;
        self.height += 1;

        var nline = std.mem.zeroes([50]u8);
        for (0..line.len) |i| {
            nline[i] = line[i];
        }
        try self.line.append(nline);
    }

    fn get(self: *Square, x: usize, y: usize) ?u8 {
        if (x >= self.width) return null;
        if (y >= self.height) return null;
        return self.line.items[y][x];
    }

    fn check_hor_mirror(self: *Square, smudge: usize) ?usize {
        outer: for (1..self.height) |i| {
            var smudged: usize = 0;
            for (0..i) |j| {
                const rowa = i - j - 1;
                const rowb = i + j;
                if (rowb >= self.height) continue;
                for (0..self.width) |col| {
                    if (!(self.get(col, rowa) == self.get(col, rowb))) {
                        smudged += 1;
                        if (smudged > smudge) {
                            continue :outer;
                        }
                    }
                }
            }
            if (smudge == smudged) {
                return i;
            }
        }

        return null;
    }

    fn check_vert_mirror(self: *Square, smudge: usize) ?usize {
        outer: for (1..self.width) |i| {
            var smudged: usize = 0;
            for (0..i) |j| {
                const cola = i - j - 1;
                const colb = i + j;
                if (colb >= self.width) continue;
                for (0..self.height) |row| {
                    if (!(self.get(cola, row) == self.get(colb, row))) {
                        smudged += 1;
                        if (smudged > smudge) {
                            continue :outer;
                        }
                    }
                }
            }
            if (smudge == smudged) {
                return i;
            }
        }

        return null;
    }

    fn check_mirror(self: *Square, smudge: usize) usize {
        if (self.check_vert_mirror(smudge)) |i| {
            return i;
        }
        return 100 * self.check_hor_mirror(smudge).?;
    }
};

fn day(contents: []const u8, allocator: std.mem.Allocator) anyerror!void {
    var par = Parser.init(contents);
    var total: usize = 0;
    var total2: usize = 0;

    var sq = Square.init(allocator);
    while (par.peek(0) != undefined) : (par.pos += 1) {
        const line = par.until('\n');
        if (line.len == 0) {
            total += sq.check_mirror(0);
            total2 += sq.check_mirror(1);
            //  handle the thing
            sq.deinit();
            sq = Square.init(allocator);
        } else {
            // build the thing
            try sq.add_line(line);
        }
    }

    total += sq.check_mirror(0);
    total2 += sq.check_mirror(1);

    std.debug.print("Part1 {}\n", .{total});
    std.debug.print("Part2 {}\n", .{total2});
}
