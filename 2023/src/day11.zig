const std = @import("std");
const expect = std.testing.expect;
const utils = @import("./utils.zig");

pub fn main() !void {
    try utils.mainImpl(day);
}

const Planet = struct {
    x: usize,
    y: usize,

    fn dist(self: *const Planet, other: *const Planet) usize {
        var dx: usize = undefined;
        var dy: usize = undefined;
        if (self.x > other.x) {
            dx = self.x - other.x;
        } else {
            dx = other.x - self.x;
        }

        if (self.y > other.y) {
            dy = self.y - other.y;
        } else {
            dy = other.y - self.y;
        }

        return dx + dy;
    }
};

const Part = struct {
    x: usize,
    y: usize,
    oldness: usize,
    total: usize,
    planets: std.ArrayList(Planet),

    fn init(oldness: usize, alloc: std.mem.Allocator) Part {
        return Part{ .x = 0, .y = 0, .total = 0, .oldness = oldness, .planets = std.ArrayList(Planet).init(alloc) };
    }

    fn planet(self: *Part) !void {
        const plan = Planet{ .x = self.x, .y = self.y };
        for (self.planets.items) |p2| {
            self.total += plan.dist(&p2);
        }
        try self.planets.append(plan);
    }

    fn row(self: *Part, wide: bool) void {
        if (wide) {
            self.y += self.oldness;
        } else {
            self.y += 1;
        }
        self.x = 0;
    }
    fn col(self: *Part, wide: bool) void {
        if (wide) {
            self.x += self.oldness;
        } else {
            self.x += 1;
        }
    }
};

const Coord = [2]usize;

fn day(contents: []const u8, allocator: std.mem.Allocator) anyerror!void {
    var rows = std.ArrayList(usize).init(allocator);

    var rowWidth: usize = 0;
    while (contents[rowWidth] != '\n') {
        rowWidth += 1;
    }

    for (0..rowWidth) |col| {
        var idx = col;
        var j: usize = 0;
        while (idx < contents.len) : ({
            j += 1;
            idx = col + j * (rowWidth + 1);
        }) {
            if (contents[idx] != '.') break;
        }
        if (idx >= contents.len) {
            try rows.append(col);
        }
    }

    try rows.append(contents.len);

    var xSane: usize = 0;
    var rowIdx: usize = 0;
    var line_found = false;

    const oldness = 1000000;
    _ = oldness;

    var part1 = Part.init(2, allocator);
    var part2 = Part.init(1000000, allocator);

    for (contents) |c| {
        switch (c) {
            '#' => {
                line_found = true;
                try part1.planet();
                try part2.planet();
            },
            '\n' => {
                part1.row(!line_found);
                part2.row(!line_found);
                rowIdx = 0;
                line_found = false;
                xSane = 0;
                continue;
            },
            else => {},
        }

        const wide = rows.items[rowIdx] == xSane;
        part1.col(wide);
        part2.col(wide);
        if (wide) {
            rowIdx += 1;
        }
        xSane += 1;
    }

    std.debug.print("Part1 {}\n", .{part1.total});
    std.debug.print("Part2 {}\n", .{part2.total});
}
