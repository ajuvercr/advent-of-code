const std = @import("std");
const utils = @import("./utils.zig");
const Point = utils.Point;
const Field = utils.Field;

pub fn main() !void {
    try utils.mainImpl(day);
}

const Line = struct {
    start: isize,
    end: isize,
    start_up: bool,
    end_up: bool,
    y: isize,
    fn new(start: isize, start_up: bool, end: isize, end_up: bool, y: isize) Line {
        // if (start > end) {
        //     return Line{
        //         .start = end,
        //         .start_up = end_up,
        //         .end = start,
        //         .end_up = start_up,
        //         .y = y,
        //     };
        // } else {
        return Line{
            .start = start,
            .start_up = start_up,
            .end = end,
            .end_up = end_up,
            .y = y,
        };
        // }
    }

    fn len(self: *const Line) isize {
        return self.end - self.start + 1;
    }

    fn contains(self: *const Line, x: isize) bool {
        return self.start <= x and self.end >= x;
    }
};

const Data = struct {
    point: Point,
    from_up: bool,
};

fn char_to_dir(c: u8) Point {
    return switch (c) {
        'U' => Point.UP,
        'R' => Point.RIGHT,
        'D' => Point.DOWN,
        'L' => Point.LEFT,
        else => unreachable,
    };
}

fn sorter(ctx: void, a: Line, b: Line) bool {
    _ = ctx;
    if (a.y == b.y) {
        return a.start < b.start;
    }
    return a.y < b.y;
}

const Inp = struct {
    dir: Point,
    len: isize,
    fn parse(par: *std.fmt.Parser) Inp {
        const dir_c = par.char().?;
        var dir = char_to_dir(dir_c);
        par.pos += 1;
        const amount = par.number().?;
        par.pos += 2;
        const color = par.until(')');
        _ = color;
        par.pos += 2;
        return Inp{
            .dir = dir,
            .len = @bitCast(amount),
        };
    }

    fn dx(self: *const Inp) isize {
        if (self.dir.eq(Point.RIGHT)) return self.len;
        // Going to the left
        return -1 * self.len;
    }

    fn dy(self: *const Inp) isize {
        if (self.dir.eq(Point.DOWN)) return self.len;
        return -1 * self.len;
    }
};

fn day(contents: []const u8, allocator: std.mem.Allocator) anyerror!void {
    var par = std.fmt.Parser{ .buf = contents };
    var total: isize = 0;
    var total2: usize = 0;
    _ = total2;

    var location = Point.new(0, 0);
    _ = location;

    var points = std.ArrayList(Point).init(allocator);
    defer points.deinit();

    var lines = std.ArrayList(Line).init(allocator);
    defer lines.deinit();

    var at = Point.new(0, 0);
    var from_down = true;
    while (par.peek(0) != undefined) {
        const hor = Inp.parse(&par);
        const vert = Inp.parse(&par);

        const next = at.add(Point.new(hor.dx(), 0));

        // We have to create a new line
        if (hor.dir.eq(Point.LEFT)) {
            try lines.append(
                Line.new(next.x, vert.dir.eq(Point.UP), at.x, !from_down, next.y),
            );
        } else {
            try lines.append(
                Line.new(at.x, !from_down, next.x, vert.dir.eq(Point.UP), next.y),
            );
        }

        from_down = vert.dir.eq(Point.UP);
        at = next.add(Point.new(0, vert.dy()));
    }

    var current_lines = std.ArrayList(Line).init(allocator);
    defer current_lines.deinit();

    std.sort.heap(Line, lines.items, {}, sorter);
    var current_y = lines.items[0].y - 1;

    for (lines.items) |incoming| {
        if (incoming.y != current_y) {
            const dy = incoming.y - current_y;
            std.debug.print("dy {} - {} = {any}\n", .{ incoming.y, current_y, dy });

            // Let's handle all blocks build by lines
            for (current_lines.items) |seg| {
                total += dy * seg.len();
            }

            current_y = incoming.y;
        }

        var contains_end: ?Line = null;
        var contains_end_idx: ?usize = null;
        var contains_start: ?Line = null;
        var contains_start_idx: ?usize = null;

        for (0..current_lines.items.len) |i| {
            var cl = current_lines.items[i];

            // I thikn
            // if (incoming.start > cl.start and incoming.end < cl.end) {
            //     _ = current_lines.orderedRemove(idx);
            // }

            if (cl.contains(incoming.start)) {
                contains_start = cl;
                contains_start_idx = i;
            }

            if (cl.contains(incoming.end)) {
                contains_end_idx = i;
                contains_end = cl;
            }
        }

        if (contains_end == null or contains_start == null) {
            try current_lines.append(incoming);
        }

        if (contains_start_idx) |start| {
            current_lines.items[start].end = incoming.start - 1;
        }

        if (contains_end_idx) |end| {
            current_lines.items[end].start = incoming.end + 1;
        }

        var idx_rev = current_lines.items.len;
        while (idx_rev > 0) {
            idx_rev -= 1;
            var cl = current_lines.items[idx_rev];
            if (cl.len() <= 0) {
                const out = current_lines.orderedRemove(idx_rev);
                std.debug.print("Removing {}  len={}\n", .{ out, out.len() });
            }
        }

        for (current_lines.items) |line| {
            std.debug.print("Y={} {any}\n", .{ current_y, line });
        }
    }

    const dy = lines.getLast().y + 1 - current_y;
    for (current_lines.items) |seg| {
        total += dy * seg.len();
    }

    for (lines.items) |line| {
        std.debug.print("Part1 {any}\n", .{line});
    }
    std.debug.print("Part1 {any}\n", .{total});
}
