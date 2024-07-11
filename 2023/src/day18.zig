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
        return self.end - self.start;
    }

    fn contains(self: *const Line, x: isize) bool {
        return self.start <= x and self.end >= x;
    }

    pub fn format(
        self: Line,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("Line(y={} {}-{})", .{ self.y, self.start, self.end });
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
        const dir = char_to_dir(dir_c);
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

fn sorter_x(ctx: void, a: Line, b: Line) bool {
    _ = ctx;
    return a.start < b.start;
}

fn clean(lines: *std.ArrayList(Line)) void {
    std.sort.heap(Line, lines.items, {}, sorter_x);

    var idx = lines.items.len;
    while (idx > 1) {
        idx -= 1;
        const a = lines.items[idx - 1];
        if (a.len() <= 0) {
            _ = lines.orderedRemove(idx - 1);
            continue;
        }
        const b = lines.items[idx];
        if (b.len() <= 0) {
            _ = lines.orderedRemove(idx);
            continue;
        }

        if (a.end == b.start) {
            lines.items[idx - 1].end = b.end;
            _ = lines.orderedRemove(idx);
        }
    }
    if (lines.items.len > 0) {
        if (lines.items[0].len() <= 0) {
            _ = lines.orderedRemove(0);
        }
    }
}

fn day(contents: []const u8, allocator: std.mem.Allocator) anyerror!void {
    var par = std.fmt.Parser{ .buf = contents };
    var total: isize = 0;

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
                Line.new(next.x, vert.dir.eq(Point.UP), at.x + 1, !from_down, next.y),
            );
        } else {
            try lines.append(
                Line.new(at.x, !from_down, next.x + 1, vert.dir.eq(Point.UP), next.y),
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
                std.debug.print("seg len {} * {} =  {}\n", .{ dy, seg.len(), dy * seg.len() });
                total += dy * seg.len();
            }
            std.debug.print("Total: {}\n", .{total});

            current_y = incoming.y;
        }

        std.debug.print("Handling {}\n", .{incoming});
        var should_add = true;

        for (0..current_lines.items.len) |i| {
            const cl = current_lines.items[i];

            if (incoming.start <= cl.end and incoming.end >= cl.start) {
                // if (incoming.end == cl.end and incoming.start == cl.start) {
                //     std.debug.print("Exact match {}\n", .{cl});
                //     total += cl.len();
                //     current_lines.items[i].end = cl.start;
                //     continue;
                // }

                if (incoming.end == cl.end) {
                    should_add = false;
                    total += cl.end - incoming.start - 1;
                    std.debug.print("End got shorter {} (adding {})\n", .{ cl, current_lines.items[i].end - incoming.start - 1 });
                    current_lines.items[i].end = incoming.start + 1;
                }

                if (incoming.start == cl.start) {
                    should_add = false;
                    total += incoming.end - 1 + cl.start;
                    std.debug.print("Start got shorter {} (adding {})\n", .{ cl, incoming.end - 1 + current_lines.items[i].start });
                    current_lines.items[i].start = incoming.end - 1;
                }

                if (incoming.start == cl.end - 1) {
                    std.debug.print("End got longer {}\n", .{cl});
                    should_add = false;
                    current_lines.items[i].end = incoming.end;
                }

                if (incoming.end - 1 == cl.start) {
                    std.debug.print("Start got longer {}\n", .{cl});
                    should_add = false;
                    current_lines.items[i].start = incoming.start;
                }
            }
        }

        if (should_add) {
            try current_lines.append(incoming);
        }

        std.debug.print(" >>> Preclean\n", .{});
        for (current_lines.items) |line| {
            std.debug.print("Y={} {any}\n", .{ current_y, line });
        }
        clean(&current_lines);
        std.debug.print(" <<< PostClean\n", .{});
        for (current_lines.items) |line| {
            std.debug.print("Y={} {any}\n", .{ current_y, line });
        }
    }

    std.debug.print("Part1 {any}\n", .{total});
}
