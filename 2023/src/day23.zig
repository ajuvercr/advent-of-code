const std = @import("std");
const Parser = @import("./parser.zig").Parser;
const utils = @import("./utils.zig");

const Field = utils.Field(u8);
const Point = utils.Point;

pub fn main() !void {
    try utils.mainImpl(day);
}

fn find_start(field: *const Field) Point {
    var start = Point.new(0, 0);
    while (field.get_p(start)) |c| {
        if (c == '.') return start;
        start = start.add(Point.RIGHT);
    }
    return start;
}

fn find_end(field: *const Field) Point {
    var start = Point.new(0, field.col_length() - 1);
    while (field.get_p(start)) |c| {
        if (c == '.') return start;
        start = start.add(Point.RIGHT);
    }
    return start;
}

fn walk_dir(field: *Field, at: Point, end: Point, current: usize, dir: Point, allowed: u8) usize {
    var out: usize = 0;

    if (field.get_p(at.add(dir))) |x| {
        if (x == '.' or x == allowed) {
            const next = at.add(dir);
            _ = field.set_p(next, 'O');

            if (x == '.') {
                out = @max(out, walk(field, next, end, current + 1));
                _ = field.set_p(next, '.');
            } else {
                const next_x = next.add(dir);
                _ = field.set_p(next_x, 'O');
                out = @max(out, walk(field, next_x, end, current + 2));
                _ = field.set_p(next_x, '.');
                _ = field.set_p(next, allowed);
            }
        }
    }

    return out;
}

fn walk(field: *Field, at: Point, end: Point, current: usize) usize {
    if (at.eq(end)) return current;
    var out: usize = 0;

    // UP
    out = @max(out, walk_dir(field, at, end, current, Point.UP, '^'));
    out = @max(out, walk_dir(field, at, end, current, Point.DOWN, 'v'));
    out = @max(out, walk_dir(field, at, end, current, Point.RIGHT, '>'));
    out = @max(out, walk_dir(field, at, end, current, Point.LEFT, '<'));

    return out;
}

fn day(contents: []u8, allocator: std.mem.Allocator) anyerror!void {
    _ = allocator;
    var field = Field.init(contents, true, null);
    const start = find_start(&field);
    const end = find_end(&field);

    std.debug.print("Start {any} {any}\n", .{ start, end });
    std.debug.print("Part 1: {}\n", .{walk(&field, start, end, 0)});

    for (0..@intCast(field.row_length)) |i| {
        for (0..@intCast(field.col_length())) |j| {
            const y: isize = @intCast(i);
            const x: isize = @intCast(j);
            if (field.get(x, y)) |c| {
                if (c != '.' and c != '#') {
                    _ = field.set(x, y, '.');
                }
            }
        }
    }
    std.debug.print("Part 2: {}\n", .{walk(&field, start, end, 0)});
}
