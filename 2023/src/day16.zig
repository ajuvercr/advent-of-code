const std = @import("std");
const utils = @import("./utils.zig");
const Point = utils.Point;
const Field = utils.Field;

pub fn main() !void {
    try utils.mainImpl(day);
}

fn dir_hor(dir: Point) bool {
    return dir.y == 0;
}

fn sim_light(point: Point, dir: Point, field: *Field(u8), energy_field: *Field(usize), done: *std.AutoHashMap(isize, [4]Point)) !void {
    const found = try done.getOrPut(field._idx(point.x, point.y));
    if (!found.found_existing) {
        found.value_ptr[0] = Point.new(0, 0);
        found.value_ptr[1] = Point.new(0, 0);
        found.value_ptr[2] = Point.new(0, 0);
        found.value_ptr[3] = Point.new(0, 0);
    }
    for (0..4) |i| {
        const p = found.value_ptr[i];
        if (p.eq(Point.new(0, 0))) {
            found.value_ptr[i] = dir;
            break;
        }
        if (dir.eq(p)) return;
    }
    const next = point.add(dir);
    if (field.get_p(next)) |c| {
        const v = energy_field.get_p(next).?;
        _ = energy_field.set_p(next, v + 1);

        switch (c) {
            '.' => {
                try sim_light(next, dir, field, energy_field, done);
            },
            '-' => {
                if (dir_hor(dir)) {
                    try sim_light(next, dir, field, energy_field, done);
                } else {
                    try sim_light(next, Point.LEFT, field, energy_field, done);
                    try sim_light(next, Point.RIGHT, field, energy_field, done);
                }
            },
            '|' => {
                if (dir_hor(dir)) {
                    try sim_light(next, Point.UP, field, energy_field, done);
                    try sim_light(next, Point.DOWN, field, energy_field, done);
                } else {
                    try sim_light(next, dir, field, energy_field, done);
                }
            },
            '/' => {
                if (dir.eq(Point.RIGHT)) {
                    try sim_light(next, Point.UP, field, energy_field, done);
                }
                if (dir.eq(Point.UP)) {
                    try sim_light(next, Point.RIGHT, field, energy_field, done);
                }
                if (dir.eq(Point.LEFT)) {
                    try sim_light(next, Point.DOWN, field, energy_field, done);
                }
                if (dir.eq(Point.DOWN)) {
                    try sim_light(next, Point.LEFT, field, energy_field, done);
                }
            },
            '\\' => {
                if (dir.eq(Point.UP)) {
                    try sim_light(next, Point.LEFT, field, energy_field, done);
                }
                if (dir.eq(Point.LEFT)) {
                    try sim_light(next, Point.UP, field, energy_field, done);
                }
                if (dir.eq(Point.RIGHT)) {
                    try sim_light(next, Point.DOWN, field, energy_field, done);
                }
                if (dir.eq(Point.DOWN)) {
                    try sim_light(next, Point.RIGHT, field, energy_field, done);
                }
            },
            else => {
                std.debug.print("Unexpected character {c}\n", .{c});
                unreachable;
            },
        }
    } else {
        return;
    }
}

fn energized(start: Point, dir: Point, field: *Field(u8), allocator: std.mem.Allocator) !usize {
    const energized_content = try allocator.alloc(usize, field.contents.len);
    defer allocator.free(energized_content);
    @memset(energized_content, 0);

    var energy_field = Field(usize).init(energized_content, true, field.row_length);

    var done = std.AutoHashMap(isize, [4]Point).init(allocator);
    defer done.deinit();

    try sim_light(start, dir, field, &energy_field, &done);

    var total: usize = 0;
    for (energy_field.contents) |c| {
        if (c > 0) total += 1;
    }

    return total;
}

fn day(contents: []const u8, allocator: std.mem.Allocator) anyerror!void {
    var field = Field(u8).init(@constCast(contents), true, undefined);

    var total2: usize = 0;

    const col_length = field.col_length();

    std.debug.print("Part1 {}\n", .{try energized(Point.new(-1, 0), Point.RIGHT, &field, allocator)});
    for (0..@bitCast(field.row_length)) |i| {
        const c1 = try energized(Point.new(@bitCast(i), -1), Point.DOWN, &field, allocator);
        total2 = @max(total2, c1);
        const c2 = try energized(Point.new(@bitCast(i), col_length), Point.UP, &field, allocator);
        total2 = @max(total2, c2);
    }
    for (0..@bitCast(col_length)) |i| {
        const c1 = try energized(Point.new(-1, @bitCast(i)), Point.RIGHT, &field, allocator);
        total2 = @max(total2, c1);
        const c2 = try energized(Point.new(field.row_length, @bitCast(i)), Point.LEFT, &field, allocator);
        total2 = @max(total2, c2);
    }
    std.debug.print("Part2 {}\n", .{total2});
}
