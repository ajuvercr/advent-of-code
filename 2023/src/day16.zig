const std = @import("std");
const utils = @import("./utils.zig");

const Point = struct {
    x: isize,
    y: isize,

    fn new(x: isize, y: isize) Point {
        return Point{
            .x = x,
            .y = y,
        };
    }

    fn add(self: Point, other: Point) Point {
        return Point{
            .x = self.x + other.x,
            .y = self.y + other.y,
        };
    }

    fn eq(self: Point, other: Point) bool {
        return self.x == other.x and self.y == other.y;
    }
};

const UP = Point.new(0, -1);
const DOWN = Point.new(0, 1);
const LEFT = Point.new(-1, 0);
const RIGHT = Point.new(1, 0);

fn Field(comptime T: type) type {
    return struct {
        contents: []T,
        row_length: isize,
        has_newline: bool,
        const Self = @This();

        fn init(contents: []T, has_newline: bool, row_length: ?isize) Self {
            var rl: usize = 0;
            if (row_length) |len| {
                return Self{
                    .contents = contents,
                    .has_newline = has_newline,
                    .row_length = len,
                };
            } else {
                if (T != u8) {
                    @panic("Can only find row length for character based fields");
                }
                while (contents[rl] != '\n') {
                    rl += 1;
                }
                return Self{
                    .contents = contents,
                    .has_newline = has_newline,
                    .row_length = @bitCast(rl),
                };
            }
        }

        fn _idx(self: Self, x: isize, y: isize) isize {
            if (self.has_newline) {
                return y * (self.row_length + 1) + x;
            } else {
                return y * self.row_length + x;
            }
        }

        fn get(self: Self, x: isize, y: isize) ?T {
            const idx = self._idx(x, y);
            if (x >= self.row_length or x < 0) return undefined;
            if (idx >= 0 and idx < self.contents.len) {
                return self.contents[@bitCast(idx)];
            }
            return undefined;
        }

        fn get_p(self: Self, point: Point) ?T {
            return self.get(point.x, point.y);
        }

        fn set(self: Self, x: isize, y: isize, item: T) bool {
            const idx = self._idx(x, y);
            if (idx >= 0 and idx < self.contents.len) {
                self.contents[@bitCast(idx)] = item;
                return true;
            }
            return false;
        }

        fn set_p(self: Self, point: Point, item: T) bool {
            return self.set(point.x, point.y, item);
        }

        fn col_length(self: Self) isize {
            const cl: isize = @bitCast(self.contents.len);
            if (self.has_newline) {
                return @divFloor(cl, self.row_length + 1);
            } else {
                return @divFloor(cl, self.row_length);
            }
        }
    };
}

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
                    try sim_light(next, LEFT, field, energy_field, done);
                    try sim_light(next, RIGHT, field, energy_field, done);
                }
            },
            '|' => {
                if (dir_hor(dir)) {
                    try sim_light(next, UP, field, energy_field, done);
                    try sim_light(next, DOWN, field, energy_field, done);
                } else {
                    try sim_light(next, dir, field, energy_field, done);
                }
            },
            '/' => {
                if (dir.eq(RIGHT)) {
                    try sim_light(next, UP, field, energy_field, done);
                }
                if (dir.eq(UP)) {
                    try sim_light(next, RIGHT, field, energy_field, done);
                }
                if (dir.eq(LEFT)) {
                    try sim_light(next, DOWN, field, energy_field, done);
                }
                if (dir.eq(DOWN)) {
                    try sim_light(next, LEFT, field, energy_field, done);
                }
            },
            '\\' => {
                if (dir.eq(UP)) {
                    try sim_light(next, LEFT, field, energy_field, done);
                }
                if (dir.eq(LEFT)) {
                    try sim_light(next, UP, field, energy_field, done);
                }
                if (dir.eq(RIGHT)) {
                    try sim_light(next, DOWN, field, energy_field, done);
                }
                if (dir.eq(DOWN)) {
                    try sim_light(next, RIGHT, field, energy_field, done);
                }
            },
            else => {
                unreachable;
            },
        }
    } else {
        return;
    }
}

fn energized(start: Point, dir: Point, field: *Field(u8), allocator: std.mem.Allocator) !usize {
    var energized_content = try allocator.alloc(usize, field.contents.len);
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

    std.debug.print("Part1 {}\n", .{try energized(Point.new(-1, 0), RIGHT, &field, allocator)});
    for (0..@bitCast(field.row_length)) |i| {
        const c1 = try energized(Point.new(@bitCast(i), -1), DOWN, &field, allocator);
        total2 = @max(total2, c1);
        const c2 = try energized(Point.new(@bitCast(i), col_length), UP, &field, allocator);
        total2 = @max(total2, c2);
    }
    for (0..@bitCast(col_length)) |i| {
        const c1 = try energized(Point.new(-1, @bitCast(i)), RIGHT, &field, allocator);
        total2 = @max(total2, c1);
        const c2 = try energized(Point.new(field.row_length, @bitCast(i)), LEFT, &field, allocator);
        total2 = @max(total2, c2);
    }
    std.debug.print("Part2 {}\n", .{total2});
}
