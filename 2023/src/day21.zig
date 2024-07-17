const std = @import("std");
const Parser = @import("./parser.zig").Parser;
const expect = std.testing.expect;
const utils = @import("./utils.zig");

const Field = utils.Field;
const Point = utils.Point;
const Points = Point.Points;

pub fn main() !void {
    try utils.mainImpl(day);
}

const Data = struct {
    field: Field(u8),
    secondField: Field(u8),
    countField: Field(usize),
    copy: []u8,
    w: usize,
    h: usize,
    steps: usize,

    fn init(contents: []u8, allocator: std.mem.Allocator) !Data {
        var field = Field(u8).init(contents, true, undefined);

        const countFieldBuffer = try allocator.alloc(usize, contents.len);
        @memset(countFieldBuffer, std.math.maxInt(usize));

        const countField = Field(usize).init(countFieldBuffer, true, field.row_length);
        const second = try allocator.alloc(u8, contents.len);
        @memcpy(second, contents);
        const secondField = Field(u8).init(second, true, undefined);

        var copy = try allocator.alloc(u8, contents.len);
        @memcpy(copy, contents);

        const h: usize = @intCast(field.col_length());
        const w: usize = @intCast(field.row_length);

        for (0..copy.len) |i| {
            if (copy[i] == 'S') {
                copy[i] = '.';
            }
        }

        return Data{
            .field = field,
            .secondField = secondField,
            .copy = copy,
            .countField = countField,
            .w = w,
            .h = h,
            .steps = 0,
        };
    }

    fn set(self: *Data, pos: Point) void {
        @memcpy(self.field.contents, self.copy);
        _ = self.field.set_p(pos, 'S');
    }

    fn set_counts(self: *Data) void {
        for (0..self.w) |i| {
            for (0..self.h) |j| {
                const x: isize = @intCast(i);
                const y: isize = @intCast(j);
                if (self.field.get(x, y)) |c| {
                    if (c == 'S') {
                        if (self.countField.get(x, y)) |v| {
                            if (v > self.steps) {
                                _ = self.countField.set(x, y, self.steps);
                            }
                        }
                    }
                }
            }
        }
    }

    fn print_counts(self: *const Data) void {
        for (0..self.w) |i| {
            for (0..self.h) |j| {
                const y: isize = @intCast(i);
                const x: isize = @intCast(j);

                if (self.countField.get(x, y)) |v| {
                    if (v > 1000) {
                        std.debug.print(" #  ", .{});
                    } else {
                        std.debug.print("{: ^3} ", .{v});
                    }
                }
            }
            std.debug.print("\n", .{});
        }
    }

    fn step(self: *Data) void {
        @memcpy(self.secondField.contents, self.copy);

        for (0..self.w) |i| {
            for (0..self.h) |j| {
                var at = Point{ .x = @intCast(i), .y = @intCast(j) };
                if (self.field.get_p(at)) |x| {
                    if (x == 'S') {
                        for (Points) |p| {
                            const newPoint = at.add(p);
                            if (self.field.get_p(newPoint)) |c| {
                                if (c != '#') {
                                    _ = self.secondField.set_p(newPoint, 'S');
                                }
                            }
                        }
                    }
                }
            }
        }

        self.steps += 1;

        // swap things
        const tmp = self.secondField.contents;
        self.secondField.contents = self.field.contents;
        self.field.contents = tmp;
    }

    fn count(self: *const Data) usize {
        var out: usize = 0;
        for (0..self.w) |i| {
            for (0..self.h) |j| {
                const at = Point{ .x = @intCast(i), .y = @intCast(j) };
                if (self.field.get_p(at)) |x| {
                    if (x == 'S') {
                        out += 1;
                    }
                }
            }
        }

        return out;
    }
};

fn getPoint(data: *const Data, comptime x_f: fn (usize) usize, comptime y_f: fn (usize) usize) Point {
    return Point{
        .x = @intCast(x_f(data.w)),
        .y = @intCast(y_f(data.h)),
    };
}

fn part1(data: *Data, count: usize) usize {
    data.set(Point{
        .x = @intCast(data.w / 2),
        .y = @intCast(data.h / 2),
    });

    for (0..count) |z| {
        _ = z;
        data.set_counts();
        data.step();
        // std.debug.print("Step {}\n{s}\n", .{ z, data.field.contents });
    }

    return data.count();
}

fn day(contents: []u8, allocator: std.mem.Allocator) anyerror!void {
    var data = try Data.init(contents, allocator);
    std.debug.print("Part 1: {}\n", .{part1(&data, 10)});

    for (0..200) |i| {
        _ = i;
        data.set_counts();
        data.step();
    }

    const steps = 26501365;
    const steps_into = steps % data.w;
    var even_corner: usize = 0;
    var odd_corner: usize = 0;
    var even: usize = 0;
    var odd: usize = 0;

    for (0..data.w) |i| {
        for (0..data.h) |j| {
            const x: isize = @intCast(i);
            const y: isize = @intCast(j);

            if (data.countField.get(x, y)) |v| {
                if (v > 1000) continue;
                if (v % 2 == 0) {
                    even += 1;
                    if (v > steps_into) {
                        even_corner += 1;
                    }
                } else {
                    odd += 1;
                    if (v > steps_into) {
                        odd_corner += 1;
                    }
                }
            }
        }
    }
    const n = ((steps - (data.w / 2)) / data.w);
    const p2 = ((n + 1) * (n + 1)) * odd + (n * n) * even - (n + 1) * odd_corner + n * even_corner;

    std.debug.print("Part 2: {}\n", .{p2});
}
