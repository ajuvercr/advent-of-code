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
    copy: []u8,
    w: usize,
    h: usize,

    fn init(contents: []u8, allocator: std.mem.Allocator) !Data {
        var field = Field(u8).init(@constCast(contents), true, undefined);

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
            .w = w,
            .h = h,
        };
    }

    fn set(self: *Data, pos: Point) void {
        @memcpy(self.field.contents, self.copy);
        _ = self.field.set_p(pos, 'S');
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

const Cached = struct {
    n: []usize,
    s: []usize,
    e: []usize,
    w: []usize,
    ne: []usize,
    nw: []usize,
    se: []usize,
    sw: []usize,

    width: usize,

    fn go_dia(self: *const Cached, togo: usize, cached: []usize) usize {
        // std.debug.print("Dia togo {}\n", .{togo});
        const tileDiff = self.width + self.width;

        const times = togo / tileDiff;
        const extra = @mod(togo, tileDiff);

        return cached[extra] + cached[tileDiff - 1] * times;
    }

    fn go_ne(self: *const Cached, togo: usize) usize {
        return self.go_dia(togo, self.ne);
    }

    fn go_nw(self: *const Cached, togo: usize) usize {
        return self.go_dia(togo, self.nw);
    }

    fn go_se(self: *const Cached, togo: usize) usize {
        return self.go_dia(togo, self.se);
    }

    fn go_sw(self: *const Cached, togo: usize) usize {
        return self.go_dia(togo, self.sw);
    }

    fn go_straight(self: *const Cached, t: usize, this: []usize, left: []usize, right: []usize) usize {
        var togo = t;
        var out: usize = 0;
        while (togo > self.width) {
            out += this[@min(togo, this.len - 1 - @mod(togo, 2) - 1)];

            if (togo > self.width + 1 + self.width / 2) {
                out += self.go_dia(togo - 1 - self.width - self.width / 2, left);
                out += self.go_dia(togo - 1 - self.width - self.width / 2, right);
            }

            togo -= self.width;
        }

        return out + this[togo];
    }

    fn go_n(self: *const Cached, togo: usize) usize {
        return self.go_straight(togo, self.n, self.ne, self.nw);
    }

    fn go_s(self: *const Cached, togo: usize) usize {
        return self.go_straight(togo, self.s, self.se, self.sw);
    }

    fn go_w(self: *const Cached, togo: usize) usize {
        return self.go_straight(togo, self.w, self.nw, self.sw);
    }

    fn go_e(self: *const Cached, togo: usize) usize {
        return self.go_straight(togo, self.e, self.ne, self.se);
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
        data.step();
        // std.debug.print("Step {}\n{s}\n", .{ z, data.field.contents });
    }

    return data.count();
}

fn zero(count: usize) usize {
    _ = count;
    return 0;
}
fn full(count: usize) usize {
    return count - 1;
}
fn halve(count: usize) usize {
    return count / 2;
}

fn fill(data: *Data, start: Point, target: []usize) void {
    data.set(start);

    for (0..target.len) |z| {
        target[z] = data.count();
        // std.debug.print("Step {}\n{s}\n", .{ z, data.field.contents });
        data.step();
    }
}

fn day(contents: []u8, allocator: std.mem.Allocator) anyerror!void {
    // std.time.sleep(10);
    // const safeContents = try allocator.alloc(u8, contents.len);
    // @memcpy(safeContents, contents);
    var data = try Data.init(contents, allocator);
    std.debug.print("Part 1: {}\n", .{part1(&data, 100)});

    // Pre calculate things
    std.debug.print("North\n", .{});
    const north: []usize = try allocator.alloc(usize, data.w + data.h);
    fill(&data, getPoint(&data, halve, full), north);

    std.debug.print("South\n", .{});
    const south: []usize = try allocator.alloc(usize, data.w + data.h);
    fill(&data, getPoint(&data, halve, zero), south);

    std.debug.print("West\n", .{});
    const west: []usize = try allocator.alloc(usize, data.w + data.h);
    fill(&data, getPoint(&data, full, halve), west);

    std.debug.print("East\n", .{});
    const east: []usize = try allocator.alloc(usize, data.w + data.h);
    fill(&data, getPoint(&data, zero, halve), east);

    std.debug.print("SouthEast\n", .{});
    const se: []usize = try allocator.alloc(usize, data.w + data.h);
    fill(&data, getPoint(&data, zero, zero), se);

    std.debug.print("SouthWest\n", .{});
    const sw: []usize = try allocator.alloc(usize, data.w + data.h);
    fill(&data, getPoint(&data, full, zero), sw);

    std.debug.print("NW\n", .{});
    const nw: []usize = try allocator.alloc(usize, data.w + data.h);
    fill(&data, getPoint(&data, full, full), nw);

    std.debug.print("NE\n", .{});
    const ne: []usize = try allocator.alloc(usize, data.w + data.h);
    fill(&data, getPoint(&data, zero, full), ne);

    const cached = Cached{
        .n = north,
        .s = south,
        .w = west,
        .e = east,
        .ne = ne,
        .nw = nw,
        .se = se,
        .sw = sw,
        .width = data.w,
    };

    const straight = cached.width / 2 + 1;
    const diag = cached.width + 1;

    // too high: 626078196179547
    const steps = 26501365;
    const out = cached.go_n(steps - straight) + cached.go_s(steps - straight) + cached.go_w(steps - straight) + cached.go_e(steps - straight) + cached.go_ne(steps - diag) + cached.go_nw(steps - diag) + cached.go_se(steps - diag) + cached.go_sw(steps - diag) + cached.w[cached.w.len - 1];

    std.debug.print("Steps {}: {} (full: {} {} {} {})\n", .{ steps, out, cached.n[cached.w.len - 1], cached.e[cached.w.len - 1], cached.s[cached.w.len - 1], cached.w[cached.w.len - 1] });
}
