const std = @import("std");
const Parser = @import("./parser.zig").Parser;
const utils = @import("./utils.zig");

const Arg = std.meta.Tuple(&[_]type{ []const u8, f32 });
fn Mat(vars_c: comptime_int, T: type) type {
    return struct {
        _vars: [vars_c][]const u8,

        _data: [][]T,

        alloc: std.mem.Allocator,
        _at: usize,

        const Self = @This();
        fn init(vars: [vars_c][]const u8, rows: usize, alloc: std.mem.Allocator) !Self {
            return Self{
                ._vars = vars,
                ._data = try alloc.alloc([]T, rows),
                .alloc = alloc,
                ._at = 0,
            };
        }

        fn find_idx(self: *const Self, name: []const u8) !usize {
            if (name.len == 0) return vars_c;

            for (0..self._vars.len) |i| {
                if (std.mem.eql(u8, self._vars[i], name)) {
                    return i;
                }
            }

            return error.Def;
        }

        fn row(self: *Self, args: []const Arg) !void {
            if (self._at == self._data.len) return error.Def;

            self._data[self._at] = try self.alloc.alloc(T, vars_c + 1);
            @memset(self._data[self._at], 0.0);

            for (args) |arg| {
                const idx = try self.find_idx(arg[0]);
                self._data[self._at][idx] = arg[1];
            }

            self._at += 1;
        }
    };
}

const ErrorValue = error{
    Def,
};

fn row_echelon(matrix: [][]f32) !void {
    for (0..matrix.len) |r| {
        // Normalize row
        const c = matrix[r][r];
        if (c == 0) return error.Def;

        for (r..matrix[r].len) |j| {
            matrix[r][j] /= c;
        }

        for (r + 1..matrix.len) |r2| {
            const spil = matrix[r2][r];
            for (r..matrix[r].len) |j| {
                matrix[r2][j] -= matrix[r][j] * spil;
            }
        }
    }
}

fn parse_isize(par: *Parser) f32 {
    if (par.peek(0).? == '-') {
        par.pos += 1;
        return -1 * par.number(f32).?;
    } else {
        return par.number(f32).?;
    }
}

const Triple = struct {
    x: f32,
    y: f32,
    z: f32,

    fn init(x: f32, y: f32, z: f32) Triple {
        return Triple{
            .x = x,
            .y = y,
            .z = z,
        };
    }

    fn parse(par: *Parser) Triple {
        _ = par.while_c(' ');
        const x = parse_isize(par);
        par.pos += 1;
        _ = par.while_c(' ');
        const y = parse_isize(par);
        par.pos += 1;
        _ = par.while_c(' ');
        const z = parse_isize(par);
        _ = par.while_c(' ');

        return Triple.init(x, y, z);
    }

    fn add(self: Triple, other: Triple) Triple {
        return Triple.init(self.x + other.x, self.y + other.y, self.z + other.z);
    }

    fn eq(self: Triple, other: Triple) bool {
        return self.x == other.x and self.y == other.y and self.z == other.z;
    }

    pub fn format(
        self: *const Triple,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("{d:.1}, {d:.1}, {d:.1}", .{ self.x, self.y, self.z });
    }
};

const Hail = struct {
    pos: Triple,
    vel: Triple,
    // y = ax + b
    // b = -ax1 + y2
    // ax1 + y1 = ax2 + y2
    // a = (y2 - y1) / (x1 - x2)
    a: f32,
    b: f32,
    // c: isize,

    fn init(pos: Triple, vel: Triple) Hail {
        const a: f32 = vel.y / vel.x;
        const x1 = pos.x;
        const y1 = pos.y;
        const b: f32 = -1.0 * a * x1 + y1;

        std.debug.print("y = a({d:.1})x + b({d:.1})\n", .{ a, b });

        return Hail{
            .pos = pos,
            .vel = vel,
            .a = a,
            .b = b,
            // .c = 0,
        };
    }

    fn cross(self: *const Hail, other: *const Hail) ?Triple {
        const a1 = self.a;
        const b1 = self.b;
        const a2 = other.a;
        const b2 = other.b;

        if (a1 == a2) return null;

        const x = (b2 - b1) / (a1 - a2);
        const y = a1 * x + b1;

        return Triple.init(x, y, 0.0);
    }

    fn in_future(self: *const Hail, other: Triple) bool {
        const n = (other.x - self.pos.x) / self.vel.x;
        return n > 0;
    }
};

pub fn main() !void {
    try utils.mainImpl(day);
}

fn day(contents: []const u8, allocator: std.mem.Allocator) anyerror!void {
    var par = Parser.init(contents);

    var list = std.ArrayList(Hail).init(allocator);
    defer list.deinit();

    while (par.peek(0) != undefined) : (par.pos += 1) {
        const pos = Triple.parse(&par);
        par.pos += 1;
        const vel = Triple.parse(&par);
        try list.append(Hail.init(pos, vel));
    }

    const lowerBound = 200000000000000;
    const upperBound = 400000000000000;

    var out: usize = 0;
    for (0..list.items.len) |i| {
        for (i + 1..list.items.len) |j| {
            const a = list.items[i];
            const b = list.items[j];
            if (a.cross(&b)) |at| {
                const inside = at.x < upperBound and at.x > lowerBound and at.y < upperBound and at.y > lowerBound;
                const future = a.in_future(at) and b.in_future(at);
                if (inside and future) {
                    out += 1;
                }
                std.debug.print("{any} @ {any} crosses {any} @ {any} at {any} future {} inside {}\n", .{ a.pos, a.vel, b.pos, b.vel, at, future, inside });
            } else {
                std.debug.print("{any} @ {any} doesn't cross {any} @ {any}\n", .{ a.pos, a.vel, b.pos, b.vel });
            }
        }
    }

    var mat = try Mat(2, f32).init(.{ "t1", "t2", "t3", "vx", "vy", "vz", "x", "y", "z" }, 9, allocator);

    const h1 = list.itmes[0];
    const h2 = list.itmes[1];
    const h3 = list.itmes[2];

    try mat.row(([_]Arg{ .{ "x", 1.0 }, .{ "y", 2.0 }, .{ "", 3 } })[0..]);
    try mat.row(([_]Arg{ .{ "x", 4.0 }, .{ "y", 1.0 }, .{ "", 2 } })[0..]);

    // x = var('x')
    // y = var('y')
    // z = var('z')
    // vx = var('vx')
    // vy = var('vy')
    // vz = var('vz')
    // t1 = var('t1')
    // t2 = var('t2')
    // t3 = var('t3')

    const data: [][]f32 = mat._data;
    try row_echelon(data);

    std.debug.print("{any}\n", .{data});

    std.debug.print("Part 1 {any}\n", .{out});
}
