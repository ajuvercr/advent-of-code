const std = @import("std");
pub fn mainImpl(comptime day_f: fn (content: []u8, allocator: std.mem.Allocator) anyerror!void) !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    const args = try std.process.argsAlloc(allocator);

    var inp: []const u8 = undefined;
    if (args.len >= 2) {
        inp = args[1];
    } else {
        inp = "./test.txt";
    }

    var file2 = try std.fs.cwd().openFile(inp, .{});
    const contents = try file2.readToEndAlloc(allocator, 500000);
    defer allocator.free(contents);

    std.debug.print("Using input {s}\n", .{inp});
    try day_f(contents, allocator);
    std.debug.print("\n", .{});
}

pub const Point = struct {
    pub const UP = Point.new(0, -1);
    pub const DOWN = Point.new(0, 1);
    pub const LEFT = Point.new(-1, 0);
    pub const RIGHT = Point.new(1, 0);

    pub const Points: [4]Point = .{ Point.UP, Point.LEFT, Point.DOWN, Point.RIGHT };

    x: isize,
    y: isize,

    pub fn new(x: isize, y: isize) Point {
        return Point{
            .x = x,
            .y = y,
        };
    }

    pub fn add(self: Point, other: Point) Point {
        return Point{
            .x = self.x + other.x,
            .y = self.y + other.y,
        };
    }

    pub fn eq(self: Point, other: Point) bool {
        return self.x == other.x and self.y == other.y;
    }
};

pub fn Field(comptime T: type) type {
    return struct {
        contents: []T,
        row_length: isize,
        has_newline: bool,
        const Self = @This();

        pub fn init(contents: []T, has_newline: bool, row_length: ?isize) Self {
            if (row_length) |len| {
                return Self{
                    .contents = contents,
                    .has_newline = has_newline,
                    .row_length = len,
                };
            } else {
                var rl: usize = 0;
                if (T != u8) {
                    @panic("Can only find row length for character based fields");
                }
                while (contents[rl] != '\n') {
                    rl += 1;
                }

                return Self{
                    .contents = contents,
                    .has_newline = has_newline,
                    .row_length = @intCast(rl),
                };
            }
        }

        pub fn _idx(self: Self, x: isize, y: isize) isize {
            if (self.has_newline) {
                return y * (self.row_length + 1) + x;
            } else {
                return y * self.row_length + x;
            }
        }

        pub fn get_wrap(self: Self, x: isize, y: isize) ?T {
            return self.get(@mod(x, self.row_length), @mod(y, self.col_length()));
        }

        pub fn get_p_wrap(self: Self, point: Point) ?T {
            const v = self.get_wrap(point.x, point.y);
            return v;
        }

        pub fn get(self: Self, x: isize, y: isize) ?T {
            if (x >= self.row_length or x < 0 or y < 0 or y >= self.col_length()) return null;
            const idx = self._idx(x, y);
            if (idx >= 0 and idx < self.contents.len) {
                return self.contents[@bitCast(idx)];
            }
            return null;
        }

        pub fn get_p(self: Self, point: Point) ?T {
            const v = self.get(point.x, point.y);
            return v;
        }

        pub fn set(self: Self, x: isize, y: isize, item: T) bool {
            const idx = self._idx(x, y);
            if (idx >= 0 and idx < self.contents.len) {
                self.contents[@bitCast(idx)] = item;
                return true;
            }
            return false;
        }

        pub fn set_p(self: Self, point: Point, item: T) bool {
            return self.set(point.x, point.y, item);
        }

        pub fn col_length(self: Self) isize {
            const cl: isize = @bitCast(self.contents.len);
            if (self.has_newline) {
                return @divFloor(cl, self.row_length + 1);
            } else {
                return @divFloor(cl, self.row_length);
            }
        }
    };
}
