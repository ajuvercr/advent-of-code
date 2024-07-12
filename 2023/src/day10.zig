const std = @import("std");
const expect = std.testing.expect;
const utils = @import("./utils.zig");

pub fn main() !void {
    try utils.mainImpl(day);
}

fn Field(comptime T: type) type {
    return struct {
        contents: []T,
        row_length: usize,
        const Self = @This();

        fn get(self: Self, coord: Coord) T {
            const idx = coord[1] * (self.row_length + 1) + coord[0];
            return self.contents[idx];
        }

        fn set(self: Self, coord: Coord, item: T) void {
            const idx = coord[1] * (self.row_length + 1) + coord[0];
            self.contents[idx] = item;
        }

        fn init(contents: []T, row_length: usize) Self {
            return Self{
                .contents = contents,
                .row_length = row_length,
            };
        }

        fn move(self: Self, coord: Coord, from: Coord) ?Coord {
            if (T != u8) unreachable;
            const idx = coord[1] * (self.row_length + 1) + coord[0];
            if (idx >= self.contents.len) return null;
            const dir = self.contents[idx];
            switch (dir) {
                '|' => {
                    if (coord[1] < from[1]) {
                        return Coord{ coord[0], coord[1] - 1 };
                    } else {
                        return Coord{ coord[0], coord[1] + 1 };
                    }
                },
                '-' => {
                    if (coord[0] < from[0]) {
                        return Coord{ coord[0] - 1, coord[1] };
                    } else {
                        return Coord{ coord[0] + 1, coord[1] };
                    }
                },
                'L' => {
                    if (from[0] > coord[0]) {
                        return Coord{ coord[0], coord[1] - 1 };
                    } else {
                        return Coord{ coord[0] + 1, coord[1] };
                    }
                },
                'J' => {
                    if (from[0] < coord[0]) {
                        return Coord{ coord[0], coord[1] - 1 };
                    } else {
                        return Coord{ coord[0] - 1, coord[1] };
                    }
                },
                '7' => {
                    if (from[0] < coord[0]) {
                        return Coord{ coord[0], coord[1] + 1 };
                    } else {
                        return Coord{ coord[0] - 1, coord[1] };
                    }
                },
                'F' => {
                    if (from[0] > coord[0]) {
                        return Coord{ coord[0], coord[1] + 1 };
                    } else {
                        return Coord{ coord[0] + 1, coord[1] };
                    }
                },
                else => {
                    return null;
                },
            }
        }
    };
}

fn try_dir(dir: Coord, at: Coord, field: *Field(u8), shorts: *Field(usize)) void {
    if (field.move(dir, at)) |next| {
        var dist: usize = 1;
        var thisAt = dir;
        var thisNext = next;
        while (dist < shorts.get(thisAt)) {
            shorts.set(thisAt, dist);
            const n = field.move(thisNext, thisAt) orelse break;
            thisAt = thisNext;
            thisNext = n;
            dist += 1;
        }
    }
}

const Coord = [2]usize;

fn day(contents: []const u8, allocator: std.mem.Allocator) anyerror!void {
    var row_length: usize = 0;
    while (contents[row_length] != '\n') {
        row_length += 1;
    }

    var shortest_data = try allocator.alloc(usize, contents.len);
    for (0..contents.len) |i| shortest_data[i] = 100000;
    var shorts = Field(usize).init(shortest_data, row_length);

    var start_loc: usize = 0;
    while (contents[start_loc] != 'S') {
        start_loc += 1;
    }

    var field = Field(u8).init(@constCast(contents), row_length);

    const row = start_loc / (row_length + 1);
    const at = Coord{ start_loc - row * (row_length + 1), row };

    shorts.set(at, 0);

    var up = false;
    var down = false;
    var left = false;
    var right = false;

    if (at[0] < row_length - 1) {
        right = true;
        const dir = Coord{ at[0] + 1, at[1] };
        const l = field.get(dir);

        if (l == '-' or l == 'J' or l == '7') {
            try_dir(dir, at, &field, &shorts);
        }
    }
    if (at[1] < field.contents.len / (row_length + 1)) {
        down = true;
        const dir = Coord{ at[0], at[1] + 1 };
        const l = field.get(dir);

        if (l == '|' or l == 'L' or l == 'J') {
            try_dir(dir, at, &field, &shorts);
        }
    }
    if (at[0] > 0) {
        left = true;
        const dir = Coord{ at[0] - 1, at[1] };
        const l = field.get(dir);

        if (l == '-' or l == 'L' or l == 'F') {
            try_dir(dir, at, &field, &shorts);
        }
    }

    if (at[1] > 0) {
        up = true;
        const dir = Coord{ at[0], at[1] - 1 };
        const l = field.get(dir);

        if (l == '|' or l == '7' or l == 'F') {
            try_dir(dir, at, &field, &shorts);
        }
    }

    if (down and up) {
        field.set(at, '|');
    }
    if (left and right) {
        field.set(at, '-');
    }
    // ....

    field.set(at, 'J');

    for (0..shorts.contents.len) |i| {
        if (shorts.contents[i] == 100000) {
            if (field.contents[i] != '\n')
                field.contents[i] = '.';
        }
    }

    var idx: usize = 0;
    var total2: usize = 0;
    while (idx < shorts.contents.len) {
        var inside = false;
        var from_up = false;
        for (0..row_length) |_| {
            const c = field.contents[idx];
            if (inside and c == '.') {
                field.contents[idx] = 'I';
                total2 += 1;
            } else {
                switch (c) {
                    '|' => {
                        inside = !inside;
                    },
                    '-' => {},
                    'L' => {
                        from_up = true;
                    },
                    'F' => {
                        from_up = false;
                    },
                    'J' => {
                        if (!from_up) {
                            inside = !inside;
                        }
                    },
                    '7' => {
                        if (from_up) {
                            inside = !inside;
                        }
                    },
                    else => {},
                }
            }

            idx += 1;
        }
        idx += 1;
    }

    var max: usize = 0;
    for (shorts.contents) |i| {
        if (i < 100000) {
            max = @max(max, i);
        }
    }

    std.debug.print("Part1 {}\n", .{max});
    std.debug.print("Part2 {}\n", .{total2});
}
