const std = @import("std");
const utils = @import("./utils.zig");

pub fn main() !void {
    try utils.mainImpl(day);
}

const Field = struct {
    contents: []u8,
    copy: []u8,
    line_length: usize,
    line_count: usize,

    fn new(contents: []u8, copy: []u8) Field {
        var line_length: usize = 0;
        while (contents[line_length] != '\n') {
            line_length += 1;
        }

        var line_count = contents.len / (line_length + 1);

        return Field{
            .line_count = line_count,
            .line_length = line_length,
            .contents = contents,
            .copy = copy,
        };
    }

    fn idx(self: *Field, x: usize, y: usize) usize {
        return y * (self.line_length + 1) + x;
    }

    fn roll(self: *Field, jdx: usize, idy: usize) void {
        var dx = jdx;
        var dy = idy;

        var x_start: usize = 0;
        var x_length: usize = 0;
        var y_start: usize = 0;
        var y_length: usize = 0;
        if (dx == 1) {
            x_start = 0;
            x_length = self.line_length;
            y_start = (dy) / 2;
            y_length = self.line_count - 1;
            // dx = 0;
        } else {
            x_start = (dx) / 2;
            x_length = self.line_length - 1;
            y_start = 0;
            y_length = self.line_count;
            // dy = 0;
        }
        // up dx: 1 dy 2
        // down dx: 1 dy 0
        for (0..self.line_count) |_| {
            //
            // 0 ... n-1 +1
            // 1 ... n   -1
            for (y_start..y_start + y_length) |y| {
                for (x_start..x_start + x_length) |x| {
                    const this_idx = self.idx(x, y);
                    const this = self.contents[this_idx];
                    const below_idx = self.idx(x + 1 - dx, y + 1 - dy);
                    const below = self.contents[below_idx];
                    if (this == '.' and below == 'O') {
                        self.contents[this_idx] = 'O';
                        self.contents[below_idx] = '.';
                    }
                }
            }
            //
        }
    }

    fn changed(self: *Field) bool {
        const out = std.mem.eql(u8, self.copy, self.contents);
        if (out) return false;

        std.mem.copy(u8, self.copy, self.contents);
        return true;
    }

    fn count(self: *Field) usize {
        var total: usize = 0;
        for (0..self.line_count) |y| {
            for (0..self.line_length) |x| {
                const this_idx = self.idx(x, y);
                if (self.contents[this_idx] == 'O') {
                    total += self.line_count - y;
                }
            }
        }
        return total;
    }
};

fn day(c: []const u8, allocator: std.mem.Allocator) anyerror!void {
    var copy = try allocator.alloc(u8, c.len);
    var total: usize = 0;
    _ = total;
    const contents = @constCast(c);
    //
    // var line_length: usize = 0;
    // while (contents[line_length] != '\n') {
    //     line_length += 1;
    // }
    //
    // var line_count = contents.len / (line_length + 1);
    //
    // for (0..line_count) |_| {
    //     // one less in the line count (going up)
    //     for (0..line_count - 1) |y| {
    //         // All theses
    //         for (0..line_length) |x| {
    //             const this_idx = y * (line_length + 1) + x;
    //             const this = contents[this_idx];
    //
    //             // one more in going up
    //             const below_idx = (y + 1) * (line_length + 1) + x;
    //             const below = contents[below_idx];
    //             if (this == '.' and below == 'O') {
    //                 contents[this_idx] = 'O';
    //                 contents[below_idx] = '.';
    //             }
    //         }
    //     }
    //     //
    // }

    var thing = Field.new(contents, copy);
    for (0..1000000000) |_| {
        thing.roll(1, 0); // north
        thing.roll(0, 1); // east
        thing.roll(1, 2); // south
        thing.roll(2, 1); // east
        if (!thing.changed()) {
            break;
        }
    }

    std.debug.print("{s}\n", .{thing.contents});
    // for (0..line_count) |y| {
    //     for (0..line_length) |x| {
    //         const this_idx = y * (line_length + 1) + x;
    //         if (contents[this_idx] == 'O') {
    //             total += line_count - y;
    //         }
    //     }
    // }

    std.debug.print("Part1 {}\n", .{thing.count()});
    // std.debug.print("Part2 {}\n", .{total2});
}
