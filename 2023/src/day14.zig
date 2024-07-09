const std = @import("std");
const ArrayList = std.ArrayList;
const eql = std.mem.eql;
const utils = @import("./utils.zig");

pub fn main() !void {
    try utils.mainImpl(day);
}

const FieldChange = struct {
    start: usize,
    iteration: usize,
};

const Field = struct {
    contents: []u8,
    copies: ArrayList([]u8),
    line_length: usize,
    line_count: usize,

    fn new(contents: []u8, alloc: std.mem.Allocator) Field {
        var line_length: usize = 0;
        while (contents[line_length] != '\n') {
            line_length += 1;
        }

        const line_count = contents.len / (line_length + 1);

        return Field{
            .line_count = line_count,
            .line_length = line_length,
            .contents = contents,
            .copies = ArrayList([]u8).init(alloc),
        };
    }

    fn deinit(self: *Field) void {
        self.copies.deinit();
    }

    fn idx(self: *Field, x: usize, y: usize) usize {
        return y * (self.line_length + 1) + x;
    }

    fn roll(self: *Field, jdx: usize, idy: usize) void {
        const dx = jdx;
        const dy = idy;

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

    fn changed(self: *Field, alloc: std.mem.Allocator) !?FieldChange {
        for (0..self.copies.items.len) |item| {
            if (std.mem.eql(u8, self.copies.items[item], self.contents)) {
                return FieldChange{ .iteration = self.copies.items.len - item, .start = item };
            }
        }

        const newCopy = try alloc.alloc(u8, self.contents.len);
        std.mem.copyForwards(u8, newCopy, self.contents);
        try self.copies.append(newCopy);

        return null;
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
    const contents = @constCast(c);

    var thingPart1 = Field.new(contents, allocator);
    thingPart1.roll(1, 0); // north
    std.debug.print("Part1 {}\n", .{thingPart1.count()});
    thingPart1.deinit();

    var thing = Field.new(contents, allocator);
    defer thing.deinit();

    var changedSince: ?FieldChange = try thing.changed(allocator);
    for (0..1000000000) |_| {
        thing.roll(1, 0); // north
        thing.roll(0, 1); // east
        thing.roll(1, 2); // south
        thing.roll(2, 1); // east
        changedSince = try thing.changed(allocator);
        if (changedSince != null) {
            break;
        }
    }

    if (changedSince) |since| {
        const itemIndex = since.start + (1000000000 - since.start) % since.iteration;
        thing.contents = thing.copies.items[itemIndex];
        std.debug.print("Part2 {}\n", .{thing.count()});
    }

    // std.debug.print("Part2 {}\n", .{total2});
}
