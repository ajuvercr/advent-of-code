const std = @import("std");
const Parser = @import("./parser.zig").Parser;
const utils = @import("./utils.zig");
const Point = utils.Point;
const Field = utils.Field;

pub fn main() !void {
    try utils.mainImpl(day);
}

const Segment = struct {
    start: isize,
    end: isize,
    y: isize,

    fn init(start: isize, end: isize, y: isize) Segment {
        return Segment{
            .start = start,
            .end = end - 1,
            .y = y,
        };
    }
};

const LL = std.DoublyLinkedList(Segment);

// Declare an enum.
const AddType = enum {
    StartAdd,
    StartNeg,
    EndAdd,
    EndNeg,
    Split,
    Loose,
};
const NewLine = struct {
    ll: LL,
    allocator: std.mem.Allocator,
    min: *isize,
    extra: isize,

    fn init(min: *isize, allocator: std.mem.Allocator) NewLine {
        return NewLine{
            .ll = LL{ .first = null },
            .allocator = allocator,
            .min = min,
            .extra = 0,
        };
    }

    fn count(self: *const NewLine) isize {
        var out: isize = 0;

        var start_node = self.ll.first;

        while (start_node) |start| {
            out += start.data.end - start.data.start + 1;
            start_node = start.next;
        }

        return out;
    }

    fn insert(self: *NewLine, segment: Segment) !void {
        var prev_node: ?*LL.Node = self.ll.first;
        while (prev_node != null and prev_node.?.data.end < segment.start) {
            prev_node = prev_node.?.next;
        }

        if (prev_node) |prev| {
            if (segment.start == prev.data.start and segment.end == prev.data.end) {
                self.ll.remove(prev);
                self.extra += segment.end - segment.start + 1;
                self.allocator.destroy(prev);
                return;
            }

            std.debug.assert(!(segment.start < prev.data.start and segment.end > prev.data.start));
            std.debug.assert(!(segment.start < prev.data.end and segment.end > prev.data.end));

            var add_type: ?AddType = null;

            if (segment.end == prev.data.start) {
                std.debug.assert(add_type == null);
                add_type = AddType.StartAdd;
            }

            if (segment.start == prev.data.end) {
                std.debug.assert(add_type == null);
                add_type = AddType.EndAdd;
            }

            if (segment.start == prev.data.start) {
                std.debug.assert(add_type == null);
                add_type = AddType.StartNeg;
            }

            if (segment.end == prev.data.end) {
                std.debug.assert(add_type == null);
                add_type = AddType.EndNeg;
            }

            if (segment.start > prev.data.start and segment.end < prev.data.end) {
                std.debug.assert(add_type == null);
                add_type = AddType.Split;
            }

            if (prev.data.start > segment.end) {
                std.debug.assert(add_type == null);
                add_type = AddType.Loose;
            }

            std.debug.assert(add_type != null);

            if (add_type) |at| {
                switch (at) {
                    AddType.StartAdd => {
                        prev.data.start = segment.start;
                    },
                    AddType.EndAdd => {
                        prev.data.end = segment.end;
                    },
                    AddType.StartNeg => {
                        prev.data.start = segment.end;
                        self.extra += segment.end - segment.start;
                    },
                    AddType.EndNeg => {
                        prev.data.end = segment.start;
                        self.extra += segment.end - segment.start;
                    },
                    AddType.Split => {
                        const start = prev.data.start;
                        prev.data.start = segment.end;
                        // add new part
                        var node = try self.allocator.create(LL.Node);
                        node.data.start = start;
                        node.data.end = segment.start;
                        self.ll.insertBefore(prev, node);

                        self.extra += segment.end - segment.start - 1;
                    },
                    AddType.Loose => {
                        var node = try self.allocator.create(LL.Node);
                        node.data = segment;
                        self.ll.insertBefore(prev, node);
                    },
                }
            }

            if (prev.next) |next_node| {
                if (next_node.data.start == prev.data.end) {
                    prev.data.end = next_node.data.end;
                    self.ll.remove(next_node);
                    self.allocator.destroy(next_node);
                }
            }
        } else {
            var node = try self.allocator.create(LL.Node);
            node.data = segment;
            self.ll.append(node);
        }
    }

    pub fn format(
        self: *const NewLine,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = options;
        if (std.mem.eql(u8, fmt, "short")) {
            var current: isize = 0;
            var current_node = self.ll.first;
            while (current_node) |c| {
                current_node = c.next;
                // print empty space
                var i = current;
                while (i < c.data.start) {
                    i += 1;
                    try writer.print(".", .{});
                }

                // print full space
                i = c.data.start;
                while (i < c.data.end + 1) {
                    i += 1;
                    try writer.print("#", .{});
                }
                current = c.data.end + 1;
            }
        } else {
            var current_node = self.ll.first;
            while (current_node) |c| {
                current_node = c.next;
                // print empty space
                try writer.print(".#{}-{}#.", .{ c.data.start, c.data.end });
            }
        }
    }
};

const Data = struct {
    point: Point,
    from_up: bool,
};

fn char_to_dir(c: u8) Point {
    return switch (c) {
        'U' => Point.UP,
        'R' => Point.RIGHT,
        'D' => Point.DOWN,
        'L' => Point.LEFT,
        else => unreachable,
    };
}

fn sorter(ctx: void, a: Segment, b: Segment) bool {
    _ = ctx;
    if (a.y == b.y) {
        return a.start < b.start;
    }
    return a.y < b.y;
}

const Inp = struct {
    dir: Point,
    len: isize,
    fn parse(par: *Parser, part2: bool) Inp {
        const dir_c = par.char().?;
        const dir = char_to_dir(dir_c);
        par.pos += 1;
        const amount = par.number(isize).?;
        par.pos += 2;
        const color = par.until(')');
        par.pos += 2;

        if (part2) {
            const massive = std.fmt.parseInt(isize, color[1..6], 16) catch unreachable;
            const this_dir = switch (color[6]) {
                '0' => Point.RIGHT,
                '1' => Point.DOWN,
                '2' => Point.LEFT,
                '3' => Point.UP,
                else => unreachable,
            };

            return Inp{
                .dir = this_dir,
                .len = massive,
            };
        }

        return Inp{
            .dir = dir,
            .len = amount,
        };
    }

    fn dx(self: *const Inp) isize {
        if (self.dir.eq(Point.RIGHT)) return self.len;
        // Going to the left
        return -1 * self.len;
    }

    fn dy(self: *const Inp) isize {
        if (self.dir.eq(Point.DOWN)) return self.len;
        return -1 * self.len;
    }
};

fn sorter_x(ctx: void, a: Segment, b: Segment) bool {
    _ = ctx;
    return a.start < b.start;
}

fn clean(lines: *std.ArrayList(Segment)) void {
    std.sort.heap(Segment, lines.items, {}, sorter_x);

    var idx = lines.items.len;
    while (idx > 1) {
        idx -= 1;
        const a = lines.items[idx - 1];
        if (a.len() <= 0) {
            _ = lines.orderedRemove(idx - 1);
            continue;
        }
        const b = lines.items[idx];
        if (b.len() <= 0) {
            _ = lines.orderedRemove(idx);
            continue;
        }

        if (a.end == b.start) {
            lines.items[idx - 1].end = b.end;
            _ = lines.orderedRemove(idx);
        }
    }
    if (lines.items.len > 0) {
        if (lines.items[0].len() <= 0) {
            _ = lines.orderedRemove(0);
        }
    }
}

fn part(part2: bool, par: *Parser, allocator: std.mem.Allocator) !isize {
    par.pos = 0;
    var total: isize = 0;

    var points = std.ArrayList(Point).init(allocator);
    defer points.deinit();

    var lines = std.ArrayList(Segment).init(allocator);
    defer lines.deinit();

    var at = Point.new(0, 0);
    var min: isize = 0;
    while (par.peek(0) != undefined) {
        const hor = Inp.parse(par, part2);
        const vert = Inp.parse(par, part2);

        const next = at.add(Point.new(hor.dx(), 0));

        // We have to create a new line
        if (hor.dir.eq(Point.LEFT)) {
            try lines.append(
                Segment.init(next.x, at.x + 1, next.y),
            );
            min = @min(min, next.x);
        } else {
            try lines.append(
                Segment.init(at.x, next.x + 1, next.y),
            );
            min = @min(min, at.x);
        }

        at = next.add(Point.new(0, vert.dy()));
    }

    var new_lines = NewLine.init(&min, allocator);

    std.sort.heap(Segment, lines.items, {}, sorter);
    var current_y = lines.items[0].y - 1;

    for (lines.items) |incoming| {
        if (incoming.y != current_y) {
            total += (incoming.y - current_y) * new_lines.count() + new_lines.extra;
            new_lines.extra = 0;
            current_y = incoming.y;
        }
        try new_lines.insert(incoming);
    }

    total += new_lines.extra;
    return total;
}

fn day(contents: []const u8, allocator: std.mem.Allocator) anyerror!void {
    var par = Parser.init(contents);
    std.debug.print("Part1 {any}\n", .{part(false, &par, allocator)});
    std.debug.print("Part2 {any}\n", .{part(true, &par, allocator)});
}
