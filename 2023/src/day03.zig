const std = @import("std");
const utils = @import("./utils.zig");

pub fn main() !void {
    try utils.mainImpl(day);
}

const DIRS = [_][2]isize{
    [2]isize{ 0, 1 },
    [2]isize{ 0, -1 },
    [2]isize{ 1, 1 },
    [2]isize{ 1, -1 },
    [2]isize{ -1, -1 },
    [2]isize{ -1, 1 },
    [2]isize{ 1, 0 },
    [2]isize{ -1, 0 },
};

fn index_with_dir(index: isize, dir: [2]isize, line_length: isize, rows: isize) ?usize {
    const out = index + dir[0] + dir[1] * line_length;
    if (out < 0) return null;
    if (out >= line_length * rows) return null;
    return @bitCast(out);
}

fn is_digit(char: u8) bool {
    return char >= '0' and char <= '9';
}

fn next_gear(gears: *[500]usize, gear: usize) usize {
    for (0..gears.len) |i| {
        if (gears[i] == 0) {
            gears[i] = gear;
            return i;
        }

        if (gears[i] == gear) return i;
    }
    unreachable;
}

fn parse_num_ret(parser: *std.fmt.Parser) usize {
    const at = parser.pos;
    const out = parser.number() orelse unreachable;
    parser.pos = at;
    return out;
}

fn day(contents: []const u8, allocator: std.mem.Allocator) anyerror!void {
    _ = allocator;
    var rows: isize = 0;
    for (contents) |c| {
        if (c == '\n') rows += 1;
    }

    var par = std.fmt.Parser{ .buf = contents };
    var total: usize = 0;

    var gearsIdx = std.mem.zeroes([500]usize);
    var valids = std.mem.zeroes([500]usize);
    var gearValues = std.mem.zeroes([500]usize);

    const line_length: isize = @bitCast(par.until('\n').len + 1);
    par.pos = 0;

    while (par.peek(0) != undefined) : (par.pos += 1) {
        const current = par.peek(0) orelse break;
        // we found a number
        if (is_digit(current)) {
            var valid = false;
            var gear = false;
            var at = par.pos;

            while (is_digit(contents[at])) {
                for (DIRS) |dir| {
                    if (index_with_dir(@bitCast(at), dir, line_length, rows)) |idx| {
                        const c = contents[idx];
                        valid = valid or (!is_digit(c) and c != '.' and c != '\n');

                        // this is a gear
                        if (c == '*' and !gear) {
                            gear = true;
                            const val = parse_num_ret(&par);
                            const myGear = next_gear(&gearsIdx, idx);
                            valids[myGear] += 1;
                            if (gearValues[myGear] == 0) {
                                gearValues[myGear] = val;
                            } else {
                                gearValues[myGear] *= val;
                            }
                        }
                    }
                }
                at += 1;
            }

            if (valid) {
                total += par.number().?;
            } else {
                _ = par.number().?;
            }
        }
    }

    std.debug.print("Part1 {}\n", .{total});
    var total2: usize = 0;
    for (0..500) |i| {
        if (valids[i] == 2) {
            total2 += gearValues[i];
        }
    }
    std.debug.print("Part2 {}\n", .{total2});
}
