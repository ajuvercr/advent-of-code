const std = @import("std");
const utils = @import("./utils.zig");

pub fn main() !void {
    try utils.mainImpl(day);
}

fn index(par: *std.fmt.Parser, starts: *std.ArrayList(usize), ends: *std.ArrayList(usize)) !usize {
    const a: usize = par.char().? - 'A';
    const b: usize = par.char().? - 'A';
    const c: usize = par.char().? - 'A';
    const out = a * 26 * 26 + b * 26 + c;

    if (c == 0) {
        try starts.append(out);
    }
    if (c == 25) {
        try ends.append(out);
    }

    return out;
}

fn is_finished(idx: usize, ends: []const usize) bool {
    for (ends) |e| {
        if (idx == e) return true;
    }
    return false;
}

fn calc_move(dirs: []const u8, moves: *const [17576][2]usize, start: usize, ends: []const usize) u64 {
    var current: usize = start;
    var out: u64 = 0;
    while (!is_finished(current, ends)) {
        const move = dirs[out % dirs.len];
        out += 1;

        if (move == 'R') {
            current = moves[current][1];
        } else {
            current = moves[current][0];
        }
    }

    return out;
}

pub fn day(contents: []const u8, allocator: std.mem.Allocator) anyerror!void {
    var starts = std.ArrayList(usize).init(allocator);
    var ends = std.ArrayList(usize).init(allocator);

    var par = std.fmt.Parser{ .buf = contents };
    const dir = par.until('\n');
    par.pos += 2;

    var moves = std.mem.zeroes([17576][2]usize);
    while (par.peek(0) orelse '\n' != '\n') {
        const idx = try index(&par, &starts, &ends);
        par.pos += 4;
        const a = try index(&par, &starts, &ends);
        par.pos += 2;
        const b = try index(&par, &starts, &ends);
        par.pos += 2;

        moves[idx] = [2]usize{ a, b };
    }

    std.debug.print("Part1: {}\n", .{calc_move(dir, &moves, 0, &[_]usize{17575})});

    var part2: u64 = 1;
    for (starts.items) |start| {
        const count = calc_move(dir, &moves, start, ends.items);
        const gcd = std.math.gcd(part2, count);
        part2 = part2 / gcd * count;
    }

    std.debug.print("Part2: {}\n", .{part2});
}
