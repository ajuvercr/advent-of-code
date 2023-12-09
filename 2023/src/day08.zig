const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    const args = try std.process.argsAlloc(allocator);
    // try day("./test.txt");
    try day(args[1]);
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

fn calc_moves(dirs: []const u8, moves: *[17576][2]usize, starts: []usize, ends: []usize) u64 {
    var part1: u64 = 0;

    var found = false;

    // while (idx != 17575) {
    while (!found) {
        found = true;
        const move = dirs[part1 % dirs.len];
        part1 += 1;
        for (0..starts.len) |i| {
            const idx = starts[i];
            if (move == 'R') {
                starts[i] = moves[idx][1];
            } else {
                starts[i] = moves[idx][0];
            }

            var sub = false;
            for (0..ends.len) |j| {
                if (starts[i] == ends[j]) {
                    sub = true;
                    break;
                }
            }
            found = found and sub;
        }
    }

    return part1;
}

pub fn day(file: []const u8) !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var file2 = try std.fs.cwd().openFile(file, .{});
    const contents = try file2.readToEndAlloc(allocator, 200000);
    defer allocator.free(contents);

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
    std.debug.print("starts {} ends {}\n", .{ starts, ends });

    std.debug.print("Part1 {}\n", .{calc_moves(dir, &moves, starts.items, ends.items)});
}
