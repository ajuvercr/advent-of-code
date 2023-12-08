const std = @import("std");

const Range = struct {
    dest: usize,
    start: usize,
    len: usize,

    pub fn map(self: *const Range, input: usize) ?usize {
        if (input > self.start and input < self.start + self.len) {
            return self.dest + (input - self.start);
        }
        return null;
    }

    pub fn map_range(self: *const Range, input: *const SeedRange, output: *std.ArrayList(SeedRange)) void {
        _ = output;
        _ = input;
        _ = self;
    }
};

const SeedRange = struct {
    start: usize,
    len: usize,
    mapped: bool,
};

const Part1 = struct {
    seeds: [50]usize,
    mapped: [50]bool,
    seed_count: usize,

    pub fn add_seed(self: *Part1, seed: usize) void {
        self.seeds[self.seed_count] = seed;
        self.seed_count += 1;
    }

    pub fn handle_range(self: *Part1, range: *const Range) void {
        for (0..self.seed_count) |i| {
            if (!self.mapped[i]) {
                if (range.map(self.seeds[i])) |r| {
                    self.mapped[i] = true;
                    self.seeds[i] = r;
                }
            }
        }
    }

    pub fn reset(self: *Part1) void {
        self.mapped = std.mem.zeroes([50]bool);
    }

    pub fn get_min(self: *Part1) usize {
        var min = self.seeds[0];
        for (1..self.seed_count) |i| {
            if (min > self.seeds[i]) {
                min = self.seeds[i];
            }
        }
        return min;
    }
};

const Part2 = struct {
    seeds: [50]SeedRange,
    new_seeds: [50]SeedRange,
    seed_count: usize,
    pub fn add_seed(self: *Part2, seed: usize, len: usize) void {
        self.seeds[self.seed_count] = SeedRange{ .start = seed, .len = len, .mapped = false };
        self.seed_count += 1;
    }

    pub fn handle_range(self: *Part2, range: *const Range) void {
        var i = 0;
        while (i < self.seed_count) : (i += 1) {
            var cur = &self.seeds[i];
            if (cur.mapped) continue;
            // Start is consumed
            if (cur.start > range.start and cur.start < range.start + range.len) {
                cur.mapped = true;
                cur.start = range.dest + cur.start - range.start;

                if (cur.start + cur.len > range.start + range.len) {
                    const delta = cur.start + cur.len - range.start - range.len;
                    self.add_seed(range.start + range.len, delta);
                    cur.len -= delta;
                }
                continue;
            }

            // End is consumed
            if (cur.start + cur.len > range.start + range.len and cur.start < range.start + range.len) {}
        }
    }
};

fn parse_range(par: *std.fmt.Parser) Range {
    const dest = par.number().?;
    par.pos += 1;
    const start = par.number().?;
    par.pos += 1;
    const len = par.number().?;
    par.pos += 1;

    return Range{ .dest = dest, .start = start, .len = len };
}
pub fn main() !void {
    try day("./input/05.txt");
    // try day("./test.txt");
}

pub fn day(fileName: []const u8) !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var file = try std.fs.cwd().openFile(fileName, .{});
    const contents = try file.readToEndAlloc(allocator, 200000);
    defer allocator.free(contents);

    var par = std.fmt.Parser{ .buf = contents };

    var part1 = std.mem.zeroes(Part1);

    par.pos += 6;
    while (par.peek(0) != '\n') {
        par.pos += 1;

        const seed = par.number().?;
        part1.add_seed(seed);
    }
    par.pos += 2;

    while (par.peek(0) != null) : (par.pos += 1) {
        const name = par.until('\n');
        std.debug.print("Handling {s}\n", .{name});
        par.pos += 1;

        part1.reset();
        var peek = par.peek(0).?;

        while (peek >= '0' and peek <= '9') : (peek = par.peek(0) orelse '\n') {
            const range = parse_range(&par);
            std.debug.print("Range {} {} {}\n", .{ range.dest, range.start, range.len });
            part1.handle_range(&range);
        }
    }

    std.debug.print("Part1 {}\n", .{part1.get_min()});
    // std.debug.print("Part2 {}\n", .{total2});
}
