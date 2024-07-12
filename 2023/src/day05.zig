const std = @import("std");
const Parser = @import("./parser.zig").Parser;
const utils = @import("./utils.zig");

pub fn main() !void {
    try utils.mainImpl(day);
}

const Range = struct {
    dest: u64,
    start: u64,
    len: u64,

    pub fn end(self: *const Range) u64 {
        return self.start + self.len;
    }

    pub fn map(self: *const Range, input: u64) ?u64 {
        if (input >= self.start and input < self.start + self.len) {
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

const Part1 = struct {
    seeds: [50]u64,
    mapped: [50]bool,
    seed_count: u64,

    pub fn add_seed(self: *Part1, seed: u64) void {
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

    pub fn get_min(self: *Part1) u64 {
        var min = self.seeds[0];
        for (1..self.seed_count) |i| {
            if (min > self.seeds[i]) {
                min = self.seeds[i];
            }
        }
        return min;
    }
};

const SeedRange = struct {
    start: u64,
    len: u64,
    mapped: bool,

    fn end(self: *const SeedRange) u64 {
        return self.start + self.len;
    }
};

const Part2 = struct {
    seeds: std.ArrayList(SeedRange),

    pub fn add_seed(self: *Part2, seed: u64, len: u64) !void {
        try self.seeds.append(SeedRange{ .start = seed, .len = len, .mapped = false });
    }

    pub fn handle_range(self: *Part2, range: *const Range) !void {
        var i: u64 = 0;
        while (i < self.seeds.items.len) : (i += 1) {
            if (self.seeds.items[i].mapped) continue;

            // Range   |--------|
            // cur      |---|
            //
            //          |map|
            //
            // Range    |--------|
            // cur  |---------------|
            //
            //      |---|--map---|--|
            if (self.seeds.items[i].start < range.end() and self.seeds.items[i].end() > range.start) {
                if (self.seeds.items[i].start < range.start) {
                    const length = range.start - self.seeds.items[i].start;
                    try self.add_seed(self.seeds.items[i].start, length);
                    self.seeds.items[i].start = range.start;
                    self.seeds.items[i].len -= length;
                }

                if (self.seeds.items[i].end() > range.end()) {
                    const length = self.seeds.items[i].end() - range.end();
                    try self.add_seed(range.end(), length);
                    self.seeds.items[i].len -= length;
                }

                self.seeds.items[i].start = range.map(self.seeds.items[i].start).?;
                self.seeds.items[i].mapped = true;
            }
        }
    }

    pub fn reset(self: *Part2) void {
        for (0..self.seeds.items.len) |i| {
            self.seeds.items[i].mapped = false;
        }
    }

    pub fn get_min(self: *Part2) u64 {
        var min = self.seeds.items[0].start;
        for (self.seeds.items) |s| {
            min = @min(s.start, min);
        }
        return min;
    }
};

fn parse_range(par: *Parser) Range {
    const dest = par.number(usize).?;
    par.pos += 1;
    const start = par.number(usize).?;
    par.pos += 1;
    const len = par.number(usize).?;
    par.pos += 1;

    return Range{ .dest = dest, .start = start, .len = len };
}

fn day(contents: []const u8, allocator: std.mem.Allocator) anyerror!void {
    var par = Parser.init(contents);

    var part1 = std.mem.zeroes(Part1);

    var seeds = std.ArrayList(SeedRange).init(allocator);

    par.pos += 6;
    while (par.peek(0) != '\n') {
        par.pos += 1;
        const a = par.number(usize).?;
        part1.add_seed(a);

        par.pos += 1;
        const b = par.number(usize).?;
        part1.add_seed(b);

        try seeds.append(SeedRange{ .start = a, .len = b, .mapped = false });
    }
    par.pos += 2;

    var part2 = Part2{ .seeds = seeds };
    defer part2.seeds.deinit();

    while (par.peek(0) != null) : (par.pos += 1) {
        _ = par.until('\n');
        par.pos += 1;

        part1.reset();
        part2.reset();
        var peek = par.peek(0).?;

        while (peek >= '0' and peek <= '9') : (peek = par.peek(0) orelse '\n') {
            const range = parse_range(&par);
            part1.handle_range(&range);
            try part2.handle_range(&range);
        }
    }

    std.debug.print("Part1 {}\n", .{part1.get_min()});
    std.debug.print("Part2 {}\n", .{part2.get_min()});
}
