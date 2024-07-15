const std = @import("std");
const Parser = @import("./parser.zig").Parser;
const expect = std.testing.expect;
const utils = @import("./utils.zig");

pub fn main() !void {
    try utils.mainImpl(day);
}

const Bucket = struct {
    name: []const u8,
    value: usize,
};

const Buckets = struct {
    len: usize,
    buckets: [50]Bucket,

    fn remove(self: *Buckets, name: []const u8) void {
        for (0..self.len) |i| {
            if (std.mem.eql(u8, self.buckets[i].name, name)) {
                // remove it
                for (i + 1..self.len) |j| {
                    self.buckets[j - 1] = self.buckets[j];
                }
                self.len -= 1;
                return;
            }
        }
    }

    fn set(self: *Buckets, name: []const u8, v: usize) void {
        for (0..self.len) |i| {
            if (std.mem.eql(u8, self.buckets[i].name, name)) {
                self.buckets[i].value = v;
                return;
            }
        }

        self.buckets[self.len] = Bucket{
            .name = name,
            .value = v,
        };
        self.len += 1;
    }

    fn value(self: *const Buckets) usize {
        var out: usize = 0;
        for (0..self.len) |i| {
            out += (i + 1) * self.buckets[i].value;
        }
        return out;
    }
};

fn part1(contents: []const u8) usize {
    var out: usize = 0;
    var current: u8 = 0;

    for (contents) |c| {
        if (c == ',' or c == '\n') {
            out += current;
            current = 0;
        } else {
            current = current +% c;
            current = current *% 17;
        }
    }
    return out;
}

fn part2(contents: []const u8) usize {
    var i: usize = 0;
    var hashes: [256]Buckets = undefined;
    for (0..256) |j| {
        hashes[j].len = 0;
    }

    while (i < contents.len - 1) {
        const start = i;
        var current: u8 = 0;
        while (contents[i] != '=' and contents[i] != '-') {
            const c = contents[i];
            current = current +% c;
            current = current *% 17;
            i += 1;
        }

        if (contents[i] == '-') {
            hashes[current].remove(contents[start..i]);
            i += 2;
        } else {
            const value = @as(usize, contents[i + 1] - '0');
            hashes[current].set(contents[start..i], value);
            i += 3;
        }
    }

    var out: usize = 0;
    for (0..256) |j| {
        out += (j + 1) * hashes[j].value();
    }

    return out;
}

fn day(contents: []const u8, allocator: std.mem.Allocator) anyerror!void {
    _ = allocator;

    std.debug.print("Part1 {}\n", .{part1(contents)});
    std.debug.print("Part2 {}\n", .{part2(contents)});
}
