const std = @import("std");
const Parser = @import("./parser.zig").Parser;
const expect = std.testing.expect;
const utils = @import("./utils.zig");

pub fn main() !void {
    try utils.mainImpl(day);
}

const Cache = std.AutoHashMap(usize, usize);

fn to_key(s_len: usize, c_len: usize, deja_found: usize) usize {
    return s_len * 32 * 128 + c_len * 32 + deja_found;
}

fn count_springs(springs: []const u8, correction: []const usize, deja_found: usize, cache: *Cache) !usize {
    if (cache.get(to_key(springs.len, correction.len, deja_found))) |i| {
        return i;
    }

    var corr: usize = undefined;
    if (correction.len == 0) {
        corr = 0;
    } else {
        corr = correction[0];
    }

    if (springs.len == 0) {
        if (deja_found == corr and correction.len < 2) {
            return 1;
        }
        return 0;
    }

    const c = springs[0];

    var out: usize = 0;

    if (c == '?' or c == '.') {
        if (deja_found == 0) {
            out += try count_springs(springs[1..], correction, 0, cache);
        } else {
            if (corr == deja_found) {
                out += try count_springs(springs[1..], correction[1..], 0, cache);
            }
        }
    }

    if (c == '?' or c == '#') {
        if (corr > deja_found) {
            out += try count_springs(springs[1..], correction, deja_found + 1, cache);
        }
    }

    try cache.put(to_key(springs.len, correction.len, deja_found), out);
    return out;
}

fn day(contents: []const u8, allocator: std.mem.Allocator) anyerror!void {
    var par = Parser.init(contents);

    var total: usize = 0;
    var total2: usize = 0;

    while (par.peek(0) != undefined) {
        const springs = par.until(' ');

        var springs_total = std.ArrayList(u8).init(allocator);
        defer springs_total.deinit();

        for (0..4) |_| {
            try springs_total.appendSlice(springs);
            try springs_total.append('?');
        }
        try springs_total.appendSlice(springs);

        var corrections_total = std.ArrayList(usize).init(allocator);
        defer corrections_total.deinit();
        var corrections = std.ArrayList(usize).init(allocator);
        defer corrections.deinit();

        while (par.peek(0) != '\n') {
            par.pos += 1;
            try corrections.append(par.number(usize).?);
        }

        for (0..5) |_| {
            try corrections_total.appendSlice(corrections.items);
        }

        par.pos += 1;

        var cache1 = Cache.init(allocator);
        defer cache1.deinit();
        total += try count_springs(springs, corrections.items, 0, &cache1);
        var cache2 = Cache.init(allocator);
        defer cache2.deinit();
        total2 += try count_springs(springs_total.items, corrections_total.items, 0, &cache2);
    }

    std.debug.print("Part1 {}\n", .{total});
    std.debug.print("Part2 {}\n", .{total2});
}
