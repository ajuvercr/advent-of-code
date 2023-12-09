const std = @import("std");
const utils = @import("./utils.zig");

pub fn main() !void {
    try utils.mainImpl(day);
}

const Type = enum(u4) {
    Five,
    Four,
    Full,
    Three,
    TwoPair,
    OnePair,
    High,

    fn typePart1(counts: [13]usize) Type {
        var twos: usize = 0;
        var three: bool = false;
        for (0..13) |i| {
            switch (counts[i]) {
                5 => return Type.Five,
                4 => return Type.Four,
                3 => three = true,
                2 => twos += 1,
                else => {},
            }
        }
        if (three and twos > 0) return Type.Full;
        if (twos == 2) return Type.TwoPair;
        if (twos == 1) return Type.OnePair;
        if (three) return Type.Three;
        return Type.High;
    }

    fn typePart2(counts: [13]usize) Type {
        var twos: usize = 0;
        var three: bool = false;
        for (1..13) |i| {
            switch (counts[i]) {
                5 => return Type.Five,
                4 => {
                    if (counts[0] == 1) {
                        return Type.Five;
                    } else {
                        return Type.Four;
                    }
                },
                3 => three = true,
                2 => twos += 1,
                else => {},
            }
        }
        switch (counts[0]) {
            0 => {
                if (three and twos > 0) return Type.Full;
                if (three) return Type.Three;
                if (twos == 2) return Type.TwoPair;
                if (twos == 1) return Type.OnePair;
                return Type.High;
            },
            1 => {
                if (three) return Type.Four;
                if (twos == 2) return Type.Full;
                if (twos == 1) return Type.Three;
                return Type.OnePair;
            },
            2 => {
                if (three) return Type.Five;
                if (twos > 0) return Type.Four;
                return Type.Three;
            },
            3 => {
                if (twos > 0) return Type.Five;
                return Type.Four;
            },
            else => return Type.Five,
        }
    }

    fn new(counts: [13]usize, part1: bool) Type {
        if (part1) return Type.typePart1(counts);
        return Type.typePart2(counts);
    }
};

const Hand = struct {
    cards: [5]u8,
    counts: [13]usize,
    bid: usize,
    type: Type,

    fn new(incoming_cards: []const u8, bid: usize, part1: bool) Hand {
        var cards = std.mem.zeroes([5]u8);
        for (0..5) |i| {
            const c = incoming_cards[i];
            // 2 3 4 5 6 7 8 9 A K Q J T
            if (c >= '2' and c <= '9') {
                if (part1) {
                    cards[i] = c - '2';
                } else {
                    cards[i] = c - '2' + 1;
                }
            } else {
                if (part1) {
                    cards[i] = switch (c) {
                        'T' => 8,
                        'J' => 9,
                        'Q' => 10,
                        'K' => 11,
                        'A' => 12,
                        else => unreachable,
                    };
                } else {
                    cards[i] = switch (c) {
                        'T' => 9,
                        'J' => 0,
                        'Q' => 10,
                        'K' => 11,
                        'A' => 12,
                        else => unreachable,
                    };
                }
            }
        }

        var counts = std.mem.zeroes([13]usize);

        for (0..5) |i| {
            counts[cards[i]] += 1;
        }
        const ty = Type.new(counts, part1);

        return Hand{
            .counts = counts,
            .bid = bid,
            .cards = cards,
            .type = ty,
        };
    }

    fn bigger(self: *Hand, other: *Hand) bool {
        if (self.type < other.type) return true;
        if (self.type > other.type) return false;
        for (0..5) |i| {
            const a = self.cards[i];
            const b = other.cards[i];
            if (a > b) return true;
            if (a < b) return false;
        }
    }
};

fn sortBy(ctx: void, a: Hand, b: Hand) bool {
    _ = ctx;
    if (@intFromEnum(a.type) < @intFromEnum(b.type)) return true;
    if (@intFromEnum(a.type) > @intFromEnum(b.type)) return false;
    for (0..5) |i| {
        const av = a.cards[i];
        const bv = b.cards[i];
        if (av > bv) return true;
        if (av < bv) return false;
    }
    unreachable;
}

fn day(contents: []const u8, allocator: std.mem.Allocator) anyerror!void {
    var par = std.fmt.Parser{ .buf = contents };

    var hands1 = std.ArrayList(Hand).init(allocator);
    var hands2 = std.ArrayList(Hand).init(allocator);

    while (par.peek(0) != undefined) {
        const cards = par.buf[par.pos .. par.pos + 5];
        par.pos += 6;
        const bid = par.number().?;

        try hands1.append(Hand.new(cards, bid, true));
        try hands2.append(Hand.new(cards, bid, false));
        par.pos += 1;
    }

    std.sort.heap(Hand, hands1.items, {}, sortBy);
    std.sort.heap(Hand, hands2.items, {}, sortBy);

    var total1: usize = 0;
    var total2: usize = 0;
    for (0..hands1.items.len) |i| {
        total1 += (hands1.items.len - i) * hands1.items[i].bid;
        total2 += (hands2.items.len - i) * hands2.items[i].bid;
    }

    std.debug.print("Part1 {}\n", .{total1});
    std.debug.print("Part2 {}\n", .{total2});
}
