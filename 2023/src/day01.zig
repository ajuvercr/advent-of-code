const std = @import("std");
const expect = std.testing.expect;
const utils = @import("./utils.zig");

pub fn main() !void {
    try utils.mainImpl(day);
}

const Literal = struct {
    value: usize,
    name: []const u8,
};

const o_literals = [_]Literal{
    Literal{ .value = 1, .name = "ne" },
};

const t_literals = [_]Literal{
    Literal{ .value = 2, .name = "wo" },
    Literal{ .value = 3, .name = "hree" },
};

const f_literals = [_]Literal{
    Literal{ .value = 4, .name = "our" },
    Literal{ .value = 5, .name = "ive" },
};

const s_literals = [_]Literal{
    Literal{ .value = 6, .name = "ix" },
    Literal{ .value = 7, .name = "even" },
};

const e_literals = [_]Literal{
    Literal{ .value = 8, .name = "ight" },
};

const n_literals = [_]Literal{
    Literal{ .value = 9, .name = "ine" },
};

fn parse_spelled(first: u21, parser: *std.fmt.Parser) ?usize {
    const lits = switch (first) {
        'o' => o_literals[0..],
        't' => t_literals[0..],
        'f' => f_literals[0..],
        's' => s_literals[0..],
        'e' => e_literals[0..],
        'n' => n_literals[0..],
        else => return null,
    };

    for (lits) |lit| {
        if (parser.buf.len < parser.pos + lit.name.len) continue;
        const slice = parser.buf[parser.pos .. parser.pos + lit.name.len];
        if (std.mem.eql(u8, slice, lit.name)) {
            return lit.value;
        }
    }
    return null;
}

const Numbers = struct {
    start: usize,
    last: usize,
    first: bool,

    fn new() Numbers {
        return Numbers{
            .start = 0,
            .last = 0,
            .first = true,
        };
    }

    fn update(self: *Numbers, new_val: usize) void {
        if (self.first) {
            self.start = new_val;
            self.first = false;
        }

        self.last = new_val;
    }

    fn val(self: *Numbers) usize {
        return self.start * 10 + self.last;
    }
};

fn day(contents: []const u8, allocator: std.mem.Allocator) anyerror!void {
    _ = allocator;
    var par = std.fmt.Parser{ .buf = contents };
    var total: usize = 0;
    var total2: usize = 0;

    while (par.peek(0) != undefined) : (par.pos += 1) {
        var part1 = Numbers.new();
        var part2 = Numbers.new();

        while (par.peek(0) orelse '\n' != '\n') {
            const dig = par.char().?;

            if (dig >= '0' and dig <= '9') {
                part1.update(dig - '0');
                part2.update(dig - '0');
                continue;
            }

            if (parse_spelled(dig, &par)) |val| {
                part2.update(val);
                continue;
            }
        }

        total += part1.val();
        total2 += part2.val();
    }

    std.debug.print("Part1 {}\n", .{total});
    std.debug.print("Part2 {}\n", .{total2});
}

test "parse lit" {
    var par = std.fmt.Parser{ .buf = "onettwo" };
    try expect(parse_spelled(&par) == 1);
    try expect(par.char() == 't');

    try expect(parse_spelled(&par) == 2);
    try expect(par.char() == null);
}
