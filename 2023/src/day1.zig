const std = @import("std");
const expect = std.testing.expect;

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

fn parse_spelled(first: u8, parser: *std.fmt.Parser) ?usize {
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

pub fn day(file: []const u8) !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var file2 = try std.fs.cwd().openFile(file, .{});
    const contents = try file2.readToEndAlloc(allocator, 200000);
    defer allocator.free(contents);

    var par = std.fmt.Parser{ .buf = contents };
    var total: usize = 0;

    while (par.peek(0) != undefined) : (par.pos += 1) {
        var first = true;
        var start: usize = 0;
        var last: usize = 0;

        while (par.peek(0) orelse '\n' != '\n') {
            const dig = par.char().?;

            if (dig >= '0' and dig <= '9') {
                if (first) {
                    start = dig - '0';
                    first = false;
                }

                last = dig - '0';
                continue;
            }

            if (parse_spelled(dig, &par)) |val| {
                if (first) {
                    start = val;
                    first = false;
                }

                last = val;
                continue;
            }
        }

        total += start * 10 + last;
    }

    std.debug.print("Part1 {}\n", .{total});
}

test "parse lit" {
    var par = std.fmt.Parser{ .buf = "onettwo" };
    try expect(parse_spelled(&par) == 1);
    try expect(par.char() == 't');

    try expect(parse_spelled(&par) == 2);
    try expect(par.char() == null);
}
