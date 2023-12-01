const std = @import("std");
const expect = std.testing.expect;

const Literal = struct {
    value: usize,
    name: []const u8,
};
const literals = [_]Literal{
    // Literal{ .value = 0, .name = "zero" },
    Literal{ .value = 1, .name = "one" },
    Literal{ .value = 2, .name = "two" },
    Literal{ .value = 3, .name = "three" },
    Literal{ .value = 4, .name = "four" },
    Literal{ .value = 5, .name = "five" },
    Literal{ .value = 6, .name = "six" },
    Literal{ .value = 7, .name = "seven" },
    Literal{ .value = 8, .name = "eight" },
    Literal{ .value = 9, .name = "nine" },
};
fn parse_spelled(parser: *std.fmt.Parser) ?usize {
    for (literals) |lit| {
        if (parser.buf.len < parser.pos + lit.name.len) continue;
        const slice = parser.buf[parser.pos .. parser.pos + lit.name.len];
        if (std.mem.eql(u8, slice, lit.name)) {
            parser.pos += 1;
            return lit.value;
        }
    }
    return null;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var file2 = try std.fs.cwd().openFile("input.txt", .{});
    const contents = try file2.readToEndAlloc(allocator, 200000);
    defer allocator.free(contents);

    var par = std.fmt.Parser{ .buf = contents };
    var total: usize = 0;

    while (par.peek(0) != undefined) : (par.pos += 1) {
        var start: ?usize = null;
        var last: usize = 0;
        while (par.peek(0) orelse '\n' != '\n') {
            if (parse_spelled(&par)) |val| {
                if (start == null) {
                    start = val;
                }

                last = val;
                continue;
            }

            const dig = par.char().?;
            if (dig >= '0' and dig <= '9') {
                if (start == null) {
                    start = dig - '0';
                }

                last = dig - '0';
            }
        }

        total += (start orelse 0) * 10 + last;
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
