const std = @import("std");
const expect = std.testing.expect;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var file2 = try std.fs.cwd().openFile("input.txt", .{});
    const contents = try file2.readToEndAlloc(allocator, 20000);
    defer allocator.free(contents);

    var par = std.fmt.Parser{ .buf = contents };
    var max: ?usize = null;
    var maxes: [3]?usize = [3]?usize{ null, null, null };
    var current: usize = 0;

    while (par.peek(0) != undefined) {
        const line = par.number();
        if (line) |num| {
            current += num;
        } else {
            if (max orelse 0 < current) {
                max = current;
            }

            for (0..3) |i| {
                if (maxes[i] orelse 0 < current) {
                    var end: u32 = 2;
                    while (end > i) : (end -= 1) {
                        maxes[end] = maxes[end - 1];
                    }

                    maxes[i] = current;
                    break;
                }
            }
            current = 0;
        }
        par.pos += 1;
    }

    if (max orelse 0 < current) {
        max = current;
    }

    for (0..3) |i| {
        if (maxes[i] orelse 0 < current) {
            var end: u32 = 2;
            while (end > i) : (end -= 1) {
                maxes[end] = maxes[end - 1];
            }

            maxes[i] = current;
            break;
        }
    }

    var part2: usize = 0;
    for (0..3) |i| {
        part2 += maxes[i] orelse 0;
    }

    std.debug.print("Max calories {}\n", .{max orelse 0});
    std.debug.print("Max calories {}\n", .{part2});
}

test "simple test" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    const ctx = std.array_hash_map.AutoContext(i32);
    var map = std.ArrayHashMap(i32, i32, ctx, false).init(std.testing.allocator);
    defer map.deinit();
    try map.put(2, 23);

    std.debug.print("Contains number 2 {}\n", .{map.contains(2)});
    defer list.deinit(); // try commenting this out and see if zig detects the memory leak!
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}

const Suit = enum {
    clubs,
    spades,
    diamonds,
    hearts,
    pub fn isClubs(self: Suit) bool {
        return self == Suit.clubs;
    }
};

fn testing(a: ?i32) ?i32 {
    var b = a orelse return;
    return b + 1;
}

test "enum method" {
    try expect(Suit.spades.isClubs() == Suit.isClubs(.spades));
}
