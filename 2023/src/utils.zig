const std = @import("std");
pub fn mainImpl(comptime day_f: fn (content: []u8, allocator: std.mem.Allocator) anyerror!void) !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    const args = try std.process.argsAlloc(allocator);

    var inp: []const u8 = undefined;
    if (args.len >= 2) {
        inp = args[1];
    } else {
        inp = "./test.txt";
    }

    var file2 = try std.fs.cwd().openFile(inp, .{});
    const contents = try file2.readToEndAlloc(allocator, 200000);
    defer allocator.free(contents);

    std.debug.print("\n", .{});
    try day_f(contents, allocator);
}
