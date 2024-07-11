const std = @import("std");
const utils = @import("./utils.zig");
const Allocator = std.mem.Allocator;

pub const Error = error{
    EndOfStream,
    Utf8InvalidStartByte,
} || std.fs.File.ReadError || std.fs.File.SeekError || std.mem.Allocator.Error;

pub fn Parser(comptime Value: type, comptime Reader: type) type {
    return struct {
        const Self = @This();
        _parse: *const fn (self: *const Self, allocator: *Allocator, src: *Reader) Error!?Value,

        pub inline fn parse(self: *const Self, allocator: *Allocator, src: *Reader) Error!?Value {
            return self._parse(self, allocator, src);
        }
    };
}

pub fn Literal(comptime Reader: type) type {
    return struct {
        parser: Parser([]u8, Reader) = .{
            ._parse = parse,
        },
        want: []const u8,

        const Self = @This(); // The `want` string must stay alive for as long as the parser will be used.
        pub fn init(want: []const u8) Self {
            return Self{ .want = want };
        }

        // If a value is returned, it is up to the caller to free it.
        pub fn parse(parser: *const Parser([]u8, Reader), allocator: *Allocator, src: *Reader) Error!?[]u8 {
            const self: *const Self = @fieldParentPtr("parser", parser);

            std.debug.print("Allocating {any}\n", .{self.want.len});
            const buf = try allocator.alloc(u8, self.want.len);
            errdefer allocator.free(buf);

            const read = try src.reader().readAll(buf);
            if (read < self.want.len or !std.mem.eql(u8, buf, self.want)) {
                const seekBy: isize = @intCast(read);
                try src.seekableStream().seekBy(-seekBy);
                allocator.free(buf); // parsing failed
                return null;
            }

            return buf;
        }
    };
}

pub fn OneOf(comptime Value: type, comptime Reader: type) type {
    return struct {
        parser: Parser(Value, Reader) = .{
            ._parse = parse,
        },
        parsers: []const *const Parser(Value, Reader),

        const Self = @This();

        // `parsers` slice must stay alive for as long as the parser will be
        // used.
        pub fn init(parsers: []const *const Parser(Value, Reader)) Self {
            const self = Self{ .parsers = parsers };
            return self;
        }

        // Caller is responsible for freeing the value, if any.
        fn parse(parser: *const Parser(Value, Reader), allocator: *Allocator, src: *Reader) Error!?Value {
            const self: *const Self = @fieldParentPtr("parser", parser);
            for (self.parsers) |one_of_parser| {
                const result = try one_of_parser.parse(allocator, src);
                if (result != null) {
                    return result;
                }
            }
            return null;
        }
    };
}

pub fn main() !void {
    const alt_slice: []const i32 = &.{ 1, 2, 3, 4 };

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator = gpa.allocator();

    std.debug.print("Hello world {any}\n", .{alt_slice});

    var buf = "sheep".*;
    var reader = std.io.fixedBufferStream(&buf);
    // Define our parser.
    const onesOfs: []const *const Parser([]u8, @TypeOf(reader)) = &.{
        &Literal(@TypeOf(reader)).init("dog").parser,
        &Literal(@TypeOf(reader)).init("sheep").parser,
        &Literal(@TypeOf(reader)).init("cat").parser,
    };
    var one_of = OneOf([]u8, @TypeOf(reader)).init(onesOfs);
    const parsed = try one_of.parser.parse(&allocator, &reader);

    std.debug.print("Parsed {s}", .{parsed orelse ""});
}
