const std = @import("std");
const utils = @import("./utils.zig");
const Allocator = std.mem.Allocator;

pub const Parser = struct {
    buf: []const u8,
    pos: usize,
    pub fn init(buffer: []const u8) Parser {
        return Parser{ .buf = buffer, .pos = 0 };
    }

    pub fn char(self: *Parser) ?u8 {
        if (self.pos < self.buf.len) {
            self.pos += 1;
            return self.buf[self.pos - 1];
        }
        return null;
    }

    pub fn peek(self: *Parser, at: usize) ?u8 {
        if (self.pos + at < self.buf.len) {
            return self.buf[self.pos + at];
        }
        return null;
    }

    pub fn number(self: *Parser, comptime T: type) ?usize {
        const start = self.pos;
        var at = self.pos;
        while (at < self.buf.len and self.buf[at] >= '0' and self.buf[at] <= '9') {
            at += 1;
        }

        const out = std.fmt.parseInt(T, self.buf[start..at], 10) catch {
            return null;
        };
        self.pos = at;
        return out;
    }
};
