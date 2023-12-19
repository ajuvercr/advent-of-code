const std = @import("std");
const utils = @import("./utils.zig");

pub fn main() !void {
    try utils.mainImpl(day);
}

const Input = struct {
    x: usize,
    m: usize,
    a: usize,
    s: usize,

    fn get(self: *const Input, field: u8) usize {
        switch (field) {
            'x' => return self.x,
            'm' => return self.m,
            'a' => return self.a,
            's' => return self.s,
            else => unreachable,
        }
    }

    fn value(self: *const Input) usize {
        return self.x + self.m + self.a + self.s;
    }

    fn parse(par: *std.fmt.Parser) ?Input {
        if (par.peek(0) orelse '\n' == '\n') return null;
        par.pos += 3;
        const x = par.number().?;
        par.pos += 3;
        const m = par.number().?;
        par.pos += 3;
        const a = par.number().?;
        par.pos += 3;
        const s = par.number().?;
        par.pos += 1;
        return Input{
            .x = x,
            .m = m,
            .a = a,
            .s = s,
        };
    }
};

const Operator = enum {
    Lt,
    Gt,
    fn parse(par: *std.fmt.Parser) Operator {
        const c = par.char().?;
        return switch (c) {
            '<' => Operator.Lt,
            '>' => Operator.Gt,
            else => unreachable,
        };
    }
};

const Option = struct {
    field: u8,
    value: usize,
    op: Operator,
    target: []const u8,
    fn parse(par: *std.fmt.Parser) ?Option {
        const ending = par.peek(1);
        if (ending != '<' and ending != '>') return null;

        const field = par.char().?;
        const op = Operator.parse(par);
        const value = par.number().?;
        par.pos += 1;
        const target = par.until(',');
        return Option{
            .field = field,
            .value = value,
            .op = op,
            .target = target,
        };
    }

    pub fn format(
        self: Option,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        const c: u8 = switch (self.op) {
            Operator.Gt => '>',
            Operator.Lt => '<',
        };

        try writer.print("{c}{c}{}:{s}", .{ self.field, c, self.value, self.target });
    }

    pub fn works(self: *const Option, input: Input) ?[]const u8 {
        const v = input.get(self.field);
        const valid = switch (self.op) {
            Operator.Lt => v < self.value,
            Operator.Gt => v > self.value,
        };
        if (valid) {
            return self.target;
        }
        return null;
    }
};

const Rule = struct {
    name: []const u8,
    constraints: std.ArrayList(Option),
    default: []const u8,
    fn parse(par: *std.fmt.Parser, allocator: std.mem.Allocator) !?Rule {
        if (par.peek(0) orelse '\n' == '\n') return null;
        var constraints = std.ArrayList(Option).init(allocator);
        const name = par.until('{');
        par.pos += 1;
        while (true) {
            if (Option.parse(par)) |o| {
                try constraints.append(o);
                par.pos += 1;
            } else {
                break;
            }
        }
        const default = par.until('}');
        par.pos += 1;
        return Rule{
            .name = name,
            .constraints = constraints,
            .default = default,
        };
    }

    fn next(self: *const Rule, input: Input) []const u8 {
        var out = self.default;
        for (self.constraints.items) |c| {
            if (c.works(input)) |o| {
                out = o;
                break;
            }
        }
        return out;
    }
    pub fn format(
        self: Rule,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("({s}: ", .{self.name});

        for (self.constraints.items) |c| {
            try writer.print("{},", .{c});
        }

        try writer.print(" -> {s})", .{self.default});
    }
};

fn day(contents: []const u8, allocator: std.mem.Allocator) anyerror!void {
    var par = std.fmt.Parser{ .buf = contents };
    var total: usize = 0;
    var total2: usize = 0;
    _ = total2;

    var map = std.StringHashMap(Rule).init(allocator);
    while (true) {
        if (try Rule.parse(&par, allocator)) |rule| {
            try map.put(rule.name, rule);
            par.pos += 1;
        } else {
            break;
        }
    }
    par.pos += 1;

    while (true) {
        if (Input.parse(&par)) |input| {
            par.pos += 1;
            var start: []const u8 = "in";
            while (start.len != 1 or (start[0] != 'R' and start[0] != 'A')) {
                const v = map.get(start).?;
                start = v.next(input);
            }
            if (start[0] == 'A') {
                total += input.value();
            }
        } else {
            break;
        }
    }
    std.debug.print("Part1 {}\n", .{total});
}
