const std = @import("std");
const utils = @import("./utils.zig");

pub fn main() !void {
    try utils.mainImpl(day);
}
const Range = struct {
    min: u64,
    max: u64,
    fn new(min: usize, max: usize) Range {
        return Range{
            .min = min,
            .max = max,
        };
    }

    fn split(self: *const Range, op: Operator, at: usize) ?[2]Range {
        if (at < self.max and at >= self.min) {
            switch (op) {
                Operator.Lt => {
                    return .{
                        Range.new(self.min, at),
                        Range.new(at, self.max),
                    };
                },
                Operator.Gt => {
                    return .{
                        Range.new(at + 1, self.max),
                        Range.new(self.min, at + 1),
                    };
                },
            }
        } else {
            return null;
        }
    }
    fn value(self: *const Range) u64 {
        return self.max - self.min;
    }
    fn valid(self: *const Range) bool {
        return self.min < self.max;
    }
};

const RInput = struct {
    x: Range,
    m: Range,
    a: Range,
    s: Range,

    fn start() RInput {
        return RInput{
            .x = Range.new(1, 4001),
            .m = Range.new(1, 4001),
            .a = Range.new(1, 4001),
            .s = Range.new(1, 4001),
        };
    }

    fn valid(self: *const RInput) bool {
        return self.x.valid() and self.m.valid() and self.a.valid() and self.s.valid();
    }

    fn value(self: *const RInput) u64 {
        return self.x.value() * self.m.value() * self.a.value() * self.s.value();
    }

    fn split(self: *RInput, option: *const Option) ?RInput {
        const sidx: usize = 1;
        const oidx: usize = 0;

        switch (option.field) {
            'x' => {
                if (self.x.split(option.op, option.value)) |things| {
                    self.x = things[sidx];
                    return RInput{
                        .x = things[oidx],
                        .m = self.m,
                        .a = self.a,
                        .s = self.s,
                    };
                }
            },
            'm' => {
                if (self.m.split(option.op, option.value)) |things| {
                    self.m = things[sidx];
                    return RInput{
                        .x = self.x,
                        .m = things[oidx],
                        .a = self.a,
                        .s = self.s,
                    };
                }
            },
            'a' => {
                if (self.a.split(option.op, option.value)) |things| {
                    self.a = things[sidx];

                    return RInput{
                        .x = self.x,
                        .m = self.m,
                        .a = things[oidx],
                        .s = self.s,
                    };
                }
            },
            's' => {
                if (self.s.split(option.op, option.value)) |things| {
                    self.s = things[sidx];
                    return RInput{
                        .x = self.x,
                        .m = self.m,
                        .a = self.a,
                        .s = things[oidx],
                    };
                }
            },
            else => unreachable,
        }
        return null;
    }
};

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

fn calc_options(current: []const u8, map: std.StringHashMap(Rule), inpt: *RInput) u64 {
    if (current.len == 1 and (current[0] == 'A' or current[0] == 'R')) {
        if (current[0] == 'A') return inpt.value();
        return 0;
    }
    const rule = map.get(current).?;
    var out: u64 = 0;
    for (rule.constraints.items) |c| {
        if (inpt.split(&c)) |ne| {
            if (ne.valid()) {
                var thing = ne;
                out += calc_options(c.target, map, &thing);
            }
        }
    }
    if (inpt.valid()) {
        out += calc_options(rule.default, map, inpt);
    }

    return out;
}

fn day(contents: []const u8, allocator: std.mem.Allocator) anyerror!void {
    var par = std.fmt.Parser{ .buf = contents };
    var total: usize = 0;

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
    var done = RInput.start();
    std.debug.print("Part2 {}\n", .{calc_options("in", map, &done)});
}
