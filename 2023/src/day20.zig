const std = @import("std");
const Parser = @import("./parser.zig").Parser;
const utils = @import("./utils.zig");

fn FieldChangeN(comptime amount: usize) type {
    return struct {
        high: [amount]usize,
        high_c: usize,
        low: [amount]usize,
        low_c: usize,

        const Self = @This();

        pub fn init() Self {
            var out = Self{
                .high = undefined,
                .high_c = 0,
                .low = undefined,
                .low_c = 0,
            };

            for (0..amount) |i| {
                out.high[i] = 0;
                out.low[i] = 0;
            }

            return out;
        }

        fn handle(self: *Self, input: bool, iteration: usize) void {
            if (input) {
                if (self.high_c < amount) {
                    self.high[self.high_c] = iteration;
                    self.high_c += 1;
                }
            } else {
                if (self.low_c < amount) {
                    self.low[self.low_c] = iteration;
                    self.low_c += 1;
                }
            }
        }

        fn done(self: *Self) bool {
            return self.low_c == amount and self.high_c == amount;
        }

        pub fn format(
            self: *const Self,
            comptime fmt: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            _ = fmt;
            _ = options;

            try writer.print("high {}", .{self.high[0]});
            for (1..self.high_c) |i| {
                try writer.print(" -{}- {}", .{ self.high[i] - self.high[i - 1], self.high[i] });
            }

            try writer.print("\tlow {}", .{self.low[0]});
            for (1..self.low_c) |i| {
                try writer.print(" -{}- {}", .{ self.low[i] - self.low[i - 1], self.low[i] });
            }
        }

        fn cycle(self: *const Self) usize {
            std.debug.assert(amount >= 2);
            return self.high[1] - self.high[0];
        }
    };
}

fn lcm(v1: usize, v2: usize) usize {
    return v1 / std.math.gcd(v1, v2) * v2;
}

pub fn main() !void {
    try utils.mainImpl(day);
}
const Pulse = struct {
    high: bool,
    target: *Module,
    origin: *Module,
};

const Type = enum { flip, conj, broad };
const Module = struct {
    input_c: usize,
    output_c: usize,
    name: []const u8,
    input: [50]*Module,
    output: [50]*Module,
    memory: []bool,
    ty: Type,

    fn input_idx(self: *const Module, module: *Module) usize {
        for (0..self.input_c) |i| {
            if (module == self.input[i]) return i;
        }

        unreachable;
    }

    fn output_idx(self: *const Module, module: *Module) usize {
        for (0..self.output_c) |i| {
            if (module == self.output[i]) return i;
        }

        unreachable;
    }

    fn wanted_mem(self: *const Module) usize {
        return switch (self.ty) {
            Type.flip => 1,
            Type.conj => self.input_c,
            Type.broad => 0,
        };
    }

    fn add_input(self: *Module, input: *Module) void {
        self.input[self.input_c] = input;
        self.input_c += 1;
    }
    fn add_output(self: *Module, output: *Module) void {
        self.output[self.output_c] = output;
        self.output_c += 1;
    }

    fn handle(self: *Module, pulse: Pulse, todo: *std.TailQueue(Pulse), allocator: std.mem.Allocator) !void {
        var high: bool = pulse.high;
        switch (self.ty) {
            Type.broad => {},
            Type.flip => {
                if (pulse.high) return;
                self.memory[0] = !self.memory[0];
                high = self.memory[0];
            },
            Type.conj => {
                const idx = self.input_idx(pulse.origin);
                self.memory[idx] = pulse.high; // actually this is is_high
                high = false;
                for (0..self.input_c) |i| {
                    if (!self.memory[i]) {
                        high = true;
                        break;
                    }
                }
            },
        }

        for (0..self.output_c) |i| {
            const target = self.output[i];
            var node = try allocator.create(std.TailQueue(Pulse).Node);
            node.data.target = target;
            node.data.high = high;
            node.data.origin = self;
            todo.append(node);
        }
    }
};

fn init_module(modules: *std.StringArrayHashMap(*Module), name: []const u8, allocator: std.mem.Allocator) !*Module {
    if (modules.get(name)) |ptr| {
        return ptr;
    } else {
        const out = try allocator.create(Module);
        out.name = name;
        out.input_c = 0;
        out.output_c = 0;
        try modules.put(name, out);
        return out;
    }
}

fn day(contents: []const u8, allocator: std.mem.Allocator) anyerror!void {
    var par = Parser.init(contents);
    var total2: usize = 0;

    var modules = std.StringArrayHashMap(*Module).init(allocator);

    while (par.peek(0) != undefined) : (par.pos += 1) {
        const c = par.peek(0) orelse unreachable;

        var ty = Type.broad;
        switch (c) {
            '%' => {
                par.pos += 1;
                ty = Type.flip;
            },
            '&' => {
                par.pos += 1;
                ty = Type.conj;
            },
            else => {},
        }
        const name = par.until(' ');

        var module = try init_module(&modules, name, allocator);
        module.ty = ty;
        par.pos += 4;

        while (par.peek(0) != '\n') {
            const o = par.buf[par.pos .. par.pos + 2];
            par.pos += 2;
            var out_module = try init_module(&modules, o, allocator);

            module.add_output(out_module);
            out_module.add_input(module);

            if (par.peek(0) == ',') par.pos += 2;
        }
    }

    var mem_req: usize = 0;
    for (modules.values()) |module| {
        mem_req += module.wanted_mem();
    }

    var mem = try allocator.alloc(bool, mem_req);
    var i: usize = 0;
    for (modules.values()) |module| {
        const len = module.wanted_mem();
        module.memory = mem[i .. i + len];
        i += module.wanted_mem();
    }

    var low: usize = 0;
    var high: usize = 0;

    var queue = std.TailQueue(Pulse){};
    const buffer = try allocator.alloc(u8, 500000000);
    var fba = std.heap.FixedBufferAllocator.init(buffer);

    const broadcaster = modules.get("broadcaster").?;
    const rx_ptr = modules.get("rx").?;
    const gf_ptr = rx_ptr.input[0];

    var gf_inputs = try allocator.alloc(FieldChangeN(2), gf_ptr.input_c);
    for (0..gf_inputs.len) |gf_input| {
        gf_inputs[gf_input] = FieldChangeN(2).init();
    }

    @memset(mem, false);
    var done = false;
    while (!done) {
        fba.reset();
        const stack_alloc = fba.allocator();

        total2 += 1;
        var node = try stack_alloc.create(std.TailQueue(Pulse).Node);
        node.data.target = broadcaster;
        node.data.high = false;
        node.data.origin = undefined;
        queue.append(node);

        while (queue.popFirst()) |first| {
            const pulse = first.data;
            if (pulse.high) {
                high += 1;
            } else {
                low += 1;
            }
            if (first.data.target == gf_ptr) {
                for (0..gf_inputs.len) |gf_input| {
                    if (gf_ptr.input[gf_input] == first.data.origin) {
                        gf_inputs[gf_input].handle(first.data.high, total2);
                    }
                }
            }
            try first.data.target.handle(pulse, &queue, stack_alloc);
        }

        done = true;
        for (0..gf_inputs.len) |gf_input| {
            done = done and gf_inputs[gf_input].done();
        }

        if (total2 == 1000) {
            std.debug.print("Part1 {}\n", .{high * low});
        }
    }

    var part2: usize = 1;
    for (0..gf_inputs.len) |gf_input| {
        const mod = gf_inputs[gf_input];
        std.debug.print("Input {} {s}: {}\n", .{ gf_input, gf_ptr.input[gf_input].name, mod });
        part2 = lcm(part2, mod.cycle());
    }

    std.debug.print("Part2 {}\n", .{part2});
}
