const std = @import("std");
const utils = @import("./utils.zig");

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
    ty: Type,
    name: []const u8,
    memory: []bool,
    input: []*Module,
    output: []*Module,

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
    fn add_output(self: *Module, input: *Module) void {
        self.output[self.input_c] = input;
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
                self.memory[idx] = pulse.high;
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

fn init_module(modules: *std.StringArrayHashMap(Module), name: []const u8, allocator: std.mem.Allocator) !*Module {
    if (modules.getPtr(name)) |out| {
        std.debug.print("Found module! {s} {*}\n", .{ out.name, out });

        return out;
    } else {
        try modules.put(name, Module{
            .name = name,
            .ty = undefined,
            .memory = undefined,
            .input_c = 0,
            .output_c = 0,
            .input = try allocator.alloc(*Module, 50),
            .output = try allocator.alloc(*Module, 50),
        });
        const ptr = modules.getPtr(name).?;
        std.debug.print("Didn't find module! {s} {*}\n", .{ name, ptr });

        return ptr;
    }
}

fn day(contents: []const u8, allocator: std.mem.Allocator) anyerror!void {
    std.debug.print("Module size {}\n", .{@sizeOf(Module)});
    var par = std.fmt.Parser{ .buf = contents };
    var total2: usize = 0;

    var modules = std.StringArrayHashMap(Module).init(allocator);

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
        std.debug.print("------- {s} -------- {} {}\n", .{ module.name, module.input_c, module.output_c });
        module.ty = ty;
        par.pos += 4;

        var i: usize = 0;
        while (par.peek(i) != '\n') {
            if (par.peek(i) == ',') {
                const o = par.until(',');
                var out_module = try init_module(&modules, o, allocator);
                std.debug.print("Adding output {s} {}\n", .{ out_module.name, out_module.name.len });

                std.debug.print("To self {*}\n", .{module});
                module.output[module.input_c] = out_module;
                module.output_c += 1;
                out_module.input[out_module.input_c] = module;
                out_module.input_c += 1;
                std.debug.print("To out\n", .{});
                par.pos += 2;
                i = 0;
            }
            i += 1;
        }

        const o = par.until('\n');
        var out_module = try init_module(&modules, o, allocator);
        std.debug.print("Last Adding output {s}\n", .{out_module.name});
        std.debug.print("To self\n", .{});
        std.debug.print("To out\n", .{});
        module.output[module.input_c] = out_module;
        module.output_c += 1;
        out_module.input[out_module.input_c] = module;
        out_module.input_c += 1;
    }

    var mem_req: usize = 0;
    for (modules.values()) |module| {
        mem_req += module.wanted_mem();
    }

    var mem = try allocator.alloc(bool, mem_req);
    var i: usize = 0;
    for (modules.values()) |*module| {
        const len = module.wanted_mem();
        module.memory = mem[i .. i + len];
        i += module.wanted_mem();
    }

    const low: usize = 0;
    const high: usize = 0;
    var rx: usize = 0;
    var queue = std.TailQueue(Pulse){};
    const buffer = try allocator.alloc(u8, 500000000);
    var fba = std.heap.FixedBufferAllocator.init(buffer);

    const broadcaster = modules.getPtr("broadcaster").?;
    const rx_ptr = modules.getPtr("rx").?;
    @memset(mem, false);
    while (rx != 1) {
        fba.reset();
        const stack_alloc = fba.allocator();
        if (total2 % 1000000 == 0)
            std.debug.print("{}: rx {}\n ", .{ total2, rx });
        total2 += 1;
        rx = 0;
        var node = try stack_alloc.create(std.TailQueue(Pulse).Node);
        node.data.target = broadcaster;
        node.data.high = false;
        node.data.origin = undefined;
        queue.append(node);

        while (queue.popFirst()) |first| {
            const pulse = first.data;
            if (first.data.target == rx_ptr) {
                std.debug.print("Rx ptr!\n", .{});
                if (!pulse.high)
                    rx += 1;
            } else {
                try first.data.target.handle(pulse, &queue, stack_alloc);
            }
        }
    }

    std.debug.print("Part1 {}\n", .{low * high});
    std.debug.print("Part2 {}\n", .{total2});
}
