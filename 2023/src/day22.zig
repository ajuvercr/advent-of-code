const std = @import("std");
const Parser = @import("./parser.zig").Parser;
const utils = @import("./utils.zig");

fn DField(comptime T: type) type {
    return struct {
        x_l: usize,
        y_l: usize,
        z_l: usize,

        buf: []T,

        const Self = @This();

        fn init(width: usize, depth: usize, height: usize, alloctor: std.mem.Allocator, start: T) !Self {
            const buf = try alloctor.alloc(T, width * depth * height);
            @memset(buf, start);

            return Self{
                .x_l = width,
                .y_l = depth,
                .z_l = height,
                .buf = buf,
            };
        }

        fn get(self: *const Self, triple: Triple) T {
            const idx = triple.x * self.y_l * self.z_l + triple.y * self.z_l + triple.z;
            return self.buf[idx];
        }

        fn set(self: *const Self, triple: Triple, value: T) void {
            const idx = triple.x * self.y_l * self.z_l + triple.y * self.z_l + triple.z;
            self.buf[idx] = value;
        }

        fn set_ret(self: *Self, triple: Triple, value: T) T {
            const idx = triple.x * self.y_l * self.z_l + triple.y * self.z_l + triple.z;
            const out = self.buf[idx];
            self.buf[idx] = value;
            return out;
        }
    };
}

const Triple = struct {
    x: usize,
    y: usize,
    z: usize,

    fn init(x: usize, y: usize, z: usize) Triple {
        return Triple{
            .x = x,
            .y = y,
            .z = z,
        };
    }

    fn parse(par: *Parser) Triple {
        const x = par.number(usize).?;
        par.pos += 1;
        const y = par.number(usize).?;
        par.pos += 1;
        const z = par.number(usize).?;
        par.pos += 1;

        return Triple.init(x, y, z);
    }

    fn add(self: Triple, other: Triple) Triple {
        return Triple.init(self.x + other.x, self.y + other.y, self.z + other.z);
    }

    fn eq(self: Triple, other: Triple) bool {
        return self.x == other.x and self.y == other.y and self.z == other.z;
    }
};

fn _delta(x: usize, y: usize) usize {
    if (x > y) {
        return 1;
    }
    return 0;
}
const Block = struct {
    // Smaller of the two
    from: Triple,
    to: Triple,

    delta: Triple,
    id: usize,

    fn init(from: Triple, to: Triple, id: usize) Block {
        if (from.x > to.x or from.y > to.y or from.z > to.z) {
            return Block.init(to, from, id);
        } else {
            const delta = Triple.init(
                _delta(to.x, from.x),
                _delta(to.y, from.y),
                _delta(to.z, from.z),
            );

            return Block{
                .from = from,
                .to = to,
                .delta = delta,
                .id = id,
            };
        }
    }

    fn set_v(self: *const Block, field: *DField(usize), value: usize) void {
        var f = self.from;
        field.set(f, value);
        while (!f.eq(self.to)) {
            f = f.add(self.delta);
            field.set(f, value);
        }
    }

    fn set(self: *const Block, field: *DField(usize)) void {
        self.set_v(field, self.id);
    }

    fn good_location(self: *const Block, triple: Triple, field: *const DField(usize)) bool {
        const c = field.get(Triple.init(triple.x, triple.y, triple.z - 1));
        return c == 0 or c == self.id;
    }

    fn lower(self: *Block, field: *DField(usize)) bool {
        var lowered = false;
        while (self.from.z > 1) {
            var f = self.from;
            if (!self.good_location(f, field)) return lowered;
            while (!f.eq(self.to)) {
                f = f.add(self.delta);
                if (!self.good_location(f, field)) return lowered;
            }

            // all is good, we can lower

            lowered = true;
            self.set_v(field, 0);
            self.to = Triple.init(self.to.x, self.to.y, self.to.z - 1);
            self.from = Triple.init(self.from.x, self.from.y, self.from.z - 1);
            self.set(field);
        }
        return lowered;
    }

    fn set_deps(self: *const Block, field: *const DField(usize), dependencies: *DField(bool)) void {
        var f = self.from;
        var f_z = Triple.init(f.x, f.y, f.z - 1);

        if (!self.good_location(f, field)) {
            dependencies.set(Triple.init(self.id, field.get(f_z), 0), true);
        }

        while (!f.eq(self.to)) {
            f = f.add(self.delta);
            f_z = Triple.init(f.x, f.y, f.z - 1);
            if (!self.good_location(f, field)) {
                dependencies.set(Triple.init(self.id, field.get(f_z), 0), true);
            }
        }
    }
};

fn can_be_disintegrated(dependencies: *DField(bool), max: usize) usize {
    var out: usize = 0;
    for (1..max) |at| {
        var self_can_be_disintegrated: bool = true;
        for (1..max) |i| {
            if (dependencies.get(Triple.init(i, at, 0))) { // Something rests on me
                var count: usize = 0;
                for (1..max) |j| {
                    if (dependencies.get(Triple.init(i, j, 0))) { // What else does it rest on
                        count += 1;
                    }
                }
                std.debug.assert(count > 0);
                if (count == 1) {
                    self_can_be_disintegrated = false;
                    break;
                }
            }
        }
        if (self_can_be_disintegrated) {
            out += 1;
        }
    }
    return out;
}

fn disintegrated_determine_fall(blocks: []Block, field: *DField(usize), alloctor: std.mem.Allocator) !usize {
    const block_cpy = try alloctor.alloc(Block, blocks.len);
    @memcpy(block_cpy, blocks);
    const cpy = try alloctor.alloc(usize, field.buf.len);
    @memcpy(cpy, field.buf);

    var fallen = try alloctor.alloc(bool, blocks.len);
    var out: usize = 0;

    for (0..blocks.len) |i| {
        @memcpy(blocks, block_cpy);
        @memset(fallen, false);
        @memcpy(field.buf, cpy);
        blocks[i].set_v(field, 0);

        var changed = true;
        while (changed) {
            changed = false;
            for (0..blocks.len) |j| {
                if (i == j) continue;
                if (blocks[j].lower(field)) {
                    fallen[j] = true;
                    changed = true;
                }
            }
        }

        for (0..blocks.len) |j| {
            if (fallen[j]) out += 1;
        }
    }
    return out;
}

pub fn main() !void {
    try utils.mainImpl(day);
}

fn day(contents: []const u8, allocator: std.mem.Allocator) anyerror!void {
    var par = Parser.init(contents);
    var max_x: usize = 0;
    var max_y: usize = 0;
    var max_z: usize = 0;

    var list = std.ArrayList(Block).init(allocator);
    defer list.deinit();

    var idx: usize = 1;
    while (par.peek(0) != undefined) {
        const from = Triple.parse(&par);
        const to = Triple.parse(&par);
        max_x = @max(max_x, to.x);
        max_y = @max(max_y, to.y);
        max_z = @max(max_z, to.z);
        max_x = @max(max_x, from.x);
        max_y = @max(max_y, from.y);
        max_z = @max(max_z, from.z);
        try list.append(Block.init(from, to, idx));
        idx += 1;
    }

    var field = try DField(usize).init(max_x + 1, max_y + 1, max_z + 1, allocator, 0);

    for (list.items) |b| {
        b.set(&field);
    }

    var changed = true;
    while (changed) {
        changed = false;
        for (0..list.items.len) |i| {
            changed = changed or list.items[i].lower(&field);
        }
    }

    var dependencies = try DField(bool).init(idx, idx, 1, allocator, false);

    for (list.items) |b| {
        b.set_deps(&field, &dependencies);
    }

    std.debug.print("Part1 {any}\n", .{can_be_disintegrated(&dependencies, idx)});
    std.debug.print("Part2 {any}\n", .{try disintegrated_determine_fall(list.items, &field, allocator)});
}
