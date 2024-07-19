const std = @import("std");
const Parser = @import("./parser.zig").Parser;
const utils = @import("./utils.zig");

pub fn main() !void {
    try utils.mainImpl(day);
}

fn get_graph(size: usize, allocator: std.mem.Allocator) ![][]usize {
    var g = try allocator.alloc([]usize, size);
    for (0..size) |i| {
        g[i] = try allocator.alloc(usize, size);
        @memset(g[i], 0);
    }

    return g;
}

fn make_sure_exists(key: []const u8, map: *std.StringHashMap(usize)) !void {
    _ = try map.getOrPutValue(key, @intCast(map.count()));
}

fn build_map(par: *Parser, map: *std.StringHashMap(usize)) !void {
    while (par.peek(0) != undefined) {
        const start = par.until(':');
        try make_sure_exists(start, map);

        par.pos += 2;
        try make_sure_exists(par.buf[par.pos .. par.pos + 3], map);

        par.pos += 3;
        while (par.char().? == ' ') {
            try make_sure_exists(par.buf[par.pos .. par.pos + 3], map);
            par.pos += 3;
        }
    }
}

fn build_graph(par: *Parser, map: *std.StringHashMap(usize), graph: [][]usize) void {
    while (par.peek(0) != undefined) {
        const start = par.until(':');
        const start_idx = map.get(start).?;

        par.pos += 2;
        var key = par.buf[par.pos .. par.pos + 3];
        var target = map.get(key).?;
        graph[start_idx][target] = 1;
        graph[target][start_idx] = 1;

        par.pos += 3;
        while (par.char().? == ' ') {
            key = par.buf[par.pos .. par.pos + 3];
            target = map.get(key).?;
            graph[start_idx][target] = 1;
            graph[target][start_idx] = 1;
            par.pos += 3;
        }
    }
}

const ST = struct {
    s: usize,
    t: usize,
    minCut: usize,
};

fn contract(edge: [][]usize, bin: []bool, vis: []bool, dist: []usize) ST {
    @memset(dist, 0);
    @memset(vis, false);

    const n = edge.len;

    var st = ST{
        .s = 0,
        .t = 0,
        .minCut = std.math.maxInt(usize),
    };

    for (0..n) |i| {
        _ = i;
        var k: usize = 0;
        var maxc: usize = 0;
        var found = false;
        for (0..n) |j| {
            if (bin[j] or vis[j] or dist[j] < maxc) {
                continue;
            }
            found = true;
            k = j;
            maxc = dist[j];
        }
        if (!found) {
            return st;
        }
        st.s = st.t;
        st.t = k;
        st.minCut = maxc;
        vis[k] = true;

        for (0..n) |j| {
            if (bin[j] or vis[j]) continue;
            dist[j] += edge[k][j];
        }
    }
    return st;
}

fn stoer(graph: [][]usize, allocator: std.mem.Allocator) !usize {
    const bin = try allocator.alloc(bool, graph.len);
    @memset(bin, false);
    const vis = try allocator.alloc(bool, graph.len);
    const dist = try allocator.alloc(usize, graph.len);
    const counts = try allocator.alloc(usize, graph.len);
    @memset(counts, 1);

    var minCut: usize = std.math.maxInt(usize);
    var value: usize = std.math.maxInt(usize);
    for (0..graph.len) |i| {
        _ = i;
        const st = contract(graph, bin, vis, dist);
        if (st.minCut == 0) continue;
        bin[st.t] = true;
        if (st.minCut < minCut) {
            minCut = st.minCut;
            if (minCut > 0) {
                value = (graph.len - counts[st.t]) * counts[st.t];
            }
        }
        for (0..graph.len) |j| {
            graph[j][st.s] += graph[j][st.t];
            graph[st.s][j] = graph[j][st.s];
        }
        counts[st.s] += counts[st.t];
        counts[st.t] = counts[st.s];
    }
    return value;
}

fn day(contents: []const u8, allocator: std.mem.Allocator) anyerror!void {
    var par = Parser.init(contents);

    var map = std.StringHashMap(usize).init(allocator);
    try build_map(&par, &map);
    par.pos = 0;

    const graph = try get_graph(@intCast(map.count()), allocator);
    build_graph(&par, &map, graph);

    const minCut = try stoer(graph, allocator);
    std.debug.print("Part 1 {}\n", .{minCut});
}
