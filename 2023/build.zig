const std = @import("std");

fn add_exec(b: *std.Build, name: []const u8, path: []const u8, msg: []const u8, target: std.Build.ResolvedTarget, optimize: std.builtin.OptimizeMode) void {
    const exe = b.addExecutable(.{
        .name = name,
        // In this case the main source file is merely a path, however, in more
        // complicated build scripts, this could be a generated file.
        .root_source_file = b.path(path),
        .target = target,
        .optimize = optimize,
        .error_tracing = true,
        .unwind_tables = true,
    });
    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    // run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step(name, msg);
    run_step.dependOn(&run_cmd.step);
}
// Although this function looks imperative, note that its job is to
// declaratively construct a build graph that will be executed by an external
// runner.
pub fn build(b: *std.Build) !void {
    // Standard target options allows the person running `zig build` to choose
    // what target to build for. Here we do not override the defaults, which
    // means any target is allowed, and the default is native. Other options
    // for restricting supported target set are available.
    const target = b.standardTargetOptions(.{});

    // Standard optimization options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall. Here we do not
    // set a preferred release mode, allowing the user to decide how to optimize.
    // const optimize = b.standardOptimizeOption(.{ .preferred_optimize_mode = std.builtin.OptimizeMode.ReleaseFast });
    const optimize = b.standardOptimizeOption(.{});

    var buffer: [1000000]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buffer);

    const alloc = fba.allocator();
    const srcDir = try std.fs.cwd().openDir("src", .{ .iterate = true });
    var walker = try srcDir.walk(alloc);

    var innerBuffer: [1000000]u8 = undefined;
    var innerFba = std.heap.FixedBufferAllocator.init(&innerBuffer);

    while (try walker.next()) |entry| {
        innerFba.reset();
        const allocator = innerFba.allocator();
        if (std.mem.eql(u8, entry.basename[0..3], "day")) {
            const day = entry.basename[0 .. entry.basename.len - 4];
            const path = try std.fmt.allocPrintZ(allocator, "src/{s}", .{entry.basename});
            const msg = try std.fmt.allocPrintZ(allocator, "Run day {s}", .{day});

            add_exec(b, day, path, msg, target, optimize);
        }
    }
    add_exec(b, "parsec", "./src/parsec.zig", "do the parsec", target, optimize);
}
