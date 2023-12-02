const std = @import("std");

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
    const optimize = b.standardOptimizeOption(.{});

    var iter = (try std.fs.cwd().openIterableDir(
        "./src/",
        .{},
    )).iterate();

    var buffer: [100]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buffer);
    while (try iter.next()) |entry| {
        fba.reset();
        const allocator = fba.allocator();
        if (std.mem.eql(u8, entry.name[0..3], "day")) {
            const day = entry.name[0 .. entry.name.len - 4];
            const path = try std.fmt.allocPrintZ(allocator, "src/{s}", .{entry.name});
            const msg = try std.fmt.allocPrintZ(allocator, "Run day {s}", .{day});

            const exe = b.addExecutable(.{
                .name = day,
                // In this case the main source file is merely a path, however, in more
                // complicated build scripts, this could be a generated file.
                .root_source_file = .{ .path = path },
                .target = target,
                .optimize = optimize,
            });

            // This declares intent for the executable to be installed into the
            // standard location when the user invokes the "install" step (the default
            // step when running `zig build`).
            b.installArtifact(exe);

            // This *creates* a Run step in the build graph, to be executed when another
            // step is evaluated that depends on it. The next line below will establish
            // such a dependency.
            const run_cmd = b.addRunArtifact(exe);

            // By making the run step depend on the install step, it will be run from the
            // installation directory rather than directly from within the cache directory.
            // This is not necessary, however, if the application depends on other installed
            // files, this ensures they will be present and in the expected location.
            run_cmd.step.dependOn(b.getInstallStep());

            // This allows the user to pass arguments to the application in the build
            // command itself, like this: `zig build run -- arg1 arg2 etc`
            if (b.args) |args| {
                run_cmd.addArgs(args);
            }

            // This creates a build step. It will be visible in the `zig build --help` menu,
            // and can be selected like this: `zig build run`
            // This will evaluate the `run` step rather than the default, which is "install".
            const run_step = b.step(day, msg);
            run_step.dependOn(&run_cmd.step);
        }
    }

    // Creates a step for unit testing. This only builds the test executable
    // but does not run it.
    const unit_tests = b.addTest(.{
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });

    const run_unit_tests = b.addRunArtifact(unit_tests);

    // Similar to creating the run step earlier, this exposes a `test` step to
    // the `zig build --help` menu, providing a way for the user to request
    // running the unit tests.
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_unit_tests.step);
}
