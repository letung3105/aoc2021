const std = @import("std");
const heap = std.heap;
const debug = std.debug;
const io = std.io;

const MAX_LINE_SZ = 16;

pub fn main() !void {
    var gpa = heap.GeneralPurposeAllocator(.{}){};
    defer debug.assert(!gpa.deinit());

    var buf: [MAX_LINE_SZ]u8 = undefined;
    var stdin_stream = io.getStdIn().reader();

    var octopusses1: [10][10]u8 = undefined;
    var octopusses2: [10][10]u8 = undefined;
    var line_num: usize = 0;
    while (stdin_stream.readUntilDelimiterOrEof(&buf, '\n')) |maybe_line| {
        const line = maybe_line orelse break;
        for (line) |c, i| {
            octopusses1[line_num][i] = c - '0';
            octopusses2[line_num][i] = c - '0';
        }
        line_num += 1;
    } else |err| return err;

    var flashes: usize = 0;
    var steps: usize = 0;
    while (steps < 100) : (steps += 1) flashes += step(&octopusses1);
    debug.print("{}\n", .{flashes});

    steps = 1;
    while (step(&octopusses2) != 100) steps += 1;
    debug.print("{}\n", .{steps});
}

fn step(octopusses: *[10][10]u8) usize {
    for (octopusses) |*row| {
        for (row) |*octo| octo.* += 1;
    }

    var flashes: usize = 0;
    while (true) {
        var miniflashes: usize = 0;
        for (octopusses) |row, i| {
            for (row) |octo, j| {
                if (octo <= 9) continue;
                miniflashes += 1;
                octopusses[i][j] = 0;
                for (neighbours(.{ .i = @intCast(isize, i), .j = @intCast(isize, j) })) |pos| {
                    if (pos.i < 0 or pos.i > 9 or pos.j < 0 or pos.j > 9) continue;
                    var energy = &octopusses[@intCast(usize, pos.i)][@intCast(usize, pos.j)];
                    if (energy.* != 0) energy.* += 1;
                }
            }
        }
        if (miniflashes == 0) break;
        flashes += miniflashes;
    }
    return flashes;
}

const Pos = struct { i: isize, j: isize };

fn neighbours(pos: Pos) [8]Pos {
    const OFFSETS = [_]Pos{
        .{ .i = 0, .j = 1 },
        .{ .i = 0, .j = -1 },
        .{ .i = 1, .j = 0 },
        .{ .i = 1, .j = 1 },
        .{ .i = 1, .j = -1 },
        .{ .i = -1, .j = 0 },
        .{ .i = -1, .j = 1 },
        .{ .i = -1, .j = -1 },
    };

    var positions: [8]Pos = undefined;
    for (OFFSETS) |offset, i| {
        positions[i] = .{
            .i = pos.i + offset.i,
            .j = pos.j + offset.j,
        };
    }
    return positions;
}
