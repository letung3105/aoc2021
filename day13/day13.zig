const std = @import("std");
const fmt = std.fmt;
const debug = std.debug;
const heap = std.heap;
const io = std.io;
const math = std.math;
const mem = std.mem;

const BUFFER_CAPACITY = 16;

pub fn main() !void {
    var gpa = heap.GeneralPurposeAllocator(.{}){};
    defer debug.assert(!gpa.deinit());

    var stdin = io.getStdIn().reader();
    var buf: [BUFFER_CAPACITY]u8 = undefined;

    var coordinates = std.AutoHashMap(Vec2, void).init(gpa.allocator());
    defer coordinates.deinit();

    var xmax: isize = math.minInt(isize);
    var ymax: isize = math.minInt(isize);

    // parse the list of coordinates
    while (stdin.readUntilDelimiterOrEof(&buf, '\n')) |maybe_line| {
        const line = maybe_line orelse break;
        if (line.len == 0) break;

        var line_it = mem.split(u8, line, ",");
        const x = try fmt.parseInt(isize, line_it.next().?, 10);
        const y = try fmt.parseInt(isize, line_it.next().?, 10);

        try coordinates.put(.{ .x = x, .y = y }, .{});
        xmax = math.max(xmax, x);
        ymax = math.max(ymax, y);
    } else |err| return err;

    var folds = std.ArrayList(Fold).init(gpa.allocator());
    defer folds.deinit();

    // parse the list of folds
    while (stdin.readUntilDelimiterOrEof(&buf, '\n')) |maybe_line| {
        const line = maybe_line orelse break;
        if (line.len == 0) break;

        var line_it = mem.split(u8, line, " ");
        _ = line_it.next().?;
        _ = line_it.next().?;

        const fold = line_it.next().?;
        var fold_it = mem.split(u8, fold, "=");

        const axis = fold_it.next().?;
        const coord = try fmt.parseInt(isize, fold_it.next().?, 10);

        if (mem.eql(u8, axis, "x")) {
            try folds.append(.{ .x = coord });
        } else if (mem.eql(u8, axis, "y")) {
            try folds.append(.{ .y = coord });
        }
    } else |err| return err;

    try doFold(folds.items[0], &coordinates);
    debug.print("{}\n", .{coordinates.count()});

    for (folds.items[1..]) |fold| try doFold(fold, &coordinates);
    printPaper(&coordinates);
}

const Vec2 = struct { x: isize, y: isize };

const Fold = union(enum) { x: isize, y: isize };

fn doFold(fold: Fold, coordinates: *std.AutoHashMap(Vec2, void)) !void {
    var gpa = heap.GeneralPurposeAllocator(.{}){};
    defer debug.assert(!gpa.deinit());

    var folded_dots = std.ArrayList(Vec2).init(gpa.allocator());
    defer folded_dots.deinit();

    var keys_it = coordinates.keyIterator();
    switch (fold) {
        .x => |fold_coord| {
            while (keys_it.next()) |coord| {
                if (coord.x > fold_coord) try folded_dots.append(coord.*);
            }
            for (folded_dots.items) |coord| {
                _ = coordinates.remove(coord);
                try coordinates.put(.{ .x = 2 * fold_coord - coord.x, .y = coord.y }, .{});
            }
        },
        .y => |fold_coord| {
            while (keys_it.next()) |coord| {
                if (coord.y > fold_coord) try folded_dots.append(coord.*);
            }
            for (folded_dots.items) |coord| {
                _ = coordinates.remove(coord);
                try coordinates.put(.{ .x = coord.x, .y = 2 * fold_coord - coord.y }, .{});
            }
        },
    }
}

fn printPaper(coordinates: *const std.AutoHashMap(Vec2, void)) void {
    var xmax: isize = math.minInt(isize);
    var ymax: isize = math.minInt(isize);

    var keys_it = coordinates.keyIterator();
    while (keys_it.next()) |coord| {
        xmax = math.max(xmax, coord.x);
        ymax = math.max(ymax, coord.y);
    }

    var y: isize = 0;
    while (y <= ymax) : (y += 1) {
        var x: isize = 0;
        while (x <= xmax) : (x += 1) {
            const c = if (coordinates.contains(.{ .x = x, .y = y })) "#" else ".";
            debug.print("{s}", .{c});
        }
        debug.print("\n", .{});
    }
}
