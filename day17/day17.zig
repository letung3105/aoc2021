const std = @import("std");

const BUF_SZ = 64;

pub fn main() !void {
    var buf: [BUF_SZ]u8 = undefined;
    var istream = std.io.getStdIn().reader();
    const maybe_line = try istream.readUntilDelimiterOrEof(&buf, '\n');
    const line = maybe_line.?;

    var line_split = std.mem.split(u8, line, ": ");
    _ = line_split.next().?;
    const target = line_split.next().?;

    var target_split = std.mem.split(u8, target, ", ");
    const xtarget = target_split.next().?;
    const ytarget = target_split.next().?;

    var xtarget_split = std.mem.split(u8, xtarget, "=");
    _ = xtarget_split.next().?;
    const xlims = xtarget_split.next().?;

    var ytarget_split = std.mem.split(u8, ytarget, "=");
    _ = ytarget_split.next().?;
    const ylims = ytarget_split.next().?;

    var xlims_split = std.mem.split(u8, xlims, "..");
    const xlower = try std.fmt.parseInt(isize, xlims_split.next().?, 10);
    const xupper = try std.fmt.parseInt(isize, xlims_split.next().?, 10);

    var ylims_split = std.mem.split(u8, ylims, "..");
    const ylower = try std.fmt.parseInt(isize, ylims_split.next().?, 10);
    const yupper = try std.fmt.parseInt(isize, ylims_split.next().?, 10);

    const dx0_lower = std.math.min(0, xlower);
    const dx0_upper = std.math.max(0, xupper);
    const dy0_lower = -300;
    const dy0_upper = 300;

    var count: usize = 0;
    var ymax: isize = std.math.minInt(isize);
    var dx0: isize = dx0_lower;
    while (dx0 <= dx0_upper) : (dx0 += 1) {
        var dy0: isize = dy0_lower;
        while (dy0 <= dy0_upper) : (dy0 += 1) {
            const result = simulate(dx0, dy0, xlower, xupper, ylower, yupper);
            if (result.succeeded) {
                if (ymax < result.ymax) ymax = result.ymax;
                count += 1;
            }
        }
    }
    std.debug.print("{}\n", .{ymax});
    std.debug.print("{}\n", .{count});
}

const SimulationResult = struct {
    succeeded: bool,
    ymax: isize,
};

fn simulate(dx0: isize, dy0: isize, xlower: isize, xupper: isize, ylower: isize, yupper: isize) SimulationResult {
    var ddx: isize = if (dx0 > 0) -1 else 1;
    var dx = dx0;
    var dy = dy0;
    var x: isize = 0;
    var y: isize = 0;
    var result = SimulationResult{
        .succeeded = false,
        .ymax = std.math.minInt(isize),
    };
    while (y >= ylower) {
        if (x >= xlower and
            x <= xupper and
            y >= ylower and
            y <= yupper) result.succeeded = true;
        if (result.ymax < y) result.ymax = y;
        if (dx == 0) ddx = 0;

        x += dx;
        y += dy;
        dx += ddx;
        dy -= 1;
    }
    return result;
}
