const std = @import("std");

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const GeneralPurposeAllocator = std.heap.GeneralPurposeAllocator;
const PriorityQueue = std.PriorityQueue;
const Order = std.math.Order;

const MAX_LINE_SZ = 100;

pub fn main() !void {
    var gpa = GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(!gpa.deinit());

    var risks = ArrayList(ArrayList(usize)).init(&gpa.allocator);
    defer {
        for (risks.items) |risks_row| risks_row.deinit();
        risks.deinit();
    }

    var stdin_stream = std.io.getStdIn().reader();
    var buf: [MAX_LINE_SZ]u8 = undefined;
    while (stdin_stream.readUntilDelimiterOrEof(&buf, '\n')) |maybe_line| {
        if (maybe_line) |line| {
            var risks_row = ArrayList(usize).init(&gpa.allocator);
            for (line) |c| try risks_row.append(c - '0');
            try risks.append(risks_row);
        } else break;
    } else |err| {
        std.debug.warn("Could not read input", .{});
        return;
    }

    const minRisk1 = try dijkstra(&risks);
    std.debug.print("{}\n", .{minRisk1});

    try extendVert(&gpa.allocator, &risks);
    try extendHorz(&risks);
    const minRisk2 = try dijkstra(&risks);
    std.debug.print("{}\n", .{minRisk2});
}

fn extendHorz(risks: *ArrayList(ArrayList(usize))) !void {
    for (risks.items) |*risks_row| {
        const n = risks_row.items.len * 4;
        var i: usize = 0;
        while (i < n) : (i += 1) {
            const risk = risks_row.items[i];
            try risks_row.append(if (risk < 9) risk + 1 else 1);
        }
    }
}

fn extendVert(allocator: *Allocator, risks: *ArrayList(ArrayList(usize))) !void {
    const n = risks.items.len * 4;
    var i: usize = 0;
    while (i < n) : (i += 1) {
        var risks_row_new = ArrayList(usize).init(allocator);
        for (risks.items[i].items) |risk|
            try risks_row_new.append(if (risk < 9) risk + 1 else 1);
        try risks.append(risks_row_new);
    }
}

const Position = struct {
    i: usize,
    j: usize,
    risk: usize,
};

fn positionOrder(x: Position, y: Position) Order {
    return std.math.order(x.risk, y.risk);
}

fn dijkstra(risks: *ArrayList(ArrayList(usize))) !usize {
    const nrows = risks.items.len;
    const ncols = risks.items[0].items.len;
    const offsets = [_][2]isize{
        [_]isize{ 0, 1 },
        [_]isize{ 1, 0 },
        [_]isize{ 0, -1 },
        [_]isize{ -1, 0 },
    };

    var gpa = GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(!gpa.deinit());

    var path_risks = ArrayList(ArrayList(usize)).init(&gpa.allocator);
    defer {
        for (path_risks.items) |path_risks_row| path_risks_row.deinit();
        path_risks.deinit();
    }
    for (risks.items) |risks_row, i| {
        var path_risks_row = ArrayList(usize).init(&gpa.allocator);
        for (risks_row.items) |risk, j|
            try path_risks_row.append(std.math.maxInt(usize));
        try path_risks.append(path_risks_row);
    }

    var next_to_visit =
        PriorityQueue(Position).init(&gpa.allocator, positionOrder);
    defer next_to_visit.deinit();

    path_risks.items[0].items[0] = 0;
    try next_to_visit.add(.{ .i = 0, .j = 0, .risk = 0 });

    while (next_to_visit.count() > 0) {
        const pos = next_to_visit.remove();
        if (pos.risk > path_risks.items[pos.i].items[pos.j]) continue;
        for (offsets) |offset| {
            // geting neighbouring positions
            const i = @intCast(isize, pos.i) + offset[0];
            const j = @intCast(isize, pos.j) + offset[1];
            if (i < 0 or
                j < 0 or
                i >= nrows or
                j >= ncols) continue;

            const ii = @intCast(usize, i);
            const jj = @intCast(usize, j);
            const risk_new = pos.risk + risks.items[ii].items[jj];
            const risk_old = path_risks.items[ii].items[jj];
            if (risk_new < risk_old) {
                path_risks.items[ii].items[jj] = risk_new;
                try next_to_visit.add(.{ .i = ii, .j = jj, .risk = risk_new });
            }
        }
    }
    return path_risks.items[nrows - 1].items[ncols - 1];
}

// dynamic programming solution that assumes that we can only go right and down
// won't work :(
fn constructPathMinRisk(risks: *ArrayList(ArrayList(usize))) void {
    const nrows = risks.items.len;
    var i = nrows;
    while (i > 0) : (i -= 1) {
        const ncols = risks.items[i - 1].items.len;
        var j = ncols;
        while (j > 0) : (j -= 1) {
            if (i == nrows and j == ncols) continue;
            var risk_curr = &risks.items[i - 1].items[j - 1];
            const risk_accum1 =
                if (j < ncols) risks.items[i - 1].items[j] + risk_curr.* else std.math.maxInt(usize);
            const risk_accum2 =
                if (i < nrows) risks.items[i].items[j - 1] + risk_curr.* else std.math.maxInt(usize);
            const risk_accum =
                if (risk_accum1 < risk_accum2) risk_accum1 else risk_accum2;
            risk_curr.* = if (i == 1 and j == 1) risk_accum - risk_curr.* else risk_accum;
        }
    }
}
