const std = @import("std");

const Allocator = std.mem.Allocator;
const GeneralPurposeAllocator = std.heap.GeneralPurposeAllocator;
const ArrayList = std.ArrayList;

const BUF_SZ: usize = 64;

pub fn main() !void {
    var gpa = GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(!gpa.deinit());

    var pairs = ArrayList(Pair).init(gpa.allocator());
    defer {
        for (pairs.items) |*p| p.deinit();
        pairs.deinit();
    }

    var acc = try Pair.fromStr(gpa.allocator(), "");
    defer acc.deinit();

    var buf: [BUF_SZ]u8 = undefined;
    var istream = std.io.getStdIn().reader();
    while (istream.readUntilDelimiterOrEof(&buf, '\n')) |maybe_line| {
        if (maybe_line) |line| {
            if (line.len == 0) continue;
            var p = try Pair.fromStr(gpa.allocator(), line);
            try pairs.append(p);
            try acc.add(&p);
            try acc.reduce();
        } else break;
    } else |err| {
        return err;
    }

    acc.print();
    const acc_mag = acc.magnitude();
    std.debug.print("{}\n", .{acc_mag});

    var mag_max: isize = std.math.minInt(isize);
    for (pairs.items) |p1, i| {
        for (pairs.items) |p2, j| {
            if (i == j) continue;

            var p = try Pair.fromStr(gpa.allocator(), "");
            defer p.deinit();
            try p.add(&p1);
            try p.reduce();
            try p.add(&p2);
            try p.reduce();

            const mag = try p.magnitude();
            if (mag > mag_max) mag_max = mag;
        }
    }
    std.debug.print("{}\n", .{mag_max});
}

const Pair = struct {
    const Self = @This();
    const Elem = struct { val: isize, lvl: usize };

    elems: ArrayList(Elem),

    fn fromStr(allocator: Allocator, repr: []const u8) !Self {
        var elems = ArrayList(Elem).init(allocator);
        var lvl: usize = 0;
        for (repr) |c| {
            switch (c) {
                '[' => lvl += 1,
                ']' => lvl -= 1,
                '0'...'9' => {
                    const val = try std.fmt.charToDigit(c, 10);
                    try elems.append(.{ .val = val, .lvl = lvl });
                },
                else => {},
            }
        }
        return Self{ .elems = elems };
    }

    fn deinit(self: *Self) void {
        self.elems.deinit();
    }

    fn add(self: *Self, other: *const Self) !void {
        const is_empty = self.elems.items.len == 0;
        try self.elems.appendSlice(other.elems.items);
        if (!is_empty) {
            for (self.elems.items) |*elm| elm.lvl += 1;
        }
    }

    fn reduce(self: *Self) !void {
        var canReduce = self.elems.items.len != 0;
        reduce: while (canReduce) {
            for (self.elems.items[0 .. self.elems.items.len - 1]) |elm, i| {
                if (elm.lvl == 5 and self.elems.items[i + 1].lvl == 5) {
                    self.explode(i);
                    continue :reduce;
                }
            }
            for (self.elems.items) |elm, i| {
                if (elm.val >= 10) {
                    try self.split(i);
                    continue :reduce;
                }
            }
            canReduce = false;
        }
    }

    fn explode(self: *Self, idx: usize) void {
        if (idx > 0)
            self.elems.items[idx - 1].val += self.elems.items[idx].val;
        if (idx + 2 < self.elems.items.len)
            self.elems.items[idx + 2].val += self.elems.items[idx + 1].val;
        self.elems.items[idx].val = 0;
        self.elems.items[idx].lvl -= 1;
        _ = self.elems.orderedRemove(idx + 1);
    }

    fn split(self: *Self, idx: usize) !void {
        const fst = @divFloor(self.elems.items[idx].val, 2);
        const snd = self.elems.items[idx].val - fst;
        const lvl = self.elems.items[idx].lvl + 1;
        try self.elems.insert(idx, .{ .val = fst, .lvl = lvl });
        self.elems.items[idx + 1].val = snd;
        self.elems.items[idx + 1].lvl = lvl;
    }

    fn magnitude(self: *const Self) !isize {
        var gpa = GeneralPurposeAllocator(.{}){};
        defer std.debug.assert(!gpa.deinit());

        var stack = ArrayList(Elem).init(gpa.allocator());
        defer stack.deinit();

        var i: usize = 0;
        while (i < self.elems.items.len) : (i += 1) {
            var val = self.elems.items[i].val;
            var lvl = self.elems.items[i].lvl;
            while (stack.items.len > 0 and lvl == stack.items[stack.items.len - 1].lvl) {
                const lhs = stack.pop();
                lvl -= 1;
                val *= 2;
                val += lhs.val * 3;
            }
            try stack.append(.{ .val = val, .lvl = lvl });
        }
        return stack.pop().val;
    }

    fn print(self: *const Self) void {
        for (self.elems.items) |elm| std.debug.print("{} ", .{elm.val});
        std.debug.print("\n", .{});
    }
};
