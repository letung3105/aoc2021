const std = @import("std");
const debug = std.debug;
const heap = std.heap;
const io = std.io;
const math = std.math;
const mem = std.mem;

const BUFFER_CAPACITY = 32;

const Polymere = std.AutoHashMap([2]u8, usize);

const Insertion = std.AutoHashMap([2]u8, u8);

pub fn main() !void {
    var gpa = heap.GeneralPurposeAllocator(.{}){};
    defer debug.assert(!gpa.deinit());

    var stdin = io.getStdIn().reader();
    var buf: [BUFFER_CAPACITY]u8 = undefined;

    var pair_counts = Polymere.init(gpa.allocator());
    defer pair_counts.deinit();

    var insertion_rules = Insertion.init(gpa.allocator());
    defer insertion_rules.deinit();

    const polymere = (try stdin.readUntilDelimiterOrEof(&buf, '\n')).?;
    var polymere_idx: usize = 0;
    while (polymere_idx < polymere.len - 1) : (polymere_idx += 1) {
        const pair = [_]u8{ polymere[polymere_idx], polymere[polymere_idx + 1] };
        var entry = try pair_counts.getOrPut(pair);
        if (entry.found_existing) entry.value_ptr.* += 1 else entry.value_ptr.* = 1;
    }

    while (stdin.readUntilDelimiterOrEof(&buf, '\n')) |maybe_line| {
        const line = maybe_line orelse break;
        if (line.len == 0) continue;

        var line_it = mem.split(u8, line, " -> ");
        const pair = line_it.next().?;
        debug.assert(pair.len == 2);
        const elem = line_it.next().?;
        debug.assert(elem.len == 1);
        try insertion_rules.put([_]u8{ pair[0], pair[1] }, elem[0]);
    } else |err| return err;

    // PART 01

    var steps: usize = 10;
    while (steps > 0) : (steps -= 1) try insert(&pair_counts, &insertion_rules);

    var elem_counts1 = std.AutoHashMap(u8, usize).init(gpa.allocator());
    defer elem_counts1.deinit();
    try countElem(&elem_counts1, &pair_counts);

    const ans1 = getAns(&elem_counts1);
    debug.print("{}\n", .{ans1});

    // PART 02

    steps = 30;
    while (steps > 0) : (steps -= 1) try insert(&pair_counts, &insertion_rules);

    var elem_counts2 = std.AutoHashMap(u8, usize).init(gpa.allocator());
    defer elem_counts2.deinit();
    try countElem(&elem_counts2, &pair_counts);

    const ans2 = getAns(&elem_counts2);
    debug.print("{}\n", .{ans2});
}

fn getAns(elem_counts: *std.AutoHashMap(u8, usize)) usize {
    var max_elem_counts: usize = math.minInt(usize);
    var min_elem_counts: usize = math.maxInt(usize);
    var elem_counts_it = elem_counts.valueIterator();
    while (elem_counts_it.next()) |v| {
        max_elem_counts = math.max(max_elem_counts, v.*);
        min_elem_counts = math.min(min_elem_counts, v.*);
    }
    return max_elem_counts - min_elem_counts;
}

fn countElem(elem_counts: *std.AutoHashMap(u8, usize), pair_counts: *const Polymere) !void {
    var pair_counts_it = pair_counts.iterator();
    while (pair_counts_it.next()) |kv| {
        const pair = kv.key_ptr.*;
        const count = kv.value_ptr.*;

        var entry1 = try elem_counts.getOrPut(pair[0]);
        if (entry1.found_existing) entry1.value_ptr.* += count else entry1.value_ptr.* = count;

        var entry2 = try elem_counts.getOrPut(pair[1]);
        if (entry2.found_existing) entry2.value_ptr.* += count else entry2.value_ptr.* = count;
    }

    var elem_counts_it = elem_counts.valueIterator();
    while (elem_counts_it.next()) |v| v.* = try math.divCeil(usize, v.*, 2);
}

fn insert(pair_counts: *Polymere, insertion_rules: *const Insertion) !void {
    var gpa = heap.GeneralPurposeAllocator(.{}){};
    defer debug.assert(!gpa.deinit());

    var new_counts = Polymere.init(gpa.allocator());
    defer new_counts.deinit();

    var pair_counts_it = pair_counts.iterator();
    while (pair_counts_it.next()) |kv| {
        const pair = kv.key_ptr.*;
        const count = kv.value_ptr.*;

        if (insertion_rules.get(pair)) |elem| {
            const p1 = [_]u8{ elem, pair[1] };
            const p2 = [_]u8{ pair[0], elem };

            var entry1 = try new_counts.getOrPut(p1);
            if (entry1.found_existing) entry1.value_ptr.* += count else entry1.value_ptr.* = count;

            var entry2 = try new_counts.getOrPut(p2);
            if (entry2.found_existing) entry2.value_ptr.* += count else entry2.value_ptr.* = count;

            kv.value_ptr.* -= count;
        }
    }

    var new_counts_it = new_counts.iterator();
    while (new_counts_it.next()) |kv| {
        const pair = kv.key_ptr.*;
        const count = kv.value_ptr.*;

        var entry = try pair_counts.getOrPut(pair);
        if (entry.found_existing) entry.value_ptr.* += count else entry.value_ptr.* = count;
    }
}

fn printPairCounts(pair_counts: *const Polymere) void {
    var it = pair_counts.iterator();
    while (it.next()) |kv| {
        debug.print("{s}: {}\n", .{ kv.key_ptr.*, kv.value_ptr.* });
    }
}

fn printElemCounts(elem_counts: *const std.AutoHashMap(u8, usize)) void {
    var it = elem_counts.iterator();
    while (it.next()) |kv| {
        debug.print("{c}: {}\n", .{ kv.key_ptr.*, kv.value_ptr.* });
    }
}

fn printInsertionRule(insertion_rules: *const Insertion) void {
    var it = insertion_rules.iterator();
    while (it.next()) |kv| {
        debug.print("{s} -> {c}\n", .{ kv.key_ptr.*, kv.value_ptr.* });
    }
}
