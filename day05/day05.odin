package main

import "core:bufio"
import "core:fmt"
import "core:io"
import "core:os"
import "core:strconv"
import "core:strings"

MAX_BUFFER :: 64

Vec2 :: struct { x, y: int }

split_pair :: proc(line: ^string, sep: string) -> (v1, v2: string, ok: bool) {
    v1, ok = strings.split_iterator(line, sep)
    v2, ok = strings.split_iterator(line, sep)
    return
}

main :: proc() {
  stdin_reader, ok := io.to_reader(os.stream_from_handle(os.stdin))
  if !ok {
    fmt.eprintln("Could not read input")
    os.exit(1)
  }
  defer io.destroy(stdin_reader)

  buf: [MAX_BUFFER]u8
  scanner: bufio.Scanner
  bufio.scanner_init_with_buffer(&scanner, stdin_reader, buf[:])
  defer bufio.scanner_destroy(&scanner)

  overlaps_counts1 := make(map[Vec2]int)
  defer delete(overlaps_counts1)

  overlaps_counts2 := make(map[Vec2]int)
  defer delete(overlaps_counts2)

  xmax, ymax := min(int), min(int)

  for bufio.scanner_scan(&scanner) {
    line := bufio.scanner_text(&scanner)

    ok: bool
    v1, v2, sx1, sy1, sx2, sy2: string
    x1, y1, x2, y2: int

    v1, v2, ok = split_pair(&line, " -> ")
    if !ok {
      fmt.eprintln("Invalid input")
      os.exit(1)
    }

    sx1, sy1, ok = split_pair(&v1, ",")
    if !ok {
      fmt.eprintln("Invalid input")
      os.exit(1)
    }
    sx2, sy2, ok = split_pair(&v2, ",")
    if !ok {
      fmt.eprintln("Invalid input")
      os.exit(1)
    }

    x1, ok = strconv.parse_int(sx1)
    if !ok {
      fmt.eprintln("Invalid input")
      os.exit(1)
    }
    y1, ok = strconv.parse_int(sy1)
    if !ok {
      fmt.eprintln("Invalid input")
      os.exit(1)
    }
    x2, ok = strconv.parse_int(sx2)
    if !ok {
      fmt.eprintln("Invalid input")
      os.exit(1)
    }
    y2, ok = strconv.parse_int(sy2)
    if !ok {
      fmt.eprintln("Invalid input")
      os.exit(1)
    }

    if x1 > xmax do xmax = x1
    if y1 > ymax do ymax = y1
    if x2 > xmax do xmax = x2
    if y2 > ymax do ymax = y2

    xstart, xend := min(x1, x2), max(x1, x2)
    ystart, yend := min(y1, y2), max(y1, y2)

    if x1 == x2 {
      for j in ystart..yend {
        overlaps_counts1[Vec2{x1, j}] += 1
        overlaps_counts2[Vec2{x1, j}] += 1
      }
    }
    if y1 == y2 {
      for i in xstart..xend {
        overlaps_counts1[Vec2{i, y1}] += 1
        overlaps_counts2[Vec2{i, y1}] += 1
      }
    }
    if xend - xstart == yend - ystart {
      if x1 <= x2 && y1 <= y2 {
        for x, y := x1, y1; x <= x2 && y <= y2; x, y = x + 1, y + 1 {
          overlaps_counts2[Vec2{x, y}] += 1
        }
      } else if x1 <= x2 && y1 >= y2 {
        for x, y := x1, y1; x <= x2 && y >= y2; x, y = x + 1, y - 1 {
          overlaps_counts2[Vec2{x, y}] += 1
        }
      } else if x1 >= x2 && y1 <= y2 {
        for x, y := x1, y1; x >= x2 && y <= y2; x, y = x - 1, y + 1 {
          overlaps_counts2[Vec2{x, y}] += 1
        }
      } else if x1 >= x2 && y1 >= y2 {
        for x, y := x1, y1; x >= x2 && y >= y2; x, y = x - 1, y - 1 {
          overlaps_counts2[Vec2{x, y}] += 1
        }
      }
    }
  }

  if err := bufio.scanner_error(&scanner); err != nil {
    if err != .No_Progress {
      fmt.eprintf("Invalid input. Error: '%s'\n", err)
      os.exit(1)
    }
  }

  count1 := 0
  for _, v in overlaps_counts1 {
    if v > 1 do count1 += 1
  }
  fmt.printf("%d\n", count1)

  count2 := 0
  for _, v in overlaps_counts2 {
    if v > 1 do count2 += 1
  }
  fmt.printf("%d\n", count2)
}