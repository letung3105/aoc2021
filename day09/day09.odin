package main

import "core:bufio"
import "core:fmt"
import "core:intrinsics"
import "core:math"
import "core:os"
import "core:io"
import "core:strconv"
import "core:strings"

BUF_CAP :: 1024
MAX_ROWS :: 100
MAX_COLS :: 100

HeightMap :: [MAX_ROWS][MAX_COLS]u8

Vec2 :: struct { x, y: int }

neighbours :: proc(pos: Vec2) -> [4]Vec2 {
  OFFSETS :: [4]Vec2{Vec2{1, 0}, Vec2{0, 1}, Vec2{-1, 0}, Vec2{0, -1}}
  out: [4]Vec2
  for offset, i in OFFSETS do out[i] = Vec2{pos.x + offset.x, pos.y + offset.y}
  return out
}

find_low_points :: proc(lowpoints: ^[dynamic]Vec2, heightmap: HeightMap, heightmap_rows, heightmap_cols: int) -> int {
  n := 0
  for y in 0..<heightmap_rows {
    for x in 0..<heightmap_cols {
      pos := Vec2{x, y}
      islow := true

      for pos_neighbour in neighbours(pos) {
        if pos_neighbour.x < 0 || pos_neighbour.x >= heightmap_cols ||
           pos_neighbour.y < 0 || pos_neighbour.y >= heightmap_rows {
         continue
        }
        if heightmap[pos_neighbour.y][pos_neighbour.x] <= heightmap[y][x] {
          islow = false
          break
        }
      }

      if islow {
        append(lowpoints, pos)
        n += 1
      }
    }
  }
  return n
}

flood :: proc(basin: ^[dynamic]Vec2, pos_start: Vec2, heightmap: HeightMap, heightmap_rows, heightmap_cols: int) -> int {
  stack := [dynamic]Vec2{}
  defer delete(stack)

  visited := [MAX_ROWS][MAX_COLS]bool{}
  n := 0
  append(&stack, pos_start)

  for len(stack) > 0 {
    pos := pop(&stack)
    if visited[pos.y][pos.x] do continue

    visited[pos.y][pos.x] = true
    n += 1
    append(basin, pos)

    for pos_neighbour in neighbours(pos) {
      if pos_neighbour.x < 0 || pos_neighbour.x >= heightmap_cols ||
         pos_neighbour.y < 0 || pos_neighbour.y >= heightmap_rows {
       continue
      }

      height_neighbour := heightmap[pos_neighbour.y][pos_neighbour.x]
      if height_neighbour != 9 && 
         height_neighbour > heightmap[pos.y][pos.x] {
        append(&stack, pos_neighbour)
      }
    }
  }
  return n
}

main :: proc() {
  stdin_reader, ok := io.to_reader(os.stream_from_handle(os.stdin))
  if !ok {
    fmt.eprintln("Could not read input")
    os.exit(1)
  }
  defer io.destroy(stdin_reader)

  buf: [BUF_CAP]u8
  scanner: bufio.Scanner
  bufio.scanner_init_with_buffer(&scanner, stdin_reader, buf[:])
  defer bufio.scanner_destroy(&scanner)

  heightmap: HeightMap
  heightmap_rows, heightmap_cols := 0, 0

  for bufio.scanner_scan(&scanner) {
    line := bufio.scanner_text(&scanner)
    heightmap_cols = 0
    for c in line {
      heightmap[heightmap_rows][heightmap_cols] = u8(c - '0')
      heightmap_cols += 1
    }
    heightmap_rows += 1
  }

  if err := bufio.scanner_error(&scanner); err != nil {
    if err != .No_Progress {
      fmt.eprintf("Invalid input. Error: '%s'\n", err)
      os.exit(1)
    }
  }

  fmt.printf("%d %d\n", heightmap_rows, heightmap_cols)

  lowpoints := [dynamic]Vec2{}
  defer delete(lowpoints)
  find_low_points(&lowpoints, heightmap, heightmap_rows, heightmap_cols)

  // PART 01

  total_risk := uint(0)
  for pos in lowpoints do total_risk += uint(heightmap[pos.y][pos.x]) + 1
  fmt.printf("%d\n", total_risk)

  // PART 02

  top3 := [3]int{min(int), min(int), min(int)}
  for pos in lowpoints {
    basin := [dynamic]Vec2{}
    defer delete(basin)
    basin_sz := flood(&basin, pos, heightmap, heightmap_rows, heightmap_cols)
    for sz, i in top3 {
      if sz < basin_sz {
        for j := 2; j > i; j -= 1 do top3[j] = top3[j-1]
        top3[i] = basin_sz
        break
      }
    }
  }
  fmt.printf("%v\n", top3)
  size_product := math.prod(top3[:])
  fmt.printf("%d\n", size_product)
}
