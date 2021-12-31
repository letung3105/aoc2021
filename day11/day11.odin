package main

import "core:bufio"
import "core:fmt"
import "core:io"
import "core:os"

BUFFER_CAPACITY :: 512

main :: proc() {
  reader, ok := io.to_reader(os.stream_from_handle(os.stdin))
  if !ok do os.exit(1)

  buf_scanner: bufio.Scanner
  defer bufio.scanner_destroy(&buf_scanner)

  buf: [BUFFER_CAPACITY]u8
  bufio.scanner_init_with_buffer(&buf_scanner, reader, buf[:])

  octopusses1: [10][10]u8
  octopusses2: [10][10]u8
  i := 0
  for bufio.scanner_scan(&buf_scanner) {
    line := bufio.scanner_bytes(&buf_scanner)
    for c, j in line {
      octopusses1[i][j] = c - '0'
      octopusses2[i][j] = c - '0'
    }
    i += 1
  }
  if err := bufio.scanner_error(&buf_scanner); err != nil && err != .No_Progress {
    os.exit(1)
  }

  flashes := uint(0)
  for _ in 0..99 {
    flashes += step(&octopusses1)
  }
  fmt.println(flashes)

  steps := 1
  for step(&octopusses2) != 100 do steps += 1
  fmt.println(steps)
}

step :: proc(octopusses: ^[10][10]u8) -> uint {
  // increase all energy by one
  for i in 0..9 do for j in 0..9 do octopusses[i][j] += 1

  flashes: uint = 0
  for {
    flashes_inner: uint = 0
    for i in 0..9 {
      for j in 0..9 {
        // these octopuses can't flash
        if octopusses[i][j] <= 9 do continue

        flashes_inner += 1
        // reset energy once flashed
        octopusses[i][j] = 0

        for pos in neighbours(Vec2{j, i}) {
          // skip out-of-bound
          if pos.x < 0 || pos.x > 9 || pos.y < 0 || pos.y > 9 do continue
          // only increase energy of those that haven't flashed
          if octopusses[pos.y][pos.x] != 0 do octopusses[pos.y][pos.x] += 1
        }
      }
    }
    // stop the process if none flashed
    if flashes_inner == 0 do break
    flashes += flashes_inner
  }

  return flashes
}

Vec2 :: struct { x, y: int }

neighbours :: proc(pos: Vec2) -> [8]Vec2 {
  OFFSETS :: [8]Vec2{
    Vec2{1, 0}, Vec2{0, 1}, Vec2{1, 1}, Vec2{-1, -1}, 
    Vec2{-1, 0}, Vec2{0, -1}, Vec2{-1, 1}, Vec2{1, -1},
  }
  out: [8]Vec2
  for offset, i in OFFSETS do out[i] = Vec2{pos.x + offset.x, pos.y + offset.y}
  return out
}

