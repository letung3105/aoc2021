package main

import "core:fmt"
import "core:os"
import "core:io"
import "core:strconv"
import "core:strings"

BUF_CAP :: 4 * 1024

main :: proc() {
  stdin_reader, ok := io.to_reader(os.stream_from_handle(os.stdin))
  if !ok {
    fmt.eprintln("Could not read input")
    os.exit(1)
  }
  defer io.destroy(stdin_reader)

  buf: [BUF_CAP]u8
  buf_len := 0
  for {
    n, err := io.read(stdin_reader, buf[buf_len:])
    if n == 0 do break
    if err != nil {
      if err != .No_Progress do break
      fmt.eprintf("Invalid input. Error %s\n", err)
      os.exit(1)
    }
    buf_len += n
  }
  input := string(buf[:buf_len])

  crabs := [dynamic]int{}
  defer delete(crabs)

  pos_min := max(int)
  pos_max := min(int)

  for {
    if val, ok := strings.split_iterator(&input, ","); ok {
      val = strings.trim_space(val)
      if num, ok := strconv.parse_int(val); ok {
        append(&crabs, num)
        pos_min = min(pos_min, num)
        pos_max = max(pos_max, num)
      } else {
        fmt.eprintf("Invalid input, got %s of size %d\n", val, len(val))
        os.exit(1)
      }
    } else do break
  }

  fuel_min1 := max(int)
  fuel_min2 := max(int)
  for pos in pos_min..pos_max {
    fuel1 := 0
    fuel2 := 0
    for crab in crabs {
      steps := max(pos, crab) - min(pos, crab)
      fuel1 += steps
      fuel2 += steps * (steps + 1) / 2
    }
    fuel_min1 = min(fuel_min1, fuel1)
    fuel_min2 = min(fuel_min2, fuel2)
  }

  fmt.printf("%d\n", fuel_min1)
  fmt.printf("%d\n", fuel_min2)
}
