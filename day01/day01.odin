package main

import "core:bufio"
import "core:fmt"
import "core:io"
import "core:math"
import "core:os"
import "core:strconv"

main :: proc() {
  stdin_reader, ok := io.to_reader(os.stream_from_handle(os.stdin))
  if !ok {
    fmt.println("Could not read input")
    os.exit(1)
  }
  defer io.destroy(stdin_reader)

  buf: [4096]u8
  s: bufio.Scanner
  defer bufio.scanner_destroy(&s)
  bufio.scanner_init_with_buffer(&s, stdin_reader, buf[:])

  // part 01 states
  prev := max(int)
  increments := 0

  // part 02 states
  count := 0
  rolling_window : [3]int
  rolling_sum_increments := 0

  for (bufio.scanner_scan(&s)) {
    line := bufio.scanner_text(&s)
    num := strconv.atoi(line)

    if num > prev {
      increments += 1
    }
    prev = num

    rolling_sum_prev := math.sum(rolling_window[:])
    rolling_window[0] = rolling_window[1]
    rolling_window[1] = rolling_window[2]
    rolling_window[2] = num

    count += 1
    if count > 3 && math.sum(rolling_window[:]) > rolling_sum_prev {
      rolling_sum_increments += 1
    }
  }

  if err := bufio.scanner_error(&s); err != nil {
    if err != .No_Progress {
      fmt.printf("Could not read input. Error: '%s'\n", err)
      os.exit(1)
    }
  }

  fmt.printf("Part 01 %d\n", increments)
  fmt.printf("Part 02 %d\n", rolling_sum_increments)
}
