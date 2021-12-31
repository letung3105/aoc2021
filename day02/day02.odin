package main

import "core:bufio"
import "core:fmt"
import "core:io"
import "core:os"
import "core:strconv"
import "core:strings"

MAX_BUFFER :: 64

SubmarineSystem1 :: struct { x, y: int }

run_cmd_with_system1 :: proc(system: ^SubmarineSystem1, cmd: string, num: int) -> bool {
  switch cmd {
    case "forward":
      system.x += num
    case "up":
      system.y -= num
    case "down":
      system.y += num
    case:
      return false
  }
  return true
}

SubmarineSystem2 :: struct { x, y, aim: int }

run_cmd_with_system2 :: proc(system: ^SubmarineSystem2, cmd: string, num: int) -> bool {
  switch cmd {
    case "forward":
      system.x += num
      system.y += num * system.aim
    case "up":
      system.aim -= num
    case "down":
      system.aim += num
    case:
      return false
  }
  return true
}

run_cmd :: proc {
  run_cmd_with_system1,
  run_cmd_with_system2,
}

main :: proc() {
  stdin_reader, ok := io.to_reader(os.stream_from_handle(os.stdin))
  if !ok {
    fmt.println("Could not read input")
    os.exit(1)
  }
  defer io.destroy(stdin_reader)

  buf: [MAX_BUFFER]u8
  scanner: bufio.Scanner
  bufio.scanner_init_with_buffer(&scanner, stdin_reader, buf[:])
  defer bufio.scanner_destroy(&scanner)

  system1 := SubmarineSystem1{}
  system2 := SubmarineSystem2{}

  for bufio.scanner_scan(&scanner) {
    line := bufio.scanner_text(&scanner)

    cmd: string
    if s, ok := strings.split_iterator(&line, " "); ok {
      cmd = s
    } else {
      fmt.println("Bad input")
      os.exit(1)
    }

    num: int
    if s, ok := strings.split_iterator(&line, " "); ok {
      num = strconv.atoi(s)
    } else {
      fmt.println("Bad input")
      os.exit(1)
    }

    if ok := run_cmd(&system1, cmd, num); !ok {
      fmt.println("Bad input")
      os.exit(1)
    }
    if ok := run_cmd(&system2, cmd, num); !ok {
      fmt.println("Bad input")
      os.exit(1)
    }
  }

  if err := bufio.scanner_error(&scanner); err != nil {
    if err != .No_Progress {
      fmt.printf("Could not read input. Error: '%s'\n", err)
      os.exit(1)
    }
  }

  fmt.printf("%d\n", system1.x * system1.y)
  fmt.printf("%d\n", system2.x * system2.y)
}
