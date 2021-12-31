package main

import "core:bufio"
import "core:fmt"
import "core:io"
import "core:os"
import "core:strconv"
import "core:strings"

MAX_BUFFER :: 512

BingoBoard :: struct {
  values: [5][5]int,
  markers: [5][5]bool,
}

call_value :: proc(board: ^BingoBoard, called: int) {
  for row, i in board.values {
    for val, j in row {
      if val == called {
        board.markers[i][j] = true
        return
      }
    }
  }
}

has_won :: proc(board: ^BingoBoard) -> bool {
  for i in 0..4 {
    won := true
    for j in 0..4 {
      if !board.markers[i][j] {
        won = false
        break
      }
    }
    if won {
      return true
    }
  }

  for j in 0..4 {
    won := true
    for i in 0..4 {
      if !board.markers[i][j] {
        won = false
        break
      }
    }
    if won {
      return true
    }
  }

  return false
}

score :: proc(board: ^BingoBoard, called: int) -> int {
  s := 0
  for row, i in board.markers {
    for marked, j in row {
      if !marked do s += board.values[i][j];
    }
  }
  return s * called
}

find_first_to_win :: proc(input: []BingoBoard, calls: []int) -> int {
  boards := make([]BingoBoard, len(input))
  copy(boards[:], input[:])
  defer delete(boards)

  for called in calls {
    for _, i in boards {
      call_value(&boards[i], called)
      if has_won(&boards[i]) do return score(&boards[i], called);
    }
  }

  return 0
}

find_last_to_win :: proc(input: []BingoBoard, calls: []int) -> int {
  boards := make([]BingoBoard, len(input))
  copy(boards[:], input[:])
  defer delete(boards)

  last_score := 0

  for called in calls {
    for _, i in boards {
      if has_won(&boards[i]) do continue;
      call_value(&boards[i], called)
      if has_won(&boards[i]) do last_score = score(&boards[i], called);
    }
  }

  return last_score
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

  calls := [dynamic]int{}
  defer delete(calls)

  if ok := bufio.scanner_scan(&scanner); ok {
    line := bufio.scanner_text(&scanner)
    for val, ok := strings.split_iterator(&line, ","); ok; 
        val, ok = strings.split_iterator(&line, ",") {
      num, ok := strconv.parse_int(val)
      if !ok {
        fmt.eprintln("Invalid input")
        os.exit(1)
      }
      append(&calls, num)
    }
  }

  boards := [dynamic]BingoBoard{}
  defer delete(boards)

  elems_count := 0
  for bufio.scanner_scan(&scanner) {
    line := bufio.scanner_text(&scanner)
    if len(line) == 0 {
      append(&boards, BingoBoard{})
      elems_count = 0
      continue
    }
    for val, ok := strings.split_iterator(&line, " "); ok; 
        val, ok = strings.split_iterator(&line, " ") {
      if num, ok := strconv.parse_int(val); ok {
        i, j := elems_count / 5, elems_count % 5
        boards[len(boards) - 1].values[i][j] = num
        elems_count += 1
      }
    }
  }

  if err := bufio.scanner_error(&scanner); err != nil {
    if err != .No_Progress {
      fmt.eprintf("Invalid input. Error: '%s'\n", err)
      os.exit(1)
    }
  }

  score1 := find_first_to_win(boards[:], calls[:])
  fmt.printf("%d\n", score1)

  score2 := find_last_to_win(boards[:], calls[:])
  fmt.printf("%d\n", score2)
}
