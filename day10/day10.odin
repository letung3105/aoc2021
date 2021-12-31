package main

import "core:bufio"
import "core:fmt"
import "core:io"
import "core:os"
import "core:slice"

BUFFER_CAPACITY :: 512
STACK_CAPACITY :: 512

main :: proc() {
  reader, ok := io.to_reader(os.stream_from_handle(os.stdin))
  if !ok do os.exit(1)

  buf_scanner: bufio.Scanner
  defer bufio.scanner_destroy(&buf_scanner)

  buf: [BUFFER_CAPACITY]u8
  bufio.scanner_init_with_buffer(&buf_scanner, reader, buf[:])

  error_score := 0
  completion_scores: [dynamic]int
  defer delete(completion_scores)

  for bufio.scanner_scan(&buf_scanner) {
    line := bufio.scanner_bytes(&buf_scanner)

    has_error := false
    token_stack: [STACK_CAPACITY]u8
    token_stack_len := 0

    loop: for b in line {
      switch b {
      case '(', '[', '{', '<':
        token_stack[token_stack_len] = b
        token_stack_len += 1
      case ')':
        if token_stack[token_stack_len - 1] == '(' do token_stack_len -= 1
        else {
          error_score += 3
          has_error = true
          break loop
        }
      case ']':
        if token_stack[token_stack_len - 1] == '[' do token_stack_len -= 1
        else {
          error_score += 57
          has_error = true
          break loop
        }
      case '}':
        if token_stack[token_stack_len - 1] == '{' do token_stack_len -= 1
        else {
          error_score += 1197
          has_error = true
          break loop
        }
      case '>':
        if token_stack[token_stack_len - 1] == '<' do token_stack_len -= 1
        else {
          error_score += 25137
          has_error = true
          break loop
        }
      }
    }

    if has_error do continue

    completion_score := 0
    for token_stack_len > 0 {
      token_stack_len -= 1
      switch token_stack[token_stack_len] {
      case '(':
        completion_score *= 5
        completion_score += 1
      case '[':
        completion_score *= 5
        completion_score += 2
      case '{':
        completion_score *= 5
        completion_score += 3
      case '<':
        completion_score *= 5
        completion_score += 4
      }
    }
    append(&completion_scores, completion_score)
  }

  if err := bufio.scanner_error(&buf_scanner); err != nil && err != .No_Progress {
    os.exit(1)
  }

  fmt.printf("%d\n", error_score)

  slice.sort(completion_scores[:])
  fmt.printf("%d\n", completion_scores[len(completion_scores) / 2])
}
