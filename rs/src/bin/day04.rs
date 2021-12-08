use std::env;

fn main() {
    let fpath = env::args()
        .nth(1)
        .expect("Path to input file is not given!");
    let s = std::fs::read_to_string(fpath).unwrap();

    let mut lines = s.lines();
    let simulation = read_simulation(&mut lines);
    let boards = read_boards(&mut lines);

    let mut bingo = BingoSession::new(boards.clone());
    let (board_id, score) = bingo.simulate_win_first(&simulation).unwrap();
    println!("Board {} will win first with score = {}", board_id, score);

    let mut bingo = BingoSession::new(boards);
    let (board_id, score) = bingo.simulate_win_last(&simulation).unwrap();
    println!("Board {} will win last with score = {}", board_id, score);
}

fn read_simulation<T, V>(mut data: V) -> Vec<u64>
where
    T: AsRef<str>,
    V: Iterator<Item = T>,
{
    let simulation = data.next().unwrap();
    data.next().unwrap(); // skip new line
    simulation
        .as_ref()
        .split(',')
        .map(|s| s.parse::<u64>().unwrap())
        .collect()
}

fn read_boards<T, V>(data: V) -> Vec<Board>
where
    T: AsRef<str>,
    V: Iterator<Item = T>,
{
    let mut boards = vec![];
    let mut cache = vec![];
    for line in data {
        let row: Vec<u64> = line
            .as_ref()
            .split_whitespace()
            .map(|s| s.trim().parse::<u64>().unwrap())
            .collect();
        if row.is_empty() {
            let board = Board::new(cache.clone());
            boards.push(board);
            cache.clear();
        } else {
            cache.push(row);
        }
    }
    let board = Board::new(cache);
    boards.push(board);
    boards
}

#[derive(Clone)]
struct BingoSession {
    boards: Vec<Board>,
}

impl BingoSession {
    fn new(boards: Vec<Board>) -> Self {
        Self { boards }
    }

    fn simulate_win_first(&mut self, values: &[u64]) -> Option<(usize, u64)> {
        for value in values {
            for (idx, board) in self.boards.iter_mut().enumerate() {
                board.mark(*value);
                if board.has_won() {
                    return Some((idx, board.score(*value)));
                }
            }
        }
        None
    }

    fn simulate_win_last(&mut self, values: &[u64]) -> Option<(usize, u64)> {
        let mut last = None;
        for value in values {
            for (idx, board) in self.boards.iter_mut().enumerate() {
                if board.has_won() {
                    continue;
                }
                board.mark(*value);
                if board.has_won() {
                    last = Some((idx, board.score(*value)));
                }
            }
        }
        last
    }
}

#[derive(Clone)]
struct Board {
    values: Vec<Vec<u64>>,
    markers: Vec<Vec<bool>>,
}

impl Board {
    fn new(values: Vec<Vec<u64>>) -> Self {
        for row in &values {
            assert_eq!(row.len(), values.len())
        }
        let markers = vec![vec![false; values.len()]; values.len()];
        Self { values, markers }
    }

    fn mark(&mut self, value: u64) {
        for (row, row_markers) in self.values.iter_mut().zip(self.markers.iter_mut()) {
            for (val, marker) in row.iter_mut().zip(row_markers.iter_mut()) {
                if *val == value {
                    *marker = true;
                }
            }
        }
    }

    fn has_won(&self) -> bool {
        for row_markers in self.markers.iter() {
            if row_markers.iter().all(|x| *x) {
                return true;
            }
        }

        let mut is_col_marked = vec![true; self.values.len()];
        for row_markers in self.markers.iter() {
            for (marker, marked) in row_markers.iter().zip(is_col_marked.iter_mut()) {
                *marked &= marker
            }
        }
        is_col_marked.iter().any(|x| *x)
    }

    fn score(&self, last_called_value: u64) -> u64 {
        last_called_value
            * self
                .values
                .iter()
                .zip(self.markers.iter())
                .map(|(row, row_markers)| row.iter().zip(row_markers.iter()))
                .flatten()
                .filter_map(|(val, marker)| if !*marker { Some(*val) } else { None })
                .sum::<u64>()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_DATA: &str = r#"7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7"#;

    #[test]
    fn test_read_simulation() {
        let mut lines = TEST_DATA.lines();
        let simulation = read_simulation(&mut lines);
        assert_eq!(
            simulation,
            [
                7, 4, 9, 5, 11, 17, 23, 2, 0, 14, 21, 24, 10, 16, 13, 6, 15, 25, 12, 22, 18, 20, 8,
                19, 3, 26, 1
            ]
        );
    }

    #[test]
    fn test_read_boards() {
        let mut lines = TEST_DATA.lines().skip(2);
        let boards = read_boards(&mut lines);
        assert_eq!(
            boards[0].values,
            vec![
                vec![22, 13, 17, 11, 0],
                vec![8, 2, 23, 4, 24],
                vec![21, 9, 14, 16, 7],
                vec![6, 10, 3, 18, 5],
                vec![1, 12, 20, 15, 19],
            ]
        );
        assert_eq!(
            boards[1].values,
            vec![
                vec![3, 15, 0, 2, 22],
                vec![9, 18, 13, 17, 5],
                vec![19, 8, 7, 25, 23],
                vec![20, 11, 10, 24, 4],
                vec![14, 21, 16, 12, 6],
            ]
        );

        assert_eq!(
            boards[2].values,
            vec![
                vec![14, 21, 17, 24, 4],
                vec![10, 16, 15, 9, 19],
                vec![18, 8, 23, 26, 20],
                vec![22, 11, 13, 6, 5],
                vec![2, 0, 12, 3, 7],
            ]
        );
    }

    #[test]
    fn day04_part01_example() {
        let mut lines = TEST_DATA.lines();
        let simulation = read_simulation(&mut lines);
        let boards = read_boards(&mut lines);
        let mut bingo = BingoSession::new(boards);
        let (_, score) = bingo.simulate_win_first(&simulation).unwrap();
        assert_eq!(score, 4512);
    }

    #[test]
    fn day04_part02_example() {
        let mut lines = TEST_DATA.lines();
        let simulation = read_simulation(&mut lines);
        let boards = read_boards(&mut lines);
        let mut bingo = BingoSession::new(boards);
        let (_, score) = bingo.simulate_win_last(&simulation).unwrap();
        assert_eq!(score, 1924);
    }
}
