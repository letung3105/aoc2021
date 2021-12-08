use std::env;

fn main() {
    let fpath = env::args()
        .nth(1)
        .expect("Path to input file is not given!");
    let s = std::fs::read_to_string(fpath).unwrap();

    let positions: Vec<u64> = s
        .trim()
        .split(',')
        .map(|s| s.parse::<u64>().unwrap())
        .collect();
    let pos_min = *positions.iter().min().unwrap();
    let pos_max = *positions.iter().max().unwrap();

    // brute force
    let mut min = u64::MAX;
    for i in pos_min..=pos_max {
        let mut s = 0;
        for &pos in &positions {
            s += pos.max(i) - pos.min(i);
        }
        min = min.min(s);
    }
    println!("{}", min);

    // brute force
    let mut min = u64::MAX;
    for i in pos_min..=pos_max {
        let mut s = 0;
        for &pos in &positions {
            let n = pos.max(i) - pos.min(i);
            s += (n * (n + 1)) / 2;
        }
        min = min.min(s);
    }
    println!("{}", min);
}
