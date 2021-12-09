use std::env;

fn main() {
    let fpath = env::args()
        .nth(1)
        .expect("Path to input file is not given!");
    let s = std::fs::read_to_string(fpath).unwrap();

    let fishes: Vec<u64> = s
        .trim()
        .split(',')
        .map(|s| s.parse::<u64>().unwrap())
        .collect();

    let mut cycles = [0u64; 9];
    for cycle in &fishes {
        cycles[*cycle as usize] += 1;
    }

    let n = simulate_v1(fishes, 80);
    println!("{}", n);

    let n = simulate_v2(cycles, 80);
    println!("{}", n);

    let n = simulate_v2(cycles, 256);
    println!("{}", n);
}

// This version is gonna be slow
fn simulate_v1(mut fishes: Vec<u64>, days: u64) -> u64 {
    for _ in 0..days {
        let mut n_new_fishes = 0;
        for fish in fishes.iter_mut() {
            *fish = if *fish == 0 {
                n_new_fishes += 1;
                6
            } else {
                *fish - 1
            };
        }
        fishes.extend(vec![8; n_new_fishes]);
    }
    fishes.len() as u64
}

fn simulate_v2(mut cycles: [u64; 9], days: u64) -> u64 {
    for _ in 0..days {
        let n_new_fishes = cycles[0];
        for i in 0..8 {
            cycles[i] = cycles[i + 1]
        }
        cycles[6] += n_new_fishes;
        cycles[8] = n_new_fishes;
    }
    cycles.iter().sum()
}

#[test]
fn day06_example01() {
    let fishes = vec![3, 4, 3, 1, 2];
    let mut cycles = [0u64; 9];
    for cycle in &fishes {
        cycles[*cycle as usize] += 1;
    }
    assert_eq!(simulate_v1(fishes, 18), 26);
    assert_eq!(simulate_v2(cycles, 18), 26);
}

#[test]
fn day06_example02() {
    let fishes = vec![3, 4, 3, 1, 2];
    let mut cycles = [0u64; 9];
    for cycle in &fishes {
        cycles[*cycle as usize] += 1;
    }
    assert_eq!(simulate_v1(fishes, 80), 5934);
    assert_eq!(simulate_v2(cycles, 80), 5934);
}

#[test]
fn day06_example03() {
    let fishes = vec![3, 4, 3, 1, 2];
    let mut cycles = [0u64; 9];
    for cycle in &fishes {
        cycles[*cycle as usize] += 1;
    }
    assert_eq!(simulate_v2(cycles, 256), 26984457539);
}
