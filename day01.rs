use std::{env, ops::Add};

fn main() {
    let fpath = env::args()
        .nth(1)
        .expect("Path to input file is not given!");
    let s = std::fs::read_to_string(fpath).unwrap();
    let xs = s
        .lines()
        .map(|l| l.parse::<u64>().expect("Line's content is not a number!"));

    let n = count_increments(xs.clone());
    println!("Number of increments: {:?}", n);

    let n = count_increments(rolling_sum(xs).iter());
    println!("Number of sum increments: {:?}", n);
}

fn count_increments<T, V>(values: V) -> usize
where
    T: Ord,
    V: Iterator<Item = T> + Clone,
{
    values
        .clone()
        .zip(values.skip(1))
        .filter(|(x, y)| x < y)
        .count()
}

fn rolling_sum<T, V>(values: V) -> Vec<T>
where
    T: Add<Output = T>,
    V: Iterator<Item = T> + Clone,
{
    values
        .clone()
        .zip(values.clone().skip(1))
        .zip(values.skip(2))
        .map(|((x, y), z)| x + y + z)
        .collect()
}
