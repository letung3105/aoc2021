use std::{collections::BTreeMap, env};

fn main() {
    let fpath = env::args()
        .nth(1)
        .expect("Path to input file is not given!");
    let s = std::fs::read_to_string(fpath).unwrap();
    let lines = s.lines();

    let pw = read_power_consumption(lines.clone());
    println!("Power consumption: {}", pw);

    let ls = read_life_support_rating(lines);
    println!("Life support rating: {}", ls);
}

fn read_power_consumption<T, V>(bit_patterns: V) -> usize
where
    T: AsRef<str>,
    V: Iterator<Item = T>,
{
    let most_common = most_common(bit_patterns);
    let mut gamma_rate = 0usize;
    let mut sigma_rate = 0usize;
    for (bit_pos, bit) in most_common.iter().enumerate() {
        if *bit {
            gamma_rate |= 1 << bit_pos;
        } else {
            sigma_rate |= 1 << bit_pos;
        }
    }
    gamma_rate * sigma_rate
}

fn read_life_support_rating<T, V>(bit_patterns: V) -> usize
where
    T: AsRef<str>,
    V: Iterator<Item = T>,
{
    let bit_patterns: Vec<String> = bit_patterns.map(|s| s.as_ref().to_string()).collect();
    let mut o2_rating_search = bit_patterns.clone();
    let mut co2_rating_search = bit_patterns;

    let mut pos = 0;
    while o2_rating_search.len() > 1 {
        let most_common = most_common(o2_rating_search.iter());
        let bit = if most_common[most_common.len() - pos - 1] {
            '1'
        } else {
            '0'
        };
        o2_rating_search.retain(|s| s.chars().nth(pos).unwrap() == bit);
        pos += 1;
    }

    let mut pos = 0;
    while co2_rating_search.len() > 1 {
        let most_common = most_common(co2_rating_search.iter());
        let bit = if most_common[most_common.len() - pos - 1] {
            '1'
        } else {
            '0'
        };
        co2_rating_search.retain(|s| s.chars().nth(pos).unwrap() != bit);
        pos += 1;
    }

    let o2_rating = usize::from_str_radix(&o2_rating_search[0], 2).unwrap();
    let co2_rating = usize::from_str_radix(&co2_rating_search[0], 2).unwrap();
    o2_rating * co2_rating
}

fn most_common<T, V>(bit_patterns: V) -> Vec<bool>
where
    T: AsRef<str>,
    V: Iterator<Item = T>,
{
    let mut total = 0;
    let mut ones = BTreeMap::<usize, usize>::new();
    for pattern in bit_patterns {
        for (idx, bit) in pattern.as_ref().chars().rev().enumerate() {
            let freq = ones.entry(idx).or_default();
            if bit == '1' {
                *freq += 1;
            }
        }
        total += 1
    }

    let mut bits = vec![false; ones.len()];
    for (bit_pos, freq) in ones.iter() {
        if *freq * 2 >= total {
            bits[*bit_pos] = true
        }
    }
    bits
}

#[test]
fn day03_part01_example() {
    let bit_patterns = [
        "00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001",
        "00010", "01010",
    ];
    let pw = read_power_consumption(bit_patterns.iter());
    assert_eq!(pw, 198)
}

#[test]
fn day03_part02_example() {
    let bit_patterns = [
        "00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001",
        "00010", "01010",
    ];
    let ls = read_life_support_rating(bit_patterns.iter());
    assert_eq!(ls, 230)
}
