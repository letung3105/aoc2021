use std::{collections::HashMap, env};

fn main() {
    let fpath = env::args()
        .nth(1)
        .expect("Path to input file is not given!");
    let s = std::fs::read_to_string(fpath).unwrap();
    let lines = s.lines();

    let n1 = count_intersections(lines.clone(), false);
    println!("Number of intersections {}", n1);

    let n2 = count_intersections(lines, true);
    println!("Number of intersections {}", n2);
}

fn count_intersections<T, V>(data: V, diagonal: bool) -> usize
where
    T: AsRef<str>,
    V: Iterator<Item = T>,
{
    let parse_point = |s: &str| -> (u64, u64) {
        let mut pair = s.split(',');
        let x = pair.next().unwrap();
        let y = pair.next().unwrap();
        (x.parse().unwrap(), y.parse().unwrap())
    };

    let mut counts = HashMap::<(u64, u64), u64>::new();
    for line in data {
        let line = line.as_ref();
        let mut arrow = line.split("->");
        let start = arrow.next().unwrap().trim();
        let stop = arrow.next().unwrap().trim();

        let (x1, y1) = parse_point(start);
        let (x2, y2) = parse_point(stop);

        let xa = x1.min(x2);
        let xb = x1.max(x2);

        let ya = y1.min(y2);
        let yb = y1.max(y2);

        let xrange: Vec<u64> = if x1 < x2 {
            (xa..=xb).collect()
        } else {
            (xa..=xb).rev().collect()
        };

        let yrange: Vec<u64> = if y1 < y2 {
            (ya..=yb).collect()
        } else {
            (ya..=yb).rev().collect()
        };

        if (xb - xa) == (yb - ya) && diagonal {
            for (i, j) in xrange.iter().zip(yrange.iter()) {
                *counts.entry((*i, *j)).or_default() += 1;
            }
        } else if xa == xb {
            for j in yrange {
                *counts.entry((xa, j)).or_default() += 1;
            }
        } else if ya == yb {
            for i in xrange {
                *counts.entry((i, ya)).or_default() += 1;
            }
        }
    }

    if std::env::var("DEBUG").is_ok() {
        print_map(&counts);
    }
    counts.values().filter(|&&x| x > 1).count()
}

fn print_map(counts: &HashMap<(u64, u64), u64>) {
    let coords: Vec<(u64, u64)> = counts.keys().cloned().collect();
    let xmax = coords.iter().map(|(x, _)| x).max().unwrap();
    let ymax = coords.iter().map(|(_, y)| y).max().unwrap();
    println!("{} -- {}", xmax, ymax);
    for j in 0..=*ymax {
        for i in 0..=*xmax {
            match counts.get(&(i, j)) {
                None => print!("."),
                Some(n) => print!("{}", n),
            }
        }
        println!()
    }
}
