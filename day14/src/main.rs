use std::collections::HashMap;
use std::io::{stdin, BufRead};

#[derive(Clone)]
struct Input {
    start: Vec<char>,
    substitutions: HashMap<(char, char), char>,
}

fn input(data: impl BufRead) -> Input {
    let mut lines = data
        .lines()
        .map(|line| line.unwrap())
        .filter(|line| !line.is_empty());
    let start = lines.next().unwrap().chars().collect();
    let substitutions = lines
        .map(|line| {
            let mut s = line.split("->");
            let f: Vec<_> = s.next().unwrap().trim().chars().collect();
            let fl = f[0];
            let fr = f[1];
            let t = s.next().unwrap().trim().chars().next().unwrap();
            ((fl, fr), t)
        })
        .collect();

    Input {
        start,
        substitutions,
    }
}

fn solve(data: Input, depth: usize) -> usize {
    let mut counting: HashMap<_, usize> = HashMap::new();
    let mut pairs: HashMap<_, usize> = HashMap::new();
    for p in data.start.windows(2) {
        *pairs.entry((p[0], p[1])).or_default() += 1;
    }
    for c in data.start {
        *counting.entry(c).or_default() += 1;
    }

    let mut newpairs = pairs.clone();
    for _ in 0..depth {
        for ((a, b), cnt) in pairs {
            if let Some(m) = data.substitutions.get(&(a, b)) {
                *counting.entry(*m).or_default() += cnt;
                *newpairs.entry((a, b)).or_default() -= cnt;
                *newpairs.entry((a, *m)).or_default() += cnt;
                *newpairs.entry((*m, b)).or_default() += cnt;
            }
        }
        pairs = newpairs.clone();
    }

    let i = counting.values().min().unwrap();
    let a = counting.values().max().unwrap();

    a - i
}

fn part1(data: Input) -> usize {
    solve(data, 10)
}

fn part2(data: Input) -> usize {
    solve(data, 40)
}

fn main() {
    let data = input(stdin().lock());

    println!("{}", part1(data.clone()));
    println!("{}", part2(data));
}
