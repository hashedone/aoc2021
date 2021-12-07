use std::io::{stdin, BufRead};

fn input(data: impl BufRead) -> Vec<i64> {
    let line = data.lines().next().unwrap().unwrap();
    let mut data: Vec<_> = line.split(',').map(|n| n.parse::<i64>().unwrap()).collect();
    data.sort();
    data
}

fn part1(data: &[i64]) -> i64 {
    let mean = data[data.len() / 2];
    data.iter().copied().map(|data| (data - mean).abs()).sum()
}

fn cost(data: &[i64], target: i64) -> i64 {
    data.iter()
        .map(|data| {
            let n = (data - target).abs();
            n * (n + 1) / 2
        })
        .sum()
}

fn part2(data: &[i64]) -> i64 {
    let low = *data.first().unwrap();
    let high = *data.last().unwrap();

    (low..=high).map(|target| cost(data, target)).min().unwrap()
}

fn main() {
    let data = input(stdin().lock());

    println!("{}", part1(&data));
    println!("{}", part2(&data));
}
