use anyhow::Error;
use itertools::Itertools;
use std::io::{stdin, BufRead};

type Result<T> = std::result::Result<T, Error>;

fn input(data: impl BufRead) -> Result<Vec<u64>> {
    data.lines()
        .filter_ok(|line| !line.is_empty())
        .map(|line| -> Result<u64> { line?.parse().map_err(Into::into) })
        .collect()
}

fn increasing_len(data: impl IntoIterator<Item = u64>) -> u64 {
    data.into_iter()
        .tuple_windows()
        .filter(|(prev, next)| next > prev)
        .count() as _
}

fn accumulate_measurements(data: impl IntoIterator<Item = u64>) -> impl Iterator<Item = u64> {
    data.into_iter().tuple_windows().map(|(a, b, c)| a + b + c)
}

fn part1(data: &[u64]) -> u64 {
    increasing_len(data.iter().copied())
}

fn part2(data: &[u64]) -> u64 {
    increasing_len(accumulate_measurements(data.iter().copied()))
}

fn main() -> Result<()> {
    let data = input(stdin().lock())?;

    println!("{}", part1(&data));
    println!("{}", part2(&data));

    Ok(())
}
