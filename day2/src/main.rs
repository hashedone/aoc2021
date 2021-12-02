use anyhow::{anyhow, bail, Error};
use itertools::Itertools;
use std::io::{stdin, BufRead};

type Result<T> = std::result::Result<T, Error>;

enum Input {
    Forward(u32),
    Down(u32),
    Up(u32),
}

impl Input {
    fn parse(data: &str) -> Result<Input> {
        let mut vals = data.split_whitespace();
        let dir = vals.next().ok_or_else(|| anyhow!("Missing direction"))?;
        let val = vals
            .next()
            .ok_or_else(|| anyhow!("Missing value"))?
            .parse()?;

        let res = match dir {
            "forward" => Self::Forward(val),
            "down" => Self::Down(val),
            "up" => Self::Up(val),
            _ => bail!("Invalid input"),
        };

        Ok(res)
    }

    fn dir(&self) -> (i64, i64) {
        match *self {
            Self::Forward(d) => (d as _, 0),
            Self::Down(d) => (0, d as _),
            Self::Up(d) => (0, -(d as i64)),
        }
    }
}

fn input(data: impl BufRead) -> Result<Vec<Input>> {
    data.lines()
        .filter_ok(|line| !line.is_empty())
        .map(|line| -> Result<_> { Input::parse(&line?).map_err(Into::into) })
        .collect()
}

fn calculate_position(
    data: impl IntoIterator<Item = impl std::borrow::Borrow<Input>>,
) -> (i64, i64) {
    data.into_iter().fold((0, 0), |(x, z), val| {
        let (dx, dz) = val.borrow().dir();
        (x + dx, z + dz)
    })
}

fn calculate_aligned_position(
    data: impl IntoIterator<Item = impl std::borrow::Borrow<Input>>,
) -> (i64, i64) {
    let (x, _, z) = data.into_iter().fold((0, 0, 0), |(x, aim, z), val| {
        let (df, daim) = val.borrow().dir();
        (x + df, aim + daim, z + df * aim)
    });
    (x, z)
}

fn part1(data: &[Input]) -> i128 {
    let (x, z) = calculate_position(data.iter());
    x as i128 * z as i128
}

fn part2(data: &[Input]) -> i128 {
    let (x, z) = calculate_aligned_position(data.iter());
    x as i128 * z as i128
}

fn main() -> Result<()> {
    let data = input(stdin().lock())?;

    println!("{}", part1(&data));
    println!("{}", part2(&data));

    Ok(())
}
