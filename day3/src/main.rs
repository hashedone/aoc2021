use anyhow::Error;
use itertools::Itertools;
use std::io::{stdin, BufRead};

type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
struct Input {
    values: Vec<char>,
    item_len: usize,
    items_cnt: usize,
}

fn input(data: impl BufRead) -> Result<Input> {
    let items: Vec<_> = data
        .lines()
        .filter_ok(|line| !line.is_empty())
        .map(|line| -> Result<Vec<char>> { Ok(line?.chars().collect()) })
        .collect::<Result<_>>()?;

    let len = items.len();
    let values: Vec<_> = items.into_iter().flatten().collect();
    let item_len = values.len() / len;

    Ok(Input {
        values,
        item_len,
        items_cnt: len,
    })
}

fn most_popular(vals: impl Iterator<Item = char>, count: usize) -> char {
    let ones = vals.filter(|s| *s == '1').count();

    if ones >= (count + 1) / 2 {
        '1'
    } else {
        '0'
    }
}

fn part1(data: &Input) -> u64 {
    let (gamma, epsilon) = (0..data.item_len).fold((0, 0), |(g, e), i| {
        let most = most_popular(
            data.values.iter().copied().skip(i).step_by(data.item_len),
            data.items_cnt,
        );
        if most == '1' {
            ((g << 1) + 1, e << 1)
        } else {
            (g << 1, (e << 1) + 1)
        }
    });

    gamma * epsilon
}

fn part2(data: &Input) -> Result<u64> {
    let idxs: Vec<_> = (0..data.items_cnt).map(|i| i * data.item_len).collect();
    let (o2, co2) = (0..data.item_len).fold((idxs.clone(), idxs), |(o2, co2), i| {
        let o2 = if o2.len() == 1 {
            o2
        } else {
            let o2_numbers = o2.iter().copied().map(|idx| data.values[idx + i]);
            let o2_most = most_popular(o2_numbers, o2.len());
            o2.iter()
                .copied()
                .filter(|idx| data.values[idx + i] == o2_most)
                .collect()
        };

        let co2 = if co2.len() == 1 {
            co2
        } else {
            let co2_numbers = co2.iter().copied().map(|idx| data.values[idx + i]);
            let co2_most = most_popular(co2_numbers, co2.len());
            co2.iter()
                .copied()
                .filter(|idx| data.values[idx + i] != co2_most)
                .collect()
        };

        (o2, co2)
    });

    let o2 = u64::from_str_radix(
        &data.values[o2[0]..o2[0] + data.item_len]
            .iter()
            .copied()
            .collect::<String>(),
        2,
    )?;

    let co2 = u64::from_str_radix(
        &data.values[co2[0]..co2[0] + data.item_len]
            .iter()
            .copied()
            .collect::<String>(),
        2,
    )?;

    Ok(o2 * co2)
}

fn main() -> Result<()> {
    let data = input(stdin().lock())?;

    println!("{}", part1(&data));
    println!("{}", part2(&data)?);

    Ok(())
}
