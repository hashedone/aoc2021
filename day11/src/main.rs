use std::io::{stdin, BufRead};

fn input(data: impl BufRead) -> [u8; 100] {
    data.lines()
        .map(|l| l.unwrap())
        .filter(|l| !l.is_empty())
        .map(|l| {
            l.chars()
                .map(|c| c.to_digit(10).unwrap() as u8)
                .collect::<Vec<_>>()
        })
        .flatten()
        .collect::<Vec<_>>()
        .try_into()
        .unwrap()
}

fn coords(idx: usize) -> (usize, usize) {
    (idx % 10, idx / 10)
}

fn index(x: usize, y: usize) -> usize {
    x + y * 10
}

fn neighbors(idx: usize) -> Vec<usize> {
    let (x, y) = coords(idx);
    [
        Some((x.wrapping_sub(1), y.wrapping_sub(1))).filter(|_| x > 0 && y > 0),
        Some((x.wrapping_sub(1), y)).filter(|_| x > 0),
        Some((x.wrapping_sub(1), y + 1)).filter(|_| x > 0 && y < 9),
        Some((x, y.wrapping_sub(1))).filter(|_| y > 0),
        Some((x, y + 1)).filter(|_| y < 9),
        Some((x + 1, y.wrapping_sub(1))).filter(|_| x < 9 && y > 0),
        Some((x + 1, y)).filter(|_| x < 9),
        Some((x + 1, y + 1)).filter(|_| x < 9 && y < 9),
    ]
    .into_iter()
    .flatten()
    .map(|(x, y)| index(x, y))
    .collect()
}

fn step(mut data: [u8; 100]) -> ([u8; 100], usize) {
    for cell in &mut data {
        *cell += 1;
    }

    while data.iter().any(|cell| *cell == 10) {
        for idx in 0..100 {
            if data[idx] == 10 {
                data[idx] = 11;
                for nb in neighbors(idx) {
                    if data[nb] < 10 {
                        data[nb] += 1
                    }
                }
            }
        }
    }

    let flashes = data.iter().filter(|cell| **cell == 11).count();

    for cell in &mut data {
        *cell %= 11;
    }

    (data, flashes)
}

fn part1(mut data: [u8; 100]) -> usize {
    std::iter::from_fn(move || {
        let (cells, flashes) = step(data);
        data = cells;
        Some(flashes)
    })
    .take(100)
    .sum()
}

fn part2(mut data: [u8; 100]) -> usize {
    std::iter::from_fn(move || {
        let (cells, flashes) = step(data);
        data = cells;
        Some(flashes)
    })
    .position(|flashes| flashes == 100)
    .unwrap()
        + 1
}

fn main() {
    let data = input(stdin().lock());

    println!("{}", part1(data));
    println!("{}", part2(data));
}
