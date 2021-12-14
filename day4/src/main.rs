use anyhow::{anyhow, Error};
use itertools::Itertools;
use std::convert::identity as ident;
use std::io::{stdin, BufRead};

type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Clone, Default)]
struct Board([Option<u64>; 25]);

impl Board {
    fn parse(lines: impl Iterator<Item = Result<String>>) -> Result<Self> {
        let numbers: Vec<_> = lines
            .map(|line| -> Result<Vec<Option<u64>>> {
                line?
                    .split_whitespace()
                    .map(|n| Ok(Some(n.parse::<u64>()?)))
                    .collect()
            })
            .collect::<Result<Vec<_>>>()?
            .into_iter()
            .flatten()
            .collect();

        Ok(Board(
            numbers
                .try_into()
                .map_err(|e: Vec<Option<u64>>| anyhow!("{:?}", e))?,
        ))
    }

    fn update(&mut self, num: u64) {
        if let Some(n) = self.0.iter_mut().find(|n| **n == Some(num)) {
            *n = None;
        }
    }

    fn won(&self) -> bool {
        let wins = [
            [0, 1, 2, 3, 4],
            [5, 6, 7, 8, 9],
            [10, 11, 12, 13, 14],
            [15, 16, 17, 18, 19],
            [20, 21, 22, 23, 24],
            [0, 5, 10, 15, 20],
            [1, 6, 11, 16, 21],
            [2, 7, 12, 17, 22],
            [3, 8, 13, 18, 23],
            [4, 9, 14, 19, 24],
        ];

        wins.iter()
            .map(|pat| pat.iter().map(|idx| self.0[*idx].is_none()).all(ident))
            .any(ident)
    }
}

#[derive(Debug)]
struct Input {
    numbers: Vec<u64>,
    boards: Vec<Board>,
}

fn input(data: impl BufRead) -> Result<Input> {
    let mut lines = data.lines().filter_ok(|l| !l.is_empty());

    let numbers: Vec<_> = lines
        .next()
        .ok_or_else(|| anyhow!("No numbers provided"))??
        .split(',')
        .map(|n| n.parse::<u64>().map_err(Into::into))
        .collect::<Result<_>>()?;

    let boards = lines
        .chunks(5)
        .into_iter()
        .map(|board| -> Result<Board> { Board::parse(board.map(|l| l.map_err(Into::into))) })
        .collect::<Result<_>>()?;

    Ok(Input { numbers, boards })
}

fn part1(data: &Input) -> Result<u64> {
    let (num, boards) = data
        .numbers
        .iter()
        .scan(data.boards.clone(), |boards, num| {
            for board in boards.iter_mut() {
                board.update(*num);
            }

            Some((*num, boards.clone()))
        })
        .find(|(_, boards)| !boards.iter().all(|board| !board.won()))
        .ok_or_else(|| anyhow!("No winning state!"))?;

    let Board(board) = boards.into_iter().find(|board| board.won()).unwrap();
    Ok(board.into_iter().flatten().sum::<u64>() * num)
}

fn part2(data: &Input) -> Result<u64> {
    let (num, winners) = data
        .numbers
        .iter()
        .scan((vec![], data.boards.clone()), |(winners, boards), num| {
            if boards.iter().all(|board| board.won()) {
                return None;
            }

            let mut boards: Vec<_> = boards.iter_mut().filter(|board| !board.won()).collect();

            for board in boards.iter_mut() {
                board.update(*num);
            }
            let winner = boards.iter().find(|board| board.won());
            if let Some(winner) = winner {
                winners.push((*winner).clone());
            }

            Some((*num, winners.clone()))
        })
        .last()
        .ok_or_else(|| anyhow!("No winning state"))?;

    let Board(board) = winners
        .into_iter()
        .last()
        .ok_or_else(|| anyhow!("No winner"))?;

    Ok(board.into_iter().flatten().sum::<u64>() * num)
}

fn main() -> Result<()> {
    let data = input(stdin().lock())?;

    println!("{}", part1(&data)?);
    println!("{}", part2(&data)?);

    Ok(())
}
