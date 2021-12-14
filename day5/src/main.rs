use anyhow::{anyhow, Error};
use itertools::Itertools;
use nom::bytes::complete::tag;
use nom::character::streaming::{multispace0, u64 as pu64};
use nom::sequence::tuple;
use nom::IResult;
use std::io::{stdin, BufRead};

type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Clone)]
struct Line {
    from: (usize, usize),
    to: (usize, usize),
}

struct Board {
    map: Vec<u64>,
    w: usize,
}

impl Board {
    fn new(lines: &[Line]) -> Self {
        let (w, h) = lines.iter().fold((0, 0), |(w, h), line| {
            let w = [w, line.to.0, line.from.0].into_iter().max().unwrap() as usize;
            let h = [h, line.to.1, line.from.1].into_iter().max().unwrap();
            (w, h)
        });

        Self {
            map: vec![0; (w + 1) * (h + 1)],
            w: w + 1,
        }
    }

    fn draw_aligned_line(self, line: &Line) -> Self {
        if line.from.0 == line.to.0 || line.from.1 == line.to.1 {
            self.draw_line(line)
        } else {
            self
        }
    }

    fn draw_line(mut self, line: &Line) -> Self {
        let xs = match (line.from.0, line.to.0) {
            (from, to) if from == to => vec![from; self.w],
            (from, to) if from < to => (from..=to).collect(),
            (from, to) => (to..=from).rev().collect(),
        };
        let ys = match (line.from.1, line.to.1) {
            (from, to) if from == to => vec![from; self.map.len()],
            (from, to) if from < to => (from..=to).collect(),
            (from, to) => (to..=from).rev().collect(),
        };

        for (x, y) in xs.into_iter().zip(ys) {
            self.map[x + y * self.w] += 1;
        }

        self
    }
}

fn part1(data: &[Line]) -> usize {
    data.iter()
        .fold(Board::new(data), |board, line| {
            board.draw_aligned_line(line)
        })
        .map
        .into_iter()
        .filter(|c| *c > 1)
        .count()
}

fn part2(data: &[Line]) -> usize {
    data.iter()
        .fold(Board::new(data), |board, line| board.draw_line(line))
        .map
        .into_iter()
        .filter(|c| *c > 1)
        .count()
}

fn parser(input: &str) -> IResult<&str, Line> {
    let (input, x1) = pu64(input)?;
    let (input, _) = tag(",")(input)?;
    let (input, y1) = pu64(input)?;
    let (input, _) = tuple((multispace0, tag("->"), multispace0))(input)?;
    let (input, x2) = pu64(input)?;
    let (input, _) = tag(",")(input)?;
    let (input, y2) = nom::character::complete::u64(input)?;

    Ok((
        input,
        Line {
            from: (x1 as usize, y1 as usize),
            to: (x2 as usize, y2 as usize),
        },
    ))
}

fn input(data: impl BufRead) -> Result<Vec<Line>> {
    data.lines()
        .filter_ok(|line| !line.is_empty())
        .map(|line| -> Result<Line> {
            let line = line?;
            let (_, line) = parser(&line).map_err(|err| anyhow!("{}", err))?;
            Ok(line)
        })
        .collect()
}

fn main() -> Result<()> {
    let data = input(stdin().lock())?;

    println!("{}", part1(&data));
    println!("{}", part2(&data));

    Ok(())
}
