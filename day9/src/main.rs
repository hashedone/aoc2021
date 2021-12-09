use std::io::{stdin, BufRead};

struct Heightmap {
    map: Vec<u8>,
    width: usize,
}

impl Heightmap {
    fn neighbors(&self, idx: usize) -> Vec<usize> {
        [
            Some(idx.wrapping_sub(self.width)).filter(|_| idx >= self.width),
            Some(idx + self.width).filter(|_| idx < self.map.len() - self.width - 1),
            Some(idx.wrapping_sub(1)).filter(|_| idx % self.width > 0),
            Some(idx + 1).filter(|_| (idx + 1) % self.width > 0),
        ]
        .into_iter()
        .flatten()
        .collect()
    }

    fn is_lowpoint(&self, idx: usize) -> bool {
        self.neighbors(idx)
            .into_iter()
            .all(|nb| self.map[nb] > self.map[idx])
    }

    fn safety(&self, idx: usize) -> i64 {
        (self.map[idx] + 1) as i64
    }

    fn indices(&self) -> impl Iterator<Item = usize> {
        0..self.map.len()
    }
}

fn input(data: impl BufRead) -> Heightmap {
    let map: Vec<Vec<_>> = data
        .lines()
        .map(|l| l.unwrap())
        .filter(|l| !l.is_empty())
        .map(|l| l.chars().map(|d| d.to_digit(10).unwrap() as u8).collect())
        .collect();
    let len = map.len();
    let map: Vec<_> = map.into_iter().flatten().collect();
    let width = map.len() / len;

    Heightmap { map, width }
}

fn part1(data: &Heightmap) -> i64 {
    data.indices()
        .filter(|idx| data.is_lowpoint(*idx))
        .map(|idx| data.safety(idx))
        .sum()
}

// Yea, supposed to avoid mutations but flood fill is really the easiest approach... I am truly
// sorry :( I could technically scan over indicies and make data a state, but I would still not
// get rid with mutation on sorting (unless I use itertools). This is at least simple.
fn part2(mut data: Heightmap) -> i64 {
    let mut basins: Vec<_> = data
        .indices()
        .filter_map(|idx| {
            if !data.is_lowpoint(idx) || data.map[idx] == 9 {
                None
            } else {
                let mut flood = vec![idx];
                let mut size = 0;
                while let Some(idx) = flood.pop() {
                    if data.map[idx] != 9 {
                        size += 1;
                        flood.extend(data.neighbors(idx));
                        data.map[idx] = 9;
                    }
                }

                Some(size)
            }
        })
        .collect();

    basins.sort();
    basins.into_iter().rev().take(3).product()
}

fn main() {
    let data = input(stdin().lock());

    println!("{}", part1(&data));
    println!("{}", part2(data));
}
