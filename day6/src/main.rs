use std::io::{stdin, BufRead};

#[derive(Debug, Clone)]
struct Fishes {
    counts: [u128; 7],
    growing: [u128; 2],
    offset: usize,
}

impl Fishes {
    fn simulate(mut self) -> Self {
        let spawned = self.counts[self.offset];
        self.offset = (self.offset + 1) % 7;
        self.counts[(self.offset + 6) % 7] += self.growing[1];
        self.growing[1] = self.growing[0];
        self.growing[0] = spawned;
        self
    }

    fn count(&self) -> u128 {
        self.counts.iter().copied().sum::<u128>() + self.growing.iter().sum::<u128>()
    }
}

fn input(data: impl BufRead) -> Fishes {
    let line = data.lines().next().unwrap().unwrap();
    line.split(',').map(|n| n.parse::<usize>().unwrap()).fold(
        Fishes {
            counts: [0; 7],
            growing: [0; 2],
            offset: 0,
        },
        |mut f, c| {
            f.counts[c] += 1;
            f
        },
    )
}

fn part1(data: Fishes) -> u128 {
    (0..80).fold(data, |fishes, _| fishes.simulate()).count()
}

fn part2(data: Fishes) -> u128 {
    (0..256).fold(data, |fishes, _| fishes.simulate()).count()
}

fn main() {
    let data = input(stdin().lock());

    println!("{}", part1(data.clone()));
    println!("{}", part2(data));
}
