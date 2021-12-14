// Again mutability, my challenge is out of my time scope. I was close to abandon it at all because
// of hurry, but at the evening came with this terribly creepy solution

use std::io::{stdin, BufRead};

#[derive(Clone, Debug)]
enum Fold {
    X(usize),
    Y(usize),
}

#[derive(Clone, Debug)]
struct Input {
    points: Vec<(usize, usize)>,
    folds: Vec<Fold>,
}

fn input(data: impl BufRead) -> Input {
    let mut lines = data.lines();
    let mut points = vec![];

    for line in &mut lines {
        let line = line.unwrap();
        if line.is_empty() {
            break;
        }

        let mut s = line.split(',');
        let x: usize = s.next().unwrap().parse().unwrap();
        let y: usize = s.next().unwrap().parse().unwrap();

        points.push((x, y));
    }

    let folds: Vec<_> = lines
        .map(|line| line.unwrap())
        .filter(|line| !line.is_empty())
        .map(|line| {
            let mut s = line.split('=');
            let dir = s.next().unwrap().chars().last().unwrap();
            let val: usize = s.next().unwrap().parse().unwrap();
            if dir == 'x' {
                Fold::X(val)
            } else {
                Fold::Y(val)
            }
        })
        .collect();

    Input { points, folds }
}

fn fold(points: &mut [(usize, usize)], fold: Fold) {
    for (x, y) in points {
        match fold {
            Fold::X(v) if *x > v => *x = (v << 1) - *x,
            Fold::Y(v) if *y > v => *y = (v << 1) - *y,
            _ => (),
        }
    }
}

fn part1(mut data: Input) -> usize {
    fold(&mut data.points, data.folds[0].clone());
    data.points.sort_unstable();
    data.points.dedup();
    data.points.len()
}

fn part2(mut data: Input) {
    for f in data.folds {
        fold(&mut data.points, f);
    }

    let w = *data.points.iter().map(|(x, _)| x).max().unwrap() + 1;
    let h = *data.points.iter().map(|(_, y)| y).max().unwrap() + 1;

    let mut paper = vec![' '; w * h];

    for (x, y) in data.points {
        paper[x + y * w] = '#';
    }

    for line in paper.chunks(w) {
        let line: String = line.iter().copied().collect();
        println!("{}", line);
    }
}

fn main() {
    let data = input(stdin().lock());

    println!("{}", part1(data.clone()));
    part2(data);
}
