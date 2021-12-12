use std::collections::HashMap;
use std::io::{stdin, BufRead};

#[derive(Debug, Clone)]
struct Graph {
    /// Marks how much time given cave was visited. `None` for big caves as those has unlimited
    /// visits
    caves: Vec<Option<u8>>,
    /// n x n flattened array, the adjacency map
    connections: Vec<bool>,
}

fn input(data: impl BufRead) -> Graph {
    let mut mapping: HashMap<_, _> = [("start".to_owned(), 0), ("end".to_owned(), 1)]
        .into_iter()
        .collect();
    // Start cave is visited twice at the beginning as it should never be considered as a
    // destination
    let mut caves = vec![Some(2), None];

    let paths: Vec<_> = data
        .lines()
        .map(|line| line.unwrap())
        .filter(|line| !line.is_empty())
        .map(|line| {
            let mut line = line.split('-');
            let f = line.next().unwrap();
            let from = f.to_owned();
            let from = *mapping.entry(from).or_insert_with(|| {
                if f.chars().next().unwrap().is_uppercase() {
                    caves.push(None)
                } else {
                    caves.push(Some(0))
                }

                caves.len() - 1
            });
            let t = line.next().unwrap();
            let to = t.to_owned();
            let to = *mapping.entry(to).or_insert_with(|| {
                if t.chars().next().unwrap().is_uppercase() {
                    caves.push(None)
                } else {
                    caves.push(Some(0))
                }

                caves.len() - 1
            });

            (from, to)
        })
        .collect();

    let mut connections = vec![false; caves.len() * caves.len()];
    for (from, to) in paths {
        connections[from * caves.len() + to] = true;
        connections[from + to * caves.len()] = true;
    }

    Graph { caves, connections }
}

fn part1(data: &Graph) -> usize {
    let mut caves = data.caves.clone();
    let mut stack = vec![0];

    std::iter::from_fn(move || {
        let mut start = 1;

        while let Some(top) = stack.last().copied() {
            // If last reached is end, just backtrack
            if top == 1 {
                start = 2;
                stack.pop();
                continue;
            }

            for i in start..caves.len() {
                if !matches!(caves[i], Some(n) if n > 0) && data.connections[top * caves.len() + i]
                {
                    stack.push(i);
                    if let Some(visits) = &mut caves[i] {
                        *visits += 1;
                    }

                    // True if reached end, else otherwise
                    return Some(i == 1);
                }
            }

            // No more connections found from last node - backtrack
            start = top + 1;
            stack.pop();

            if let Some(visits) = &mut caves[top] {
                *visits -= 1;
            }
        }

        // No more valid paths found, end of options
        None
    })
    .filter(Clone::clone)
    .count()
}

fn part2(data: &Graph) -> usize {
    let mut caves = data.caves.clone();
    let mut stack = vec![0];
    let mut smalls = 0;

    std::iter::from_fn(move || {
        let mut start = 1;

        while let Some(top) = stack.last().copied() {
            // If last reached is end, just backtrack
            if top == 1 {
                start = 2;
                stack.pop();
                continue;
            }

            for i in start..caves.len() {
                if !matches!(caves[i], Some(n) if n > (1 - smalls))
                    && data.connections[top * caves.len() + i]
                {
                    stack.push(i);
                    if let Some(visits) = &mut caves[i] {
                        *visits += 1;
                        if *visits == 2 {
                            smalls += 1;
                        }
                    }

                    // True if reached end, else otherwise
                    return Some(i == 1);
                }
            }

            // No more connections found from last node - backtrack
            start = top + 1;
            stack.pop();

            if let Some(visits) = &mut caves[top] {
                *visits -= 1;
                if stack.contains(&top) {
                    smalls -= 1;
                }
            }
        }

        // No more valid paths found, end of options
        None
    })
    .filter(Clone::clone)
    .count()
}

fn main() {
    let data = input(stdin().lock());
    println!("{}", part1(&data));
    println!("{}", part2(&data));
}
