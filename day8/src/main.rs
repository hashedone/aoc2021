use itertools::Itertools;
use std::io::{stdin, BufRead};

#[derive(Clone)]
struct Input {
    diagnostics: Vec<String>,
    outputs: Vec<String>,
}

struct EncodedInput {
    /// Single diagnostics entry. The final outputs are also added here for theoretical
    /// case where they actually provide additional intel.
    ///
    /// Every entry is bitflag describing which letters occured in the entry. The mapping is
    ///
    ///  _gfedcba
    ///
    ///  Note that the most significant bit is unused.
    diagnostics: Vec<u8>,
    /// The outputs to be determined. Exactly the same bitflags as `diagnostics`
    outputs: Vec<u8>,
}

impl EncodedInput {
    fn encode(s: String) -> u8 {
        s.chars()
            .map(|c| match c {
                'a' => 1,
                'b' => 2,
                'c' => 4,
                'd' => 8,
                'e' => 16,
                'f' => 32,
                'g' => 64,
                _ => 0,
            })
            .fold(0, |acc, v| acc | v)
    }
}

impl From<Input> for EncodedInput {
    fn from(src: Input) -> Self {
        let diagnostics: Vec<_> = src.diagnostics.into_iter().map(Self::encode).collect();
        let outputs: Vec<_> = src.outputs.into_iter().map(Self::encode).collect();
        let diagnostics = [diagnostics, outputs.clone()].concat();

        Self {
            diagnostics,
            outputs,
        }
    }
}

fn input(data: impl BufRead) -> Vec<Input> {
    data.lines()
        .map(|line| line.unwrap())
        .filter(|line| !line.is_empty())
        .map(|line| {
            let mut line = line.split('|');
            let diagnostics = line
                .next()
                .unwrap()
                .split_whitespace()
                .map(str::to_owned)
                .collect();
            let outputs = line
                .next()
                .unwrap()
                .split_whitespace()
                .map(str::to_owned)
                .collect();

            Input {
                diagnostics,
                outputs,
            }
        })
        .collect()
}

fn part1(data: &[Input]) -> usize {
    data.iter()
        .flat_map(|input| &input.outputs)
        .filter(|o| [2, 3, 4, 7].contains(&o.len()))
        .count()
}

const A: u8 = 1;
const B: u8 = 2;
const C: u8 = 4;
const D: u8 = 8;
const E: u8 = 16;
const F: u8 = 32;
const G: u8 = 64;

// numbers representation patterns
const NUMBERS: [u8; 10] = [
    A | B | C | E | F | G,     // 0
    C | F,                     // 1
    A | C | D | E | G,         // 2
    A | C | D | F | G,         // 3
    B | C | D | F,             // 4
    A | B | D | F | G,         // 5
    A | B | D | E | F | G,     // 6
    A | C | F,                 // 7
    A | B | C | D | E | F | G, // 8
    A | B | C | D | F | G,     // 9
];

fn map_number(mapping: &[u8], item: u8) -> Option<usize> {
    let num = (0..7)
        .filter(|seg| ((1 << seg) & item) > 0)
        .map(|seg| mapping[seg])
        .fold(0, |acc, seg| acc | seg);

    NUMBERS.iter().position(|n| *n == num)
}

fn part2(data: Vec<Input>) -> usize {
    data.into_iter()
        .map(|input| {
            let input = EncodedInput::from(input);
            let mapping = [A, B, C, D, E, F, G]
                .into_iter()
                .permutations(7)
                .find(move |mapping| {
                    input
                        .diagnostics
                        .iter()
                        .all(|entry| map_number(mapping, *entry).is_some())
                })
                .unwrap();

            input
                .outputs
                .into_iter()
                .fold(0, |acc, out| acc * 10 + map_number(&mapping, out).unwrap())
        })
        .sum()
}

fn main() {
    let data = input(stdin().lock());

    println!("{}", part1(&data));
    println!("{}", part2(data));
}
