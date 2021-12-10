use std::io::{stdin, BufRead};

fn input(data: impl BufRead) -> Vec<String> {
    data.lines()
        .map(|line| line.unwrap())
        .filter(|line| !line.is_empty())
        .collect()
}

fn score_error(c: char) -> i64 {
    match c {
        ')' => 3,
        ']' => 57,
        '}' => 1197,
        '>' => 25137,
        _ => 0,
    }
}

fn score_completion(c: &str) -> i64 {
    c.chars().fold(0, |score, c| match c {
        ')' => score * 5 + 1,
        ']' => score * 5 + 2,
        '}' => score * 5 + 3,
        '>' => score * 5 + 4,
        _ => score * 5,
    })
}

fn opening(c: char) -> char {
    match c {
        ')' => '(',
        ']' => '[',
        '}' => '{',
        '>' => '<',
        _ => c,
    }
}

fn closing(c: char) -> char {
    match c {
        '(' => ')',
        '[' => ']',
        '{' => '}',
        '<' => '>',
        _ => c,
    }
}

fn process(data: &str) -> Result<Vec<char>, char> {
    data.chars().fold(Ok(vec![]), |stack, c| {
        let mut stack = stack?;
        match c {
            '(' | '[' | '{' | '<' => {
                stack.push(c);
                Ok(stack)
            }
            ')' | ']' | '}' | '>' if stack.last() == Some(&opening(c)) => {
                stack.pop();
                Ok(stack)
            }
            _ => Err(c),
        }
    })
}

fn part1(data: &[String]) -> i64 {
    data.iter()
        .map(|s| score_error(process(&s).err().unwrap_or_default()))
        .sum()
}

fn part2(data: &[String]) -> i64 {
    let mut scores: Vec<_> = data
        .iter()
        .filter_map(|s| process(&s).ok())
        .map(|stack| score_completion(&stack.into_iter().rev().map(closing).collect::<String>()))
        .collect();
    let idx = scores.len() / 2;
    let (_, res, _) = scores.select_nth_unstable(idx);
    *res
}

fn main() {
    let data = input(stdin().lock());

    println!("{}", part1(&data));
    println!("{}", part2(&data));
}
