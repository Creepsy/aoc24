use core::panic;

type Equation = (u64, Vec<u64>); 

fn main() {
    let file = std::env::args().nth(1).expect("No input file given.");
    let input = std::fs::read_to_string(file).unwrap();
    let equations = parse(&input).expect("Invalid input format.");
    println!("Part 1: {}", part1(&equations));
    println!("Part 2: {}", part2(&equations));
}

fn parse(input: &str) -> Option<Vec<Equation>> {
    input.lines().map(|line| {
        let (result, operands) = line.split_once(':')?;
        Some((
            result.parse().ok()?, 
            operands.split_ascii_whitespace().map(|n| n.parse::<u64>().ok()).collect::<Option<Vec<u64>>>()?
        ))
    }).collect()
}

fn part1(equations: &Vec<Equation>) -> u64 {
    equations.iter().filter(|eq| satisfy1(*eq)).map(|t| t.0).sum()
}

fn part2(equations: &Vec<Equation>) -> u64 {
    equations.iter().filter(|eq| satisfy2(*eq)).map(|t| t.0).sum()
}

fn satisfy1((result, operands): &Equation) -> bool {
    match operands.len() {
        0 => *result == 0,
        1 => *result == operands[0],
        _ => {
            let (init, [last]) = operands.split_at(operands.len() - 1) else {
                panic!("This should never happen");
            };
            let div = result % last == 0 && satisfy1(&(result / last, init.to_vec()));
            let sub = result >= last && satisfy1(&(result - last, init.to_vec()));
            div || sub
        }
    }
}

fn satisfy2((result, operands): &Equation) -> bool {
    match operands.len() {
        0 => *result == 0,
        1 => *result == operands[0],
        _ => {
            let (init, [last]) = operands.split_at(operands.len() - 1) else {
                panic!("This should never happen");
            };
            let div = result % last == 0 && satisfy2(&(result / last, init.to_vec()));
            let sub = result >= last && satisfy2(&(result - last, init.to_vec()));
            let res_str = result.to_string();
            let trimmed = res_str.strip_suffix(&last.to_string());
            let concat = match trimmed {
                None | Some("") => false,
                Some(trimmed) => satisfy2(&(trimmed.parse().unwrap(), init.to_vec())) // unwrap is safe because the string is not empty
            };           
            div || sub || concat
        }
    }
}