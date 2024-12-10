use std::collections::HashSet;

type Antenna = (i32, i32, char);
type Antinode = (i32, i32);
type Bounds = (usize, usize);

fn main() {
    let file = std::env::args().nth(1).expect("No input file given.");
    let input = std::fs::read_to_string(file).unwrap();
    let (antennas, bounds) = parse(&input);
    println!("Part 1: {}", part1(bounds, &antennas));
    println!("Part 2: {}", part2(bounds, &antennas));
}

fn parse(input: &str) -> (Vec<Antenna>, Bounds) {
    let lines: Vec<&str> = input.lines().collect();
    let mut antennas = Vec::new();
    for y in 0..lines.len() {
        for (x, c) in lines[y].chars().enumerate() {
            if c != '.' {
                antennas.push((x as i32, y as i32, c));
            }
        }
    }
    (antennas, (lines[0].len(), lines.len()))
}

fn part1(bounds: Bounds, antennas: &Vec<Antenna>) -> usize {
    calculate_antinodes1(antennas).into_iter().filter(|node| in_bounds(bounds, *node)).count()
}

fn part2(bounds: Bounds, antennas: &Vec<Antenna>) -> usize {
    calculate_antinodes2(bounds, antennas).into_iter().filter(|node| in_bounds(bounds, *node)).count()
}

fn in_bounds((bx, by): Bounds, (x, y): Antinode) -> bool {
    0 <= x && x < bx as i32 && 0 <= y && y < by as i32
}

fn calculate_antinodes1(antennas: &Vec<Antenna>) -> Vec<Antinode> {
    let mut antinodes = HashSet::new();
    for first in 0..antennas.len() {
        for second in first + 1..antennas.len() {
            let (x_f, y_f, freq_f) = antennas[first];
            let (x_s, y_s, freq_s) = antennas[second];
            if freq_f != freq_s {
                continue;
            }
            let (d_x, d_y) = (x_s - x_f, y_s - y_f);
            antinodes.insert((x_s + d_x, y_s + d_y));
            antinodes.insert((x_f - d_x, y_f - d_y));
        }
    }
    antinodes.into_iter().collect()
}

fn calculate_antinodes2(bounds: Bounds, antennas: &Vec<Antenna>) -> Vec<Antinode> {
    let mut antinodes = HashSet::new();
    for first in 0..antennas.len() {
        for second in first + 1..antennas.len() {
            let (x_f, y_f, freq_f) = antennas[first];
            let (x_s, y_s, freq_s) = antennas[second];
            if freq_f != freq_s {
                continue;
            }
            let (d_x, d_y) = (x_s - x_f, y_s - y_f);
            antinodes.extend(propagate(bounds, (d_x, d_y), (x_f, y_f)));
            antinodes.extend(propagate(bounds, (-d_x, -d_y), (x_f, y_f)));
        }
    }
    antinodes.into_iter().collect()
}

fn propagate(bounds: Bounds, (d_x, d_y): (i32, i32), (start_x, start_y): (i32, i32)) -> Vec<(i32, i32)> {
    let mut positions = Vec::new();
    let (mut x, mut y) = (start_x, start_y);
    loop {
        if !in_bounds(bounds, (x, y)) {
            break;
        }
        positions.push((x, y));
        x += d_x;
        y += d_y;
    }
    positions
}