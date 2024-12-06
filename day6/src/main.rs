use std::{collections::HashSet, ops::Add};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum State {
    Blank,
    Blocked,
    Visited
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
enum Direction {
    Up,
    Right,
    Down,
    Left
}

type V2<T> = (T, T);

type Guard = (V2<usize>, Direction);


impl Direction {
    fn offset(&self) -> V2<i32> {
        match &self {
            Self::Up => (-1, 0),
            Self::Right => (0, 1),
            Self::Down => (1, 0),
            Self::Left => (0, -1),
        }
    }

    fn rotate(&self) -> Direction {
        match &self {
            Self::Up => Self::Right,
            Self::Right => Self::Down,
            Self::Down => Self::Left,
            Self::Left => Self::Up
        }
    }
}


fn main() {
    let file = std::env::args().nth(1).expect("No input file given");
    let input = std::fs::read_to_string(file).unwrap();
    let (mut grid, guard) = parse(&input).expect("Invalid input format");
    println!("Part 1: {}", part1(&mut grid, guard));
    println!("Part 2: {}", part2(grid, guard));
}

fn parse(input: &str) -> Result<(Vec<Vec<State>>, Guard), &str> {
    let lines: Vec<&str> = input.lines().collect();
    let mut guard = Err("No start position given");
    let mut grid = Vec::with_capacity(lines.len());
    for y in 0..lines.len() {
        let mut row = Vec::with_capacity(lines[y].len());
        for (x, c) in lines[y].chars().enumerate() {
            row.push(
                match c {
                    '#' => Ok(State::Blocked),
                    '.'|'>'|'^'|'<'|'v' => Ok(State::Blank),
                    _ => Err("Invalid map symbol")
                }?
            );
            guard = match c {
                '^' => Ok(((y, x), Direction::Up)),
                '>' => Ok(((y, x), Direction::Right)),
                'v' => Ok(((y, x), Direction::Down)),
                '<' => Ok(((y, x), Direction::Left)),
                _ => guard
            }
        }
        grid.push(row);
    }
    Ok((grid, guard?))
}

fn part1(grid: &mut Vec<Vec<State>>, mut guard: Guard) -> usize {
    loop {
        let guard_new = step(grid, guard);
        if guard_new.is_none() {
            break;
        }
        guard = guard_new.unwrap();
    }
    grid.iter().flatten().filter(|x| **x == State::Visited).count()
}

fn part2(grid: Vec<Vec<State>>, guard@(init_pos, _): Guard) -> usize {
    let mut placements = 0;
    for y in 0..grid.len() {
        for x in 0..grid[y].len() {
            if grid[y][x] == State::Visited && init_pos != (y, x) {
                let mut grid_s = grid.clone();
                grid_s[y][x] = State::Blocked;
                placements += loops(grid_s, guard) as usize;
            }
        }
    }
    placements
}

fn loops(mut grid: Vec<Vec<State>>, mut guard: Guard) -> bool {
    let mut prev_guard_states: HashSet<((usize, usize), Direction)> = HashSet::new();
    loop {
        let guard_new = step(&mut grid, guard);
        if guard_new.is_none() {
            return false;
        }
        prev_guard_states.insert(guard);
        guard = guard_new.unwrap();
        if prev_guard_states.contains(&guard) {
            return true;
        }

    }
}

fn step(grid: &mut Vec<Vec<State>>, ((y, x), direction): Guard) -> Option<Guard> {
    grid[y][x] = State::Visited;
    let mut next_direction = direction;
    loop {
        let (y_n, x_n) = v2_add((y as i32, x as i32), next_direction.offset());
        if x_n < 0 || y_n < 0 || y_n as usize >= grid.len() || x_n as usize >= grid[y_n as usize].len() {
            return None;
        }
        if grid[y_n as usize][x_n as usize] != State::Blocked {
            return Some(((y_n as usize, x_n as usize), next_direction));
        }
        next_direction = next_direction.rotate();
    }
}

fn v2_add<T: Add<Output = T>>((a1, a2): V2<T>, (b1, b2): V2<T>) -> V2<T> {
    (a1 + b1, a2 + b2)
}