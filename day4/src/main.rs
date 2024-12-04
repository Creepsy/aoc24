fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = std::env::args().nth(1).ok_or("no input file given")?;
    let input = std::fs::read_to_string(file)?;
    let grid0: Vec<Vec<char>> = input.lines().map(|s| s.chars().collect()).collect();
    let grid1 = rotate(&grid0);
    let grid2 = rotate(&grid1);
    let grid3 = rotate(&grid2);
    let xmas_count = count_xmas(&grid0) 
        + count_xmas(&grid1)
        + count_xmas(&grid2)
        + count_xmas(&grid3);
    println!("Part 1: {}", xmas_count);
    println!("Part 2: {}", count_x_mas(&grid0));
    Ok(())
}

fn count_xmas(grid: &Vec<Vec<char>>) -> u32 {
    let mut count = 0;
    for y in 0..grid.len() - 3 {
        for x in 0..grid[0].len() {
            if grid[x][y] != 'X' {
                continue;
            }
            if grid[x][y + 1] == 'M' && grid[x][y + 2] == 'A' && grid[x][y + 3] == 'S' {
                count += 1;
            }
            if grid.len() > x + 3 && grid[x + 1][y + 1] == 'M' && grid[x + 2][y + 2] == 'A' && grid[x + 3][y + 3] == 'S' {
                count += 1;
            }
        }
    }
    count
}

fn count_x_mas(grid: &Vec<Vec<char>>) -> u32 {
    let mut count = 0;
    for y in 0..grid.len() - 2 {
        for x in 0..grid[0].len() - 2 {
            if grid[x + 1][y + 1] != 'A' {
                continue;
            }
            let diag_a = grid[x][y] == 'M' && grid[x + 2][y + 2] == 'S' || grid[x][y] == 'S' && grid[x + 2][y + 2] == 'M';
            let diag_b = grid[x][y + 2] == 'M' && grid[x + 2][y] == 'S' || grid[x][y + 2] == 'S' && grid[x + 2][y] == 'M';
            if diag_a && diag_b {
                count += 1;
            }
        }
    }
    count
}

fn rotate<T: Copy>(grid: &Vec<Vec<T>>) -> Vec<Vec<T>> {
    let mut rotated_grid = Vec::with_capacity(grid[0].len());
    for x in 0..grid[0].len() {
        let mut row = Vec::with_capacity(grid.len());
        for y in (0..grid.len()).rev() {
            row.push(grid[y][x]);
        }
        rotated_grid.push(row);
    }
    rotated_grid
}