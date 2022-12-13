use std::{
    collections::{HashSet, VecDeque},
    io::BufRead,
};

type Point = (i32, i32);
type Dir = (i32, i32);

const DIRS: &[Dir] = &[(1, 0), (0, 1), (-1, 0), (0, -1)];

fn add(p: Point, dir: &Dir) -> Point {
    (p.0 + dir.0, p.1 + dir.1)
}

fn in_bounds(p: Point, size: (i32, i32)) -> bool {
    p.0 >= 0 && p.1 >= 0 && p.0 < size.0 && p.1 < size.1
}

fn walkable(from: u8, to: u8) -> bool {
    if from >= to {
        true
    } else {
        to - from == 1
    }
}

fn bfs(
    start: Point,
    is_end: impl Fn(Point) -> bool,
    grid: &[Vec<u8>],
    size: (i32, i32),
) -> Option<usize> {
    let mut done = HashSet::<Point>::new();
    let mut queue = VecDeque::new();
    queue.push_back((start, 0));

    while let Some((target, len)) = queue.pop_front() {
        if is_end(target) {
            return Some(len);
        }
        if !done.insert(target) {
            continue;
        }

        for dir in DIRS {
            let next = add(target, dir);
            if in_bounds(next, size)
                && walkable(
                    grid[next.0 as usize][next.1 as usize],
                    grid[target.0 as usize][target.1 as usize],
                )
            {
                queue.push_back((next, len + 1));
            }
        }
    }

    None
}

pub fn solve<const P1: bool, const P2: bool>(buf: impl BufRead) -> Option<()> {
    let mut grid: Vec<Vec<u8>> = buf.split(b'\n').flatten().collect();
    let mut start: Option<Point> = None;
    let mut end: Option<Point> = None;

    for (i, row) in grid.iter_mut().enumerate() {
        for (j, col) in row.iter_mut().enumerate() {
            if *col == b'S' {
                start = Some((i as i32, j as i32));
                *col = b'a';
            }

            if *col == b'E' {
                end = Some((i as i32, j as i32));
                *col = b'z';
            }
        }
    }

    let (start, end) = (start?, end?);
    let size = (grid.len() as i32, grid[0].len() as i32);

    if P1 {
        let p1 = bfs(end, |x| x == start, &grid, size);
        println!("Part 1: {}", p1.unwrap());
    }

    if P2 {
        let p1 = bfs(
            end,
            |x| grid[x.0 as usize][x.1 as usize] == b'a',
            &grid,
            size,
        )
        .unwrap();

        println!("Part 2: {}", p1);
    }
    Some(())
}
