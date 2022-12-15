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
    from >= to || (to - from == 1)
}

fn bfs(
    start: Point,
    start_char: u8,
    is_end: impl Fn(u8, Point) -> bool,
    grid: &[Vec<u8>],
    size: (i32, i32),
) -> Option<usize> {
    let mut done = HashSet::<Point>::new();
    let mut queue = VecDeque::new();
    queue.push_back((start, start_char, 0));

    while let Some((target, t_ch, len)) = queue.pop_front() {
        if is_end(t_ch, target) {
            return Some(len);
        }

        if !done.insert(target) {
            continue;
        }

        for dir in DIRS {
            let next = add(target, dir);
            if in_bounds(next, size) {
                let n_ch = grid[next.0 as usize][next.1 as usize];
                if walkable(n_ch, t_ch) {
                    queue.push_back((next, n_ch, len + 1));
                }
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
                // *col = b'a';
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
        let p1 = bfs(end, b'z', |_, x| x == start, &grid, size);
        println!("Part 1: {}", p1.unwrap());
    }

    if P2 {
        let p1 = bfs(end, b'z', |x, _| x == b'a', &grid, size).unwrap();
        println!("Part 2: {}", p1);
    }
    Some(())
}
