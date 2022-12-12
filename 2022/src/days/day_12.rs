use std::{
    collections::{BinaryHeap, HashSet},
    io::BufRead,
};
type Point = (i32, i32);
type Dir = (i32, i32);

const DIRS: &[Dir] = &[(1, 0), (0, 1), (-1, 0), (0, -1)];

fn add(p: Point, dir: &Dir) -> Point {
    (p.0 + dir.0, p.1 + dir.1)
}

fn dist_f(p: Point, e: Point) -> usize {
    (p.0 - e.0).abs() as usize + (p.1 - e.1).abs() as usize
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

#[derive(Ord, PartialEq, Eq, PartialOrd)]
struct Opt {
    dist: i32,
    len: usize,
    target: Point,
}

fn astar(
    Opt {
        target,
        len,
        dist: _dist,
    }: Opt,
    end: Point,
    grid: &[Vec<u8>],
    size: (i32, i32),
    mut add_f: impl FnMut(Opt),
) -> Option<usize> {
    if target == end {
        return Some(len);
    }

    for dir in DIRS {
        let next = add(target, dir);
        if in_bounds(next, size)
            && walkable(
                grid[target.0 as usize][target.1 as usize],
                grid[next.0 as usize][next.1 as usize],
            )
        {
            let opt = Opt {
                target: next,
                len: len + 1,
                dist: -((len + 1) as i32),
            };
            add_f(opt);
        }
    }

    None
}

fn astar_driver(start: Point, end: Point, grid: &[Vec<u8>], size: (i32, i32)) -> Option<usize> {
    let mut queue: BinaryHeap<Opt> = BinaryHeap::new();
    let mut done = HashSet::new();

    let start = Opt {
        target: start,
        len: 0,
        dist: 0,
    };

    queue.push(start);

    loop {
        let current = queue.pop()?;
        if !done.insert(current.target) {
            continue;
        };

        // Please optimize this away
        let f = |opt: Opt| {
            if !done.contains(&opt.target) {
                queue.push(opt);
            }
        };

        if let Some(x) = astar(current, end, grid, size, f) {
            return Some(x);
        }
    }
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
        let p1 = astar_driver(start, end, &grid, size);
        println!("Part 1: {}", p1.unwrap());
    }

    if P2 {
        let mut min = usize::MAX;
        for (i, row) in grid.iter().enumerate() {
            for (j, col) in row.iter().enumerate() {
                if *col == b'a' {
                    let p1 =
                        astar_driver((i as i32, j as i32), end, &grid, size).unwrap_or(usize::MAX);
                    if p1 < min {
                        min = p1;
                    }
                }
            }
        }

        println!("Part 2: {}", min);
    }
    Some(())
}
