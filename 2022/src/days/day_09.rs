use std::{collections::HashSet, io::BufRead};

use crate::parser::{parse, Char};
use derive::Parse;

type Point = (isize, isize);

#[derive(Parse, Debug, Clone, Copy)]
enum Dir {
    U,
    L,
    R,
    D,
}

impl Dir {
    fn dx(self) -> Point {
        match self {
            Dir::U => (0, 1),
            Dir::L => (-1, 0),
            Dir::R => (1, 0),
            Dir::D => (0, -1),
        }
    }
}

#[derive(Debug)]
struct Snake<const L: usize> {
    body: [Point; L],
    loc: HashSet<Point>,
}

impl<const L: usize> Snake<L> {
    fn new() -> Self {
        let mut loc = HashSet::new();
        loc.insert((0, 0));

        Self {
            body: [Point::default(); L],
            loc,
        }
    }

    fn mov(&mut self, dir: Dir, n: usize) {
        let (dx, dy) = dir.dx();
        for _ in 0..n {
            let head = self.body.first_mut().unwrap();

            *head = (head.0 + dx, head.1 + dy);

            for i in 1..L {
                let head = *self.body.get(i - 1).unwrap();
                let tail = self.body.get_mut(i).unwrap();

                let dx = head.0 - tail.0;
                let dy = head.1 - tail.1;

                // I should move
                if dx.abs() > 1 || dy.abs() > 1 {
                    *tail = (tail.0 + dx.signum(), tail.1 + dy.signum());

                    // I'm the actual tail
                    if i == L - 1 {
                        self.loc.insert(*tail);
                    }
                }
            }
        }
    }
}

type Line = (Dir, Char<b' '>, usize, Char<b'\n'>);

pub fn solve<const P1: bool, const P2: bool>(mut buf: impl BufRead) -> Option<()> {
    let mut bytes = Vec::new();
    buf.read_to_end(&mut bytes).ok()?;
    let parsed: Vec<Line> = parse(bytes)?;

    let mut part1 = Snake::<2>::new();
    let mut part2 = Snake::<10>::new();

    for (dir, _, d, _) in parsed {
        f!(P1, part1.mov(dir, d));
        f!(P2, part2.mov(dir, d));
    }

    if P1 {
        println!("Part 1: {}", part1.loc.len());
    }
    if P2 {
        println!("Part 2: {}", part2.loc.len());
    }

    Some(())
}
