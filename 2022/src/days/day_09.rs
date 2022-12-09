use std::{collections::HashSet, io::BufRead};

use crate::parser::{parse, Char};
use derive::Parse;

type Point = (isize, isize);

#[derive(Parse, Debug)]
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

type Line = (Dir, Char<b' '>, usize, Char<b'\n'>);

pub fn solve<const P1: bool, const P2: bool>(mut buf: impl BufRead) -> Option<()> {
    let mut bytes = Vec::new();
    buf.read_to_end(&mut bytes).ok()?;
    let parsed: Vec<Line> = parse(bytes)?;

    let mut head = Point::default();
    let mut tail = Point::default();

    let mut locations = HashSet::new();
    locations.insert(tail);

    for (dir, _, d, _) in parsed {
        let (dx, dy) = dir.dx();
        for _ in 0..d {
            head = (head.0 + dx, head.1 + dy);
            let x = head.0 - tail.0;
            let y = head.1 - tail.1;
            if x.abs() > 1 || y.abs() > 1 {
                tail = (tail.0 + x.signum(), tail.1 + y.signum());
                locations.insert(tail);
            }
        }
    }

    if P1 {
        println!("Part 1: {}", locations.len());
    }
    if P2 {
        println!("Part 2");
    }
    Some(())
}
