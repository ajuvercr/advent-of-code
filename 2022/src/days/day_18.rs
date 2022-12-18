use std::{collections::HashSet, io::BufRead};

use crate::{parse, Parse};

const DELTAS: &[(isize, isize, isize)] = &[
    (1, 0, 0),
    (-1, 0, 0),
    (0, 1, 0),
    (0, -1, 0),
    (0, 0, 1),
    (0, 0, -1),
];

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Point {
    x: isize,
    y: isize,
    z: isize,
}

impl Point {
    fn add(&self, delta: (isize, isize, isize)) -> Self {
        Self {
            x: self.x + delta.0,
            y: self.y + delta.1,
            z: self.z + delta.2,
        }
    }

    fn inside(&self, &((mi_x, ma_x), (mi_y, ma_y), (mi_z, ma_z)): &Bounds) -> bool {
        self.x >= mi_x - 1
            && self.x <= ma_x + 1
            && self.y >= mi_y - 1
            && self.y <= ma_y + 1
            && self.z >= mi_z - 1
            && self.z <= ma_z + 1
    }
}

impl<'a> Parse<'a> for Point {
    fn parse(buf: &mut crate::Cursor<'a>) -> Option<Self> {
        let x = isize::parse(buf)?;
        buf.next();
        let y = isize::parse(buf)?;
        buf.next();
        let z = isize::parse(buf)?;
        buf.next();
        Some(Self { x, y, z })
    }
}

fn min_max((min, max): (isize, isize), x: isize) -> (isize, isize) {
    (min.min(x), max.max(x))
}

type Bounds = ((isize, isize), (isize, isize), (isize, isize));
fn flood(start: Point, lava: &HashSet<Point>, air: &mut HashSet<Point>, bound: &Bounds) {
    if air.contains(&start) {
        return;
    }

    air.insert(start);

    for d in DELTAS {
        let next = start.add(*d);
        if next.inside(bound) && !lava.contains(&next) {
            flood(next, lava, air, bound);
        }
    }
}

pub fn solve<const P1: bool, const P2: bool>(mut buf: impl BufRead) -> Option<()> {
    let mut bytes = Vec::new();
    buf.read_to_end(&mut bytes).ok()?;
    let parsed: Vec<Point> = parse(&bytes)?;
    let mut lava = HashSet::new();
    lava.extend(parsed);

    if P1 {
        let mut count = 0;
        for point in &lava {
            for d in DELTAS {
                if !lava.contains(&point.add(*d)) {
                    count += 1;
                }
            }
        }
        println!("Part 1: {}", count);
    }

    if P2 {
        let mms = (isize::MAX, isize::MIN);
        let bounds = lava.iter().fold((mms, mms, mms), |(x, y, z), p| {
            (min_max(x, p.x), min_max(y, p.y), min_max(z, p.z))
        });
        let mut air = HashSet::new();

        flood(Point { x: 1, y: 1, z: 1 }, &lava, &mut air, &bounds);
        let mut count = 0;

        for point in &lava {
            for d in DELTAS {
                if air.contains(&point.add(*d)) {
                    count += 1;
                }
            }
        }

        println!("Part 2: {}", count);
    }
    Some(())
}
