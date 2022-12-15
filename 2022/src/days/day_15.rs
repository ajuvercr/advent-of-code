use std::{collections::HashSet, io::BufRead};

use crate::{parse, Parse};

type Point = (i64, i64);

fn man((ax, ay): Point, (bx, by): Point) -> i64 {
    (ax - bx).abs() + (ay - by).abs()
}

#[derive(Debug)]
struct Sensor {
    pos: Point,
    closest: Point,
    dist: i64,
}

impl<'a> Parse<'a> for Sensor {
    fn parse(buf: &mut crate::Cursor) -> Option<Self> {
        buf.next_n("Sensor at x=".len());
        let x = i64::parse(buf)?;
        buf.next_n(", y=".len());
        let y = i64::parse(buf)?;

        buf.next_n(": closest beacon is at x=".len());
        let cx = i64::parse(buf)?;
        buf.next_n(", y=".len());
        let cy = i64::parse(buf)?;
        buf.next();

        let pos = (x, y);
        let closest = (cx, cy);
        let dist = man(pos, closest);

        Self { pos, closest, dist }.into()
    }
}

impl Sensor {
    fn seg(&self, y: i64) -> Option<Seg> {
        let d = (self.pos.1 - y).abs();
        let dx = self.dist - d;
        if dx <= 0 {
            None
        } else {
            Seg {
                start: self.pos.0 - dx,
                end: self.pos.0 + dx + 1,
            }
            .into()
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
struct Seg {
    start: i64,
    end: i64,
}

struct Line<'a> {
    segs: &'a Vec<Seg>,
}

impl<'a> Line<'a> {
    pub fn count(&self) -> i64 {
        let mut count = 0;
        let mut end_done = self.segs.first().map(|x| x.start).unwrap() - 1;

        for seg in self.segs {
            let (start, end) = (seg.start, seg.end);
            if end < end_done {
                continue;
            }

            let s = start.max(end_done);
            count += end - s;
            end_done = end;
        }

        count
    }

    pub fn skips(&self, s: i64, e: i64) -> Option<i64> {
        let mut end_done = s;

        for seg in self.segs {
            let (start, end) = (seg.start, seg.end);
            if end < end_done {
                continue;
            }

            if start > end_done {
                return Some(start - 1);
            }

            end_done = end;

            if end_done > e {
                return None;
            }
        }

        None
    }
}

pub fn solve<const P1: bool, const P2: bool>(mut buf: impl BufRead) -> Option<()> {
    let mut bytes = Vec::new();
    buf.read_to_end(&mut bytes).ok()?;
    let parsed: Vec<Sensor> = parse(bytes)?;

    let target_y = 2000000;

    if P1 {
        let set: HashSet<Point> = parsed.iter().map(|x| x.closest).collect();
        let beacon_count = set.iter().filter(|x| x.1 == target_y).count();

        let mut segs: Vec<Seg> = parsed.iter().flat_map(|x| x.seg(target_y)).collect();
        segs.sort();
        let line_y = Line { segs: &segs };

        let part1 = line_y.count();
        println!("Part 1: {}", part1 - beacon_count as i64);
    }

    if P2 {
        let mut segs = Vec::with_capacity(1000);
        for y in 0..4000000 {
            segs.clear();
            segs.extend(parsed.iter().flat_map(|x| x.seg(y)));
            segs.sort();
            let line = Line { segs: &segs };
            if let Some(x) = line.skips(0, 4000000) {
                println!("Part 2: {}", x * 4000000 + y);
                break;
            }
        }
    }
    Some(())
}
