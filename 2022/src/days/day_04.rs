use std::{io::BufRead, str::FromStr};

struct Assignment {
    start: u32,
    end: u32,
}

impl Assignment {
    fn overlaps(&self, other: &Self) -> bool {
        self.start <= other.start && self.end >= other.end
    }

    fn overlaps_any(&self, other: &Self) -> bool {
        self.start <= other.end && self.end >= other.start
    }
}

impl FromStr for Assignment {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut items = s.split('-');
        let (start, end) = match (items.next(), items.next()) {
            (Some(x), Some(y)) => (x, y),
            _ => return Err("Did not match"),
        };

        Ok(Assignment {
            start: start.parse().map_err(|_| "nope")?,
            end: end.parse().map_err(|_| "nope")?,
        })
    }
}

struct Pair<T>(T, T);
impl<T> FromStr for Pair<T>
where
    T: FromStr<Err = &'static str>,
{
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut items = s.split(',');
        let (start, end) = match (items.next(), items.next()) {
            (Some(x), Some(y)) => (x, y),
            _ => return Err("Did not match"),
        };

        Ok(Self(
            start.parse().map_err(|_| "nope")?,
            end.parse().map_err(|_| "nope")?,
        ))
    }
}

pub fn solve<const P1: bool, const P2: bool>(buf: impl BufRead) -> Option<()> {
    let lines = buf.lines();

    let (part1, part2) = lines
        .flatten()
        .flat_map(|x| Pair::<Assignment>::from_str(&x))
        .fold((0, 0), |(p1, p2), Pair(x, y)| {
            match (
                P1 && (x.overlaps(&y) || y.overlaps(&x)),
                P2 && (x.overlaps_any(&y) || y.overlaps_any(&x)),
            ) {
                (true, true) => (p1 + 1, p2 + 1),
                (true, false) => (p1 + 1, p2),
                (false, true) => (p1, p2 + 1),
                (false, false) => (p1, p2),
            }
        });
    if P1 {
        println!("Part 1 {}", part1);
    }
    if P2 {
        println!("Part 2 {}", part2);
    }
    Some(())
}
