use std::{
    fmt::{Debug, Display},
    io::BufRead,
};

use crate::{parse, Char, Parse, Puncuated, WS};

#[derive(Debug, derive::Parse, Copy, Clone)]
struct Worry(isize);

#[derive(Debug, Clone)]
enum Op {
    Mul(Num, Num),
    Plus(Num, Num),
}

impl Op {
    fn apply(&self, item: isize) -> isize {
        match self {
            Op::Mul(l, i) => l.get(item) * i.get(item),
            Op::Plus(l, i) => l.get(item) + i.get(item),
        }
    }
}

impl<'a> Parse<'a> for Op {
    fn parse(buf: &mut crate::Cursor) -> Option<Self> {
        buf.next_n("  Operation: new = ".len());
        let left = Num::parse(buf)?;
        buf.next();
        let op = *buf.next();
        buf.next();
        let right = Num::parse(buf)?;

        match op {
            b'+' => Some(Self::Plus(left, right)),
            b'*' => Some(Self::Mul(left, right)),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
enum Num {
    Old,
    Const(isize),
}

impl Num {
    fn get(&self, old: isize) -> isize {
        match self {
            Num::Old => old,
            Num::Const(x) => *x,
        }
    }
}

impl<'a> Parse<'a> for Num {
    fn parse(buf: &mut crate::Cursor) -> Option<Self> {
        if let Some(n) = isize::parse(buf) {
            Some(Self::Const(n))
        } else {
            let old = buf.next_n(3);
            if old == b"old" {
                Some(Self::Old)
            } else {
                println!("Expected 'old' found {}", unsafe {
                    std::str::from_utf8_unchecked(old)
                });
                None
            }
        }
    }
}

#[derive(Debug, Clone)]
struct Test(isize);
impl<'a> Parse<'a> for Test {
    fn parse(buf: &mut crate::Cursor) -> Option<Self> {
        buf.next_n("  test: divisible by ".len());
        Some(Self(isize::parse(buf)?))
    }
}

impl Test {
    fn test(&self, item: isize) -> bool {
        item % self.0 == 0
    }
}

#[derive(Debug, Clone)]
struct Monkey {
    id: usize,
    items: Vec<Worry>,
    operation: Op,
    test: Test,
    res: (usize, usize),

    business: usize,
}

impl Monkey {
    fn throw(&mut self, ring: Option<isize>) -> Vec<(usize, Worry)> {
        self.items
            .drain(..)
            .map(|Worry(i)| {
                self.business += 1;
                let n = self.operation.apply(i);
                let n = if let Some(r) = ring { n % r } else { n / 3 };
                if self.test.test(n) {
                    (self.res.0, Worry(n))
                } else {
                    (self.res.1, Worry(n))
                }
            })
            .collect()
    }
}

impl<'a> Parse<'a> for Monkey {
    fn parse(buf: &mut crate::Cursor) -> Option<Self> {
        buf.next_n("Monkey ".len());
        let id = usize::parse(buf)?;
        buf.next_n(":\n  Starting items: ".len());
        let items: Puncuated<Worry, (Char<b','>, Char<b' '>)> = Puncuated::parse(buf)?;
        buf.next();
        let op = Op::parse(buf)?;
        buf.next();
        let test = Test::parse(buf)?;
        buf.next();

        buf.next_n("    If true: throw to monkey ".len());
        let left = usize::parse(buf)?;
        buf.next();
        buf.next_n("    If false: throw to monkey ".len());
        let right = usize::parse(buf)?;

        Some(Self {
            id,
            items: items.into_inner(),
            operation: op,
            test,
            res: (left, right),
            business: 0,
        })
    }
}

impl Display for Monkey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Monkey {} (business {}): ", self.id, self.business)?;

        let mut iter = self.items.iter();
        while let Some(x) = iter.next() {
            write!(f, "{}", x.0)?;

            if !iter.is_empty() {
                write!(f, ", ")?;
            }
        }

        std::fmt::Result::Ok(())
    }
}

pub fn solve<const P1: bool, const P2: bool>(mut buf: impl BufRead) -> Option<()> {
    let mut bytes = Vec::new();
    buf.read_to_end(&mut bytes).ok()?;
    let monkeys: Vec<(Monkey, WS)> = parse(&bytes)?;
    let mut monkeys: Vec<Monkey> = monkeys.into_iter().map(|(x, _)| x).collect();
    let mut m2: Vec<Monkey> = monkeys.clone();

    if P1 {
        for _ in 0..20 {
            for i in 0..monkeys.len() {
                let throws = monkeys[i].throw(None);
                for (j, t) in throws {
                    monkeys[j].items.push(t);
                }
            }
        }
        monkeys.sort_unstable_by(|x, y| x.business.cmp(&y.business).reverse());
    }

    if P2 {
        let ring: isize = monkeys.iter().map(|m| m.test.0).product();
        for _ in 0..10000 {
            for i in 0..monkeys.len() {
                let throws = m2[i].throw(Some(ring));
                for (j, t) in throws {
                    m2[j].items.push(t);
                }
            }
        }
        m2.sort_unstable_by(|x, y| x.business.cmp(&y.business).reverse());
    }

    if P1 {
        println!("Part 1: {}", monkeys[0].business * monkeys[1].business);
    }
    if P2 {
        println!("Part 2: {}", m2[0].business * m2[1].business);
    }
    Some(())
}
