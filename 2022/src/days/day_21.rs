use std::{collections::HashMap, io::BufRead};

use crate::{parse, AlphaTest, Char, Parse, Str};

#[derive(Clone, Debug, Copy)]
enum Op {
    Plus,
    Minus,
    Mul,
    Div,
}

impl Op {
    fn apply<'a>(
        &self,
        ai: &'a str,
        a: &Monkey<'_>,
        bi: &'a str,
        b: &Monkey<'_>,
    ) -> Option<Monkey<'a>> {
        match (a.is_var(), b.is_var()) {
            (true, true) => panic!(),
            (false, false) => {
                let a = a.get_num()?;
                let b = b.get_num()?;

                let num = match self {
                    Op::Plus => a + b,
                    Op::Minus => a - b,
                    Op::Mul => a * b,
                    Op::Div => a / b,
                };
                Monkey::Num(num).into()
            }
            (true, false) => Monkey::VarOp1(*self, ai, b.get_num()?).into(),
            (false, true) => Monkey::VarOp2(*self, a.get_num()?, bi).into(),
        }
    }
    fn inv_apply(&self, target: i64, a: i64, rhs: bool) -> i64 {
        match (self, rhs) {
            (Op::Plus, _) => target - a,
            (Op::Minus, true) => target + a,
            (Op::Minus, false) => a - target,
            (Op::Mul, _) => target / a,
            (Op::Div, true) => target * a,
            (Op::Div, false) => a / target,
        }
    }
}

impl From<char> for Op {
    fn from(value: char) -> Self {
        match value {
            '+' => Self::Plus,
            '*' => Self::Mul,
            '-' => Self::Minus,
            '/' => Self::Div,
            _ => panic!(),
        }
    }
}

#[derive(Clone, Debug, Copy)]
enum Monkey<'a> {
    VarOp1(Op, &'a str, i64),
    VarOp2(Op, i64, &'a str),
    Var,
    Num(i64),
    Op(Op, &'a str, &'a str),
}

impl<'a> Monkey<'a> {
    fn inv_apply(&self, target: i64) -> Result<(i64, &'a str), i64> {
        match self {
            Monkey::VarOp1(o, out, rhs) => Ok((o.inv_apply(target, *rhs, true), out)),
            Monkey::VarOp2(o, lhs, out) => Ok((o.inv_apply(target, *lhs, false), out)),
            Monkey::Var => Err(target),
            _ => panic!(),
        }
    }
}

impl<'a> Monkey<'a> {
    fn is_var(&self) -> bool {
        match self {
            Monkey::Var => true,
            Monkey::VarOp1(_, _, _) => true,
            Monkey::VarOp2(_, _, _) => true,
            _ => false,
        }
    }
    fn is_num(&self) -> bool {
        (match self {
            Monkey::Num(_) => true,
            _ => false,
        }) || self.is_var()
    }
    fn get_num(&self) -> Option<i64> {
        match self {
            Monkey::Num(x) => Some(*x),
            _ => None,
        }
    }
}

type S = Char<b' '>;
type W<'a> = Str<'a, AlphaTest>;

impl<'a> Parse<'a> for (&'a str, Monkey<'a>) {
    fn parse(buf: &mut crate::Cursor<'a>) -> Option<Self> {
        let id = W::<'a>::parse(buf)?;
        buf.next_n(2);
        match Result::<i64, (W<'a>, S, char, S, W<'a>)>::parse(buf)? {
            Ok(i) => Some((id.0, Monkey::Num(i))),
            Err((f, _, o, _, s)) => Some((id.0, Monkey::Op(o.into(), f.0, s.0))),
        }
    }
}

fn part1<'a>(it: impl Iterator<Item = (&'a str, Monkey<'a>)>) -> i64 {
    let mut map = HashMap::new();

    for (id, m) in it {
        map.insert(id, m);
    }

    let mut stack = Vec::new();
    stack.push("root");

    while !stack.is_empty() {
        let id = stack.pop().unwrap();
        match map[&id] {
            Monkey::Num(_) => {}
            Monkey::Op(o, fi, si) => {
                let f = map[fi];
                let s = map[si];
                if let Some(x) = o.apply(fi, &f, si, &s) {
                    map.insert(id, x);
                } else {
                    stack.push(id);
                    if !f.is_num() {
                        stack.push(fi);
                    }
                    if !s.is_num() {
                        stack.push(si);
                    }
                }
            }
            _ => panic!(),
        }
    }
    map["root"].get_num().unwrap()
}

fn part2<'a>(it: impl Iterator<Item = (&'a str, Monkey<'a>)>) -> i64 {
    let mut map = HashMap::new();

    for (id, m) in it {
        map.insert(id, m);
    }

    map.insert("humn", Monkey::Var);

    let mut stack = Vec::new();
    stack.push("root");

    while !stack.is_empty() {
        let id = stack.pop().unwrap();
        match map[&id] {
            Monkey::Var => {}
            Monkey::Num(_) => {}
            Monkey::Op(o, fi, si) => {
                let f = map[fi];
                let s = map[si];
                if let Some(x) = o.apply(fi, &f, si, &s) {
                    map.insert(id, x);
                } else {
                    stack.push(id);
                    if !f.is_num() {
                        stack.push(fi);
                    }
                    if !s.is_num() {
                        stack.push(si);
                    }
                }
            }
            _ => {}
        }
    }

    let (mut target, mut current) = match map["root"] {
        Monkey::VarOp1(_, current, target) => (target, current),
        Monkey::VarOp2(_, target, current) => (target, current),
        _ => panic!(),
    };

    loop {
        let n = map[current].inv_apply(target);
        match n {
            Ok((t, c)) => {
                target = t;
                current = c;
            }
            Err(x) => return x,
        }
    }
}

pub fn solve<const P1: bool, const P2: bool>(mut buf: impl BufRead) -> Option<()> {
    let mut bytes = Vec::new();
    buf.read_to_end(&mut bytes).ok()?;
    let parsed: Vec<((&str, Monkey), Char<b'\n'>)> = parse(&bytes)?;

    if P1 {
        let p1 = part1(parsed.iter().map(|(x, _)| *x));
        println!("Part 1: {:?}", p1);
    }
    if P2 {
        let p2 = part2(parsed.iter().map(|(x, _)| *x));
        println!("Part 2: {}", p2);
    }
    Some(())
}
