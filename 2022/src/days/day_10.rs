use std::io::BufRead;

use crate::{parse, Char};
use derive::Parse;

#[derive(Parse, Debug)]
enum Com {
    Noop,
    Addx(Char<b' '>, i32),
}

type Line = (Com, Char<b'\n'>);

struct Prog<const P1: bool, const P2: bool> {
    reg: i32,
    cycle: i32,
    part1: i32,
    part2: String,
}
impl<const P1: bool, const P2: bool> Prog<P1, P2> {
    fn new() -> Self {
        Prog {
            reg: 1,
            cycle: 0,
            part1: 0,
            part2: String::new(),
        }
    }
    fn handle(&mut self, com: Com) {
        match com {
            Com::Noop => self.inc(),
            Com::Addx(_, x) => {
                self.inc();
                self.inc();
                self.reg += x;
            }
        }
    }

    fn inc(&mut self) {
        if P2 {
            if (self.reg - self.cycle % 40).abs() < 2 {
                self.part2 += "█";
            } else {
                self.part2 += " ";
            }
        }
        self.cycle += 1;
        if P1 {
            if (self.cycle + 20) % 40 == 0 {
                self.part1 += self.reg * self.cycle;
            }
        }
        if P2 {
            if self.cycle % 40 == 0 {
                self.part2 += "\n";
            }
        }
    }
}

pub fn solve<const P1: bool, const P2: bool>(mut buf: impl BufRead) -> Option<()> {
    let mut bytes = Vec::new();
    buf.read_to_end(&mut bytes).ok()?;
    let parsed: Vec<Line> = parse(&bytes)?;

    let mut prog = Prog::<P1, P2>::new();
    for (com, _) in parsed {
        prog.handle(com);
    }

    if P1 {
        println!("Part 1: {}", prog.part1);
    }
    if P2 {
        println!("Part 2: \n{}", prog.part2);
    }
    Some(())
}
