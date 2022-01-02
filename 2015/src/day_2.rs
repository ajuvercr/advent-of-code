use crate::parser::*;
use std::usize;

#[derive(Debug)]
struct Present {
    l: usize,
    w: usize,
    h: usize,
}

impl Parser for Present {
    fn parse<'a>(s: &'a str) -> Option<(Self, &'a str)> {
        let (l, s) = s.mparse::<usize>()?;
        let (w, s) = s[1..].mparse::<usize>()?;
        let (h, s) = s[1..].mparse::<usize>()?;
        Some((Present { l, w, h }, s))
    }
}

impl Present {
    fn area(&self) -> usize {
        let Present { l, w, h } = self;
        let m = (l * w).min((w * h).min(h * l));
        2 * l * w + 2 * w * h + 2 * h * l + m
    }

    fn volume(&self) -> usize {
        self.l * self.w * self.h
    }

    fn ribbon(&self) -> usize {
        let r = 2 * (self.h + self.l);
        let f = 2 * (self.h + self.w);
        let t = 2 * (self.l + self.w);

        r.min(f.min(t)) + self.volume()
    }
}

pub fn solve(file: &str) {
    use std::fs;
    let s = fs::read_to_string(file).unwrap();

    let x: Vec<Present> = wparse(&s).unwrap();

    println!("part 1: {}", x.iter().map(Present::area).sum::<usize>());
    println!("part 2: {}", x.iter().map(Present::ribbon).sum::<usize>());
}
