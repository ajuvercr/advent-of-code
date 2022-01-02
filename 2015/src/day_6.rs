use crate::parser::*;
use std::fs;

#[derive(Debug)]
enum Action {
    Toggle,
    Off,
    On,
}
impl Parser for Action {
    fn parse<'a>(s: &'a str) -> Option<(Self, &'a str)> {
        if let Some(x) = s.strip_prefix("toggle") {
            return (Action::Toggle, x).into();
        }
        if let Some(x) = s.strip_prefix("turn off") {
            return (Action::Off, x).into();
        }
        if let Some(x) = s.strip_prefix("turn on") {
            return (Action::On, x).into();
        }
        None
    }
}
impl Action {
    fn apply(&self, c: isize, part1: bool) -> isize {
        if part1 {
            match self {
                &Action::Toggle => (c + 1) % 2,
                &Action::Off => 0,
                &Action::On => 1,
            }
        } else {
            match self {
                &Action::On => c + 1,
                &Action::Off => 0.max(c - 1),
                &Action::Toggle => c + 2,
            }
        }
    }
}

#[derive(Debug)]
struct Pair {
    x: usize,
    y: usize,
}
impl Parser for Pair {
    fn parse<'a>(s: &'a str) -> Option<(Self, &'a str)> {
        let (x, s) = s.mparse()?;
        let (y, s) = s[1..].mparse()?;
        (Pair { x, y }, s).into()
    }
}

#[derive(Debug)]
struct AC {
    action: Action,
    start: Pair,
    end: Pair,
}

impl Parser for AC {
    fn parse<'a>(s: &'a str) -> Option<(Self, &'a str)> {
        let (action, s) = s.mparse()?;
        let (start, s) = s[1..].mparse()?;
        let (end, s) = s[9..].mparse()?;
        Some((AC { action, start, end }, s))
    }
}

impl AC {
    fn apply(&self, lights: &mut [[isize; 1000]; 1000], part1: bool) {
        for x in self.start.x..(self.end.x + 1) {
            for y in self.start.y..(self.end.y + 1) {
                lights[x][y] = self.action.apply(lights[x][y], part1);
            }
        }
    }
}

pub fn solve(file: &str) {
    let content = fs::read_to_string(file).unwrap();
    let acs: Vec<AC> = wparse(&content).unwrap();
    let mut lights: [[isize; 1000]; 1000] = [[0; 1000]; 1000];
    acs.iter().for_each(|x| x.apply(&mut lights, true));
    let count = lights.iter().flatten().sum::<isize>();
    println!("part 1 {}", count);
    lights
        .iter_mut()
        .for_each(|x| x.iter_mut().for_each(|x| *x = 0));
    acs.iter().for_each(|x| x.apply(&mut lights, false));
    let count = lights.iter().flatten().sum::<isize>();
    println!("part 2 {}", count);
}
