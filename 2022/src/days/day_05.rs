use std::{cmp::Ordering, io::BufRead};

#[derive(Clone)]
struct Day(char);

fn borrow_parts<T>(arr: &mut [T], index0: usize, index1: usize) -> (&mut T, &mut T) {
    let mut iter = arr.iter_mut();
    match index0.cmp(&index1) {
        Ordering::Less => {
            let item0 = iter.nth(index0).unwrap();
            let item1 = iter.nth(index1 - index0 - 1).unwrap();
            (item0, item1)
        }
        Ordering::Equal => panic!("[T]::get_two_mut(): received same index twice ({})", index0),
        Ordering::Greater => {
            let item1 = iter.nth(index1).unwrap();
            let item0 = iter.nth(index0 - index1 - 1).unwrap();
            (item0, item1)
        }
    }
}

#[derive(Debug, Copy, Clone)]
struct Move(usize, usize, usize);
impl Move {
    fn handle_1(self, crates: &mut [Vec<Day>]) {
        for _ in 0..self.0 {
            if let Some(x) = crates[self.1].pop() {
                crates[self.2].push(x);
            }
        }
    }

    fn handle_2(self, crates: &mut [Vec<Day>]) {
        assert_ne!(self.1, self.2);
        let (source, target) = borrow_parts(crates, self.1, self.2);

        target.extend(source.drain(source.len() - self.0..));
    }
}

type Crates = [Vec<Day>; 10];

fn parse<I: Iterator<Item = String>>(lines: &mut I) -> Option<Crates> {
    let mut crates: Crates = Default::default();

    'outer: loop {
        let line = lines.next()?;
        for (i, c) in line.chars().skip(1).step_by(4).enumerate() {
            if c.is_digit(10) {
                lines.next()?;
                break 'outer;
            }

            if c.is_alphabetic() {
                crates[i + 1].push(Day(c));
            }
        }
    }

    for i in 0..10 {
        crates[i].reverse();
    }

    Some(crates)
}

pub fn solve<const P1: bool, const P2: bool>(buf: impl BufRead) -> Option<()> {
    let mut lines = buf.lines().flatten();
    let (mut c1, mut c2): (Crates, Crates) = {
        let crates = parse(&mut lines)?;
        match (P1, P2) {
            (true, true) => (crates.clone(), crates),
            (true, false) => (crates, Crates::default()),
            (false, true) => (Crates::default(), crates),
            (false, false) => return Some(()),
        }
    };

    while let Some(line) = lines.next() {
        let mut words = line.split(' ').skip(1).step_by(2);
        let count: usize = words.next()?.parse().ok()?;
        let start: usize = words.next()?.parse().ok()?;
        let end: usize = words.next()?.parse().ok()?;

        let mov = Move(count, start, end);
        if P1 {
            mov.handle_1(&mut c1);
        }
        if P2 {
            mov.handle_2(&mut c2);
        }
    }

    if P1 {
        let output: String = c1
            .iter()
            .skip(1)
            .map(|x| x.last())
            .map(|x| x.map(|x| x.0).unwrap_or(' '))
            .collect();
        println!("Part 1: {}", output);
    }
    if P2 {
        let output: String = c2
            .iter()
            .skip(1)
            .map(|x| x.last())
            .map(|x| x.map(|x| x.0).unwrap_or(' '))
            .collect();
        println!("Part 2: {}", output);
    }

    Some(())
}
