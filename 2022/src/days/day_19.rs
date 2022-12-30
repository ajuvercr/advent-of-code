use std::{collections::HashSet, io::BufRead, thread};

use crate::{parse, AlphaNumTest, Parse, Word};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Ore(String);

#[derive(Debug, Clone, PartialEq, Eq)]
struct Robot {
    ty: Ore,
    costs: [usize; 3],
}

impl<'a> Parse<'a> for Robot {
    fn parse(buf: &mut crate::Cursor<'a>) -> Option<Self> {
        buf.next_n("Each ".len());
        let ty = Ore(Word::<AlphaNumTest>::parse(buf)?.0);
        buf.next_n(" robot costs ".len());
        let mut costs = [0; 3];
        loop {
            let cost = usize::parse(buf)?;
            buf.next();
            let ty = Word::<AlphaNumTest>::parse(buf)?.0;
            let i = match ty.as_str() {
                "ore" => 0,
                "clay" => 1,
                "obsidian" => 2,
                _ => panic!("Unknown type {}", ty),
            };

            costs[i] = cost;

            if buf.next() == &b'.' {
                break;
            }
            buf.next_n("and ".len());
        }

        Some(Self { ty, costs })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Blueprint {
    id: usize,
    robots: [Robot; 4],
}

impl<'a> Parse<'a> for Blueprint {
    fn parse(buf: &mut crate::Cursor<'a>) -> Option<Self> {
        if buf.is_empty() {
            return None;
        }

        buf.next_n("Blueprint ".len());
        let id = usize::parse(buf)?;
        buf.next_n(": ".len());
        let a = Robot::parse(buf)?;
        buf.next();
        let b = Robot::parse(buf)?;
        buf.next();
        let c = Robot::parse(buf)?;
        buf.next();
        let d = Robot::parse(buf)?;
        buf.next();
        Some(Self {
            id,
            robots: [a, b, c, d],
        })
    }
}

#[derive(Debug)]
struct State<'a> {
    blueprint: &'a mut Blueprint,
    robots: [usize; 4],
    ores: [usize; 4],
}

#[derive(Debug)]
enum Action {
    Do(Option<usize>),
    Undo(Option<usize>),
}

fn stack(st: &mut State<'_>, time: usize) -> Option<usize> {
    let mut time = time;
    let mut max = 0;
    let mut stack: Vec<Action> = Vec::new();
    stack.push(Action::Do(None));

    let mut cache: HashSet<([usize; 4], [usize; 4], usize)> = HashSet::new();

    while let Some(op) = stack.pop() {
        match op {
            Action::Do(x) => {
                if st.ores[3] + (st.robots[3] + time).pow(2) / 2 < max {
                    continue;
                }

                stack.push(Action::Undo(x));

                // Build robot
                if let Some(i) = x {
                    let r = &st.blueprint.robots[i].costs;
                    (0..3).for_each(|i| st.ores[i] -= r[i]);
                    st.robots[i] += 1;
                }

                // Create ores
                (0..4).for_each(|i| st.ores[i] += st.robots[i]);

                if let Some(x) = x {
                    st.ores[x] -= 1;
                }

                if time == 1 {
                    max = max.max(st.ores[3]);
                } else {
                    if time < 6 || !cache.contains(&(st.robots, st.ores, time)) {
                        if time >= 6 {
                            cache.insert((st.robots, st.ores, time));
                        }
                        // Calculate next turns
                        for i in 0..4 {
                            let i = 3 - i;
                            let r = &st.blueprint.robots[i].costs;
                            if st.ores[0] >= r[0] && st.ores[1] >= r[1] && st.ores[2] >= r[2] {
                                stack.push(Action::Do(Some(i)));
                            }
                        }
                        stack.push(Action::Do(None));
                    }
                }
                time -= 1;
            }
            Action::Undo(x) => {
                time += 1;
                if let Some(i) = x {
                    let r = &st.blueprint.robots[i].costs;
                    st.ores[0] += r[0];
                    st.ores[1] += r[1];
                    st.ores[2] += r[2];
                    st.robots[i] -= 1;
                }

                for i in 0..4 {
                    st.ores[i] -= st.robots[i];
                }
            }
        }
    }

    max.into()
}

pub fn solve<const P1: bool, const P2: bool>(mut buf: impl BufRead) -> Option<()> {
    let mut bytes = Vec::new();
    buf.read_to_end(&mut bytes).ok()?;
    let parsed: Vec<Blueprint> = parse(&bytes)?;

    if P1 {
        let mut handlers = Vec::new();
        for (i, mut blueprint) in parsed.clone().into_iter().enumerate() {
            let handler = thread::spawn(move || {
                // thread code
                let mut st = State {
                    blueprint: &mut blueprint,
                    robots: [1, 0, 0, 0],
                    ores: [0, 0, 0, 0],
                };
                let max = stack(&mut st, 24).unwrap();
                println!("max {}: {}", i, max);
                (i + 1) * max
            });
            handlers.push(handler);
        }

        let mut out = 0;
        for h in handlers {
            out += h.join().unwrap();
        }
        println!("Part 1: {:?}", out);
    }
    if P2 {
        let mut out = 1;
        for (i, mut blueprint) in parsed.clone().into_iter().take(3).enumerate() {
            // thread code
            let mut st = State {
                blueprint: &mut blueprint,
                robots: [1, 0, 0, 0],
                ores: [0, 0, 0, 0],
            };
            let max = stack(&mut st, 32).unwrap();
            println!("max {}: {}", i, max);
            out *= max;
        }

        println!("Part 2: {}", out);
    }
    Some(())
}
