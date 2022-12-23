use std::{collections::HashSet, io::BufRead};

type Point = (i64, i64);

fn plus(a: &Point, b: &Point) -> Point {
    (a.0 + b.0, a.1 + b.1)
}

#[derive(Clone, Copy, Debug)]
enum Move {
    North,
    South,
    West,
    East,
}
const N: Point = (0, -1);
const E: Point = (1, 0);
const S: Point = (0, 1);
const W: Point = (-1, 0);
const NE: Point = (1, -1);
const NW: Point = (-1, -1);
const SE: Point = (1, 1);
const SW: Point = (-1, 1);

const ALL: &[Point] = &[N, E, S, W, NE, NW, SE, SW];

impl Move {
    fn tests(&self, a: Point) -> [Point; 3] {
        match self {
            Move::North => [plus(&a, &N), plus(&a, &NE), plus(&a, &NW)],
            Move::South => [plus(&a, &S), plus(&a, &SE), plus(&a, &SW)],
            Move::West => [plus(&a, &W), plus(&a, &SW), plus(&a, &NW)],
            Move::East => [plus(&a, &E), plus(&a, &SE), plus(&a, &NE)],
        }
    }
    fn mov(&self, a: Point) -> Point {
        match self {
            Move::North => plus(&N, &a),
            Move::South => plus(&S, &a),
            Move::West => plus(&W, &a),
            Move::East => plus(&E, &a),
        }
    }
}

#[derive(Clone)]
struct MoveGenerator {
    moves: [Move; 4],
}

impl MoveGenerator {
    fn moves(&self) -> &[Move; 4] {
        &self.moves
    }

    fn turn(&mut self) {
        let first = self.moves[0];
        for i in 0..3 {
            self.moves[i] = self.moves[i + 1];
        }
        self.moves[3] = first;
    }
}

#[derive(Clone)]
struct Elve {
    current: Point,
    target: Point,
}

impl Elve {
    fn new(current: Point) -> Self {
        Self {
            target: current,
            current,
        }
    }

    fn calculate_next(
        &mut self,
        gen: &[Move; 4],
        current: &HashSet<Point>,
        done: &mut HashSet<Point>,
        bad: &mut HashSet<Point>,
    ) {
        // Check if I should move
        let next = if ALL
            .iter()
            .map(|delta| plus(delta, &self.current))
            .any(|p| current.contains(&p))
        {
            let mut o = None;
            for m in gen {
                if !m
                    .tests(self.current)
                    .into_iter()
                    .any(|p| current.contains(&p))
                {
                    o = Some(m.mov(self.current));
                    break;
                }
            }
            o.unwrap_or(self.current)
        } else {
            self.current
        };

        self.target = next;

        if !done.insert(next) {
            bad.insert(next);
        }
    }

    fn move_me(&mut self, current: &mut HashSet<Point>, bad: &HashSet<Point>) -> bool {
        let o = if !bad.contains(&self.target) {
            let moved = self.current != self.target;
            self.current = self.target;
            moved
        } else {
            false
        };
        current.insert(self.current);
        o
    }
}

fn count(st: &HashSet<Point>) -> i64 {
    let (min_width, max_width) = st.iter().fold((i64::MAX, i64::MIN), |(mi, ma), p| {
        (mi.min(p.0), ma.max(p.0))
    });
    let (min_height, max_height) = st.iter().fold((i64::MAX, i64::MIN), |(mi, ma), p| {
        (mi.min(p.1), ma.max(p.1))
    });

    (max_width - min_width + 1) * (max_height - min_height + 1) - st.len() as i64
}

#[derive(Clone)]
struct State {
    elves: Vec<Elve>,
    current: HashSet<Point>,
    bad: HashSet<Point>,
    done: HashSet<Point>,
    gen: MoveGenerator,
}
impl State {
    fn new(elves: &[Elve]) -> Self {
        let elves = elves.to_vec();
        let current = elves.iter().map(|e| e.current).collect();
        let gen = MoveGenerator {
            moves: [Move::North, Move::South, Move::West, Move::East],
        };

        Self {
            elves,
            current,
            gen,
            bad: HashSet::new(),
            done: HashSet::new(),
        }
    }
    fn turn(&mut self) -> bool {
        let mut out = false;
        self.elves.iter_mut().for_each(|e| {
            e.calculate_next(
                self.gen.moves(),
                &self.current,
                &mut self.done,
                &mut self.bad,
            )
        });

        self.current.clear();
        self.elves.iter_mut().for_each(|e| {
            let moved = e.move_me(&mut self.current, &self.bad);
            out = out || moved;
        });
        self.done.clear();
        self.bad.clear();
        self.gen.turn();
        out
    }
}

pub fn solve<const P1: bool, const P2: bool>(buf: impl BufRead) -> Option<()> {
    let mut i = 0;
    let mut j = 0;
    let mut elves = Vec::new();
    for b in buf.bytes().flatten() {
        if b == b'\n' {
            i = 0;
            j += 1;
            continue;
        }
        if b == b'#' {
            elves.push(Elve::new((i, j)));
        }
        i += 1;
    }

    if P1 {
        let mut state = State::new(&elves);
        for _ in 0..10 {
            state.turn();
        }

        println!("Part 1: {}", count(&state.current));
    }
    if P2 {
        let mut state = State::new(&elves);
        let mut i = 0;

        loop {
            i += 1;
            if !state.turn() {
                break;
            }
        }
        println!("Part 2: {}", i);
    }
    Some(())
}
