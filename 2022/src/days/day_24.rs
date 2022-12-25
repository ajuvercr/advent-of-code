use std::{
    collections::{BinaryHeap, HashSet},
    io::BufRead,
    ops::Index,
};

#[derive(Clone)]
struct Deque<'a, T> {
    vec: &'a [T],
    at: usize,
    len: usize,
}
impl<'a, T: std::fmt::Debug> std::fmt::Debug for Deque<'a, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for i in self.at..self.len {
            write!(f, "{:?}, ", self.vec[i])?;
        }
        for i in 0..self.at {
            write!(f, "{:?}, ", self.vec[i])?;
        }
        Ok(())
    }
}

impl<'a, T> Deque<'a, T> {
    fn new(vec: &'a [T]) -> Self {
        Self {
            vec,
            at: 0,
            len: vec.len(),
        }
    }

    fn rotate_left(&mut self) {
        self.at += 1;
        if self.at > self.len {
            self.at = 0;
        }
    }

    fn rotate_right(&mut self) {
        if self.at == 0 {
            self.at = self.len - 1;
        } else {
            self.at -= 1;
        }
    }
}

impl<'a, T> Index<usize> for Deque<'a, T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        debug_assert!(index < self.len);
        let idx = index + self.at;
        if idx >= self.len {
            &self.vec[idx - self.len]
        } else {
            &self.vec[idx]
        }
    }
}

type Line<'a> = Vec<Deque<'a, bool>>;

impl<'a> std::fmt::Debug for Blizzards<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "left:\n")?;
        self.left.iter().try_for_each(|x| writeln!(f, "{:?}", x))?;
        write!(f, "right:\n")?;
        self.right.iter().try_for_each(|x| writeln!(f, "{:?}", x))?;
        write!(f, "up:\n")?;
        self.up.iter().try_for_each(|x| writeln!(f, "{:?}", x))?;
        write!(f, "down:\n")?;
        self.down.iter().try_for_each(|x| writeln!(f, "{:?}", x))?;

        Ok(())
    }
}
#[derive(Clone)]
struct Blizzards<'a> {
    left: Line<'a>,
    right: Line<'a>,
    up: Line<'a>,
    down: Line<'a>,
}

fn n_l<'a>(r: &'a Vec<Vec<bool>>) -> Line<'a> {
    r.iter().map(|x| Deque::new(x)).collect::<Vec<_>>()
}
impl<'a> Blizzards<'a> {
    fn turn(&self) -> Self {
        let mut this = self.clone();
        this.left.iter_mut().for_each(|x| x.rotate_left());
        this.right.iter_mut().for_each(|x| x.rotate_right());
        this.up.iter_mut().for_each(|x| x.rotate_left());
        this.down.iter_mut().for_each(|x| x.rotate_right());
        this
    }
}

struct Cache<'a> {
    cached: Vec<Blizzards<'a>>,
}

impl<'a> Cache<'a> {
    fn new(blizz: Blizzards<'a>) -> Self {
        Self {
            cached: vec![blizz],
        }
    }
    fn get(&mut self, index: usize) -> &Blizzards<'a> {
        while index >= self.cached.len() {
            let last = self.cached.last().unwrap();
            self.cached.push(last.turn());
        }

        &self.cached[index]
    }

    fn movable(&mut self, mov: Move, start: Point, target: Point, size: Point) -> bool {
        let loc = mov.loc;
        if loc == target || loc == start {
            return true;
        }
        if loc.0 == 0 || loc.1 == 0 || loc.0 == size.0 - 1 || loc.1 == size.1 - 1 {
            return false;
        }

        let bliz = self.get(mov.time);
        let c = loc.0 - 1;
        let r = loc.1 - 1;

        !(bliz.left[r][c] || bliz.right[r][c] || bliz.up[c][r] || bliz.down[c][r])
    }
}

type Point = (usize, usize);

fn cost(loc: Point, target: Point) -> isize {
    let c = loc.0.abs_diff(target.0) + loc.1.abs_diff(target.1) + 1;
    -1 * c as isize
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Move {
    cost: isize,
    loc: Point,
    time: usize,
}

impl Move {
    fn options(&self, end: Point, size: Point) -> Vec<Self> {
        let mut out = Vec::new();
        let time = self.time + 1;
        if self.loc.0 < size.0 - 1 {
            let loc = (self.loc.0 + 1, self.loc.1);
            let cost = cost(loc, end) - time as isize;
            out.push(Self { loc, time, cost })
        }
        if self.loc.1 < size.1 - 1 {
            let loc = (self.loc.0, self.loc.1 + 1);
            let cost = cost(loc, end) - time as isize;
            out.push(Self { loc, time, cost })
        }
        if self.loc.0 > 0 {
            let loc = (self.loc.0 - 1, self.loc.1);
            let cost = cost(loc, end) - time as isize;
            out.push(Self { loc, time, cost })
        }
        if self.loc.1 > 0 {
            let loc = (self.loc.0, self.loc.1 - 1);
            let cost = cost(loc, end) - time as isize;
            out.push(Self { loc, time, cost })
        }

        let cost = cost(self.loc, end) - time as isize;
        out.push(Self {
            loc: self.loc,
            time,
            cost,
        });

        out
    }
}

pub fn solve<const P1: bool, const P2: bool>(buf: impl BufRead) -> Option<()> {
    let lines = buf.split(b'\n').flatten();
    let field: Vec<Vec<u8>> = lines.collect();

    let rows = field.len();
    let cols = field[0].len();

    let left: Vec<Vec<bool>> = (1..rows - 1)
        .map(|r| (1..cols - 1).map(|c| field[r][c] == b'<').collect())
        .collect();
    let right: Vec<Vec<bool>> = (1..rows - 1)
        .map(|r| (1..cols - 1).map(|c| field[r][c] == b'>').collect())
        .collect();

    let up: Vec<Vec<bool>> = (1..cols - 1)
        .map(|c| (1..rows - 1).map(|r| field[r][c] == b'^').collect())
        .collect();
    let down: Vec<Vec<bool>> = (1..cols - 1)
        .map(|c| (1..rows - 1).map(|r| field[r][c] == b'v').collect())
        .collect();

    let bliz = Blizzards {
        left: n_l(&left),
        right: n_l(&right),
        up: n_l(&up),
        down: n_l(&down),
    };

    let mut cached = Cache::new(bliz);
    let mut queue = BinaryHeap::new();
    let mut done = HashSet::new();

    let start = (1, 0);
    let size = (field[0].len(), field.len());
    let end = (size.0 - 2, size.1 - 1);
    println!(
        "moving  from {:?} to {:?} (size {:?} cost {})",
        start,
        end,
        size,
        cost(end, start)
    );
    queue.push(Move {
        loc: start,
        time: 0,
        cost: cost(start, end),
    });

    let time = loop {
        let next = queue.pop().unwrap();
        // println!("current: {:?}", next);
        if done.insert(next) {
            // if next.time > 4 {
            //     break next.time;
            // }
            if next.loc == end {
                break next.time;
            }
            for m in next.options(end, size) {
                if cached.movable(m, start, end, size) {
                    // println!("moving {:?}", m);
                    queue.push(m);
                }
            }
        }
    };

    if P1 {
        println!("Part 1: {}", time);
    }
    if P2 {
        println!("Part 2");
    }
    Some(())
}
