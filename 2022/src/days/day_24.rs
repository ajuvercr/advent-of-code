use std::{
    collections::{BinaryHeap, HashSet},
    hash::Hash,
    io::BufRead,
    ops::Index,
};

struct PushD<T> {
    buf: Vec<(Option<usize>, T)>,
}

impl<T> PushD<T> {
    fn push(&mut self, parent: usize, t: T) -> usize {
        let o = self.buf.len();
        self.buf.push((Some(parent), t));
        o
    }
}

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
        if self.at >= self.len {
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

#[derive(Debug, Clone, Copy)]
struct Move {
    cost: isize,
    loc: Point,
    time: usize,

    idx: usize,
}
impl PartialEq for Move {
    fn eq(&self, other: &Self) -> bool {
        other.cost == self.cost && other.loc == self.loc && other.time == self.time
    }
}
impl Eq for Move {}
impl PartialOrd for Move {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cost.cmp(&other.cost))
    }
}
impl Ord for Move {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.cost.cmp(&other.cost)
    }
}
impl Hash for Move {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_isize(self.cost);
        state.write_usize(self.loc.0);
        state.write_usize(self.loc.1);
        state.write_usize(self.time);
    }
}

impl Move {
    fn options(&self, end: Point, size: Point, push_d: &mut PushD<Point>) -> Vec<Self> {
        let mut out = Vec::new();
        let time = self.time + 1;
        if self.loc.0 < size.0 - 1 {
            let loc = (self.loc.0 + 1, self.loc.1);
            let cost = cost(loc, end) - time as isize;
            let idx = push_d.push(self.idx, loc);
            out.push(Self {
                loc,
                time,
                cost,
                idx,
            })
        }
        if self.loc.1 < size.1 - 1 {
            let loc = (self.loc.0, self.loc.1 + 1);
            let cost = cost(loc, end) - time as isize;
            let idx = push_d.push(self.idx, loc);
            out.push(Self {
                loc,
                time,
                cost,
                idx,
            })
        }
        if self.loc.0 > 0 {
            let loc = (self.loc.0 - 1, self.loc.1);
            let cost = cost(loc, end) - time as isize;
            let idx = push_d.push(self.idx, loc);
            out.push(Self {
                loc,
                time,
                cost,
                idx,
            })
        }
        if self.loc.1 > 0 {
            let loc = (self.loc.0, self.loc.1 - 1);
            let cost = cost(loc, end) - time as isize;
            let idx = push_d.push(self.idx, loc);
            out.push(Self {
                loc,
                time,
                cost,
                idx,
            })
        }

        let cost = cost(self.loc, end) - time as isize;
        let idx = push_d.push(self.idx, self.loc);
        out.push(Self {
            loc: self.loc,
            time,
            cost,
            idx,
        });

        out
    }
}

fn run<'a>(cached: &mut Cache<'a>, time: usize, start: Point, end: Point, size: Point) -> Move {
    let mut queue = BinaryHeap::new();
    let mut done = HashSet::new();
    let mut push_d = PushD {
        buf: vec![(None, start)],
    };

    queue.push(Move {
        loc: start,
        time,
        cost: cost(start, end),
        idx: 0,
    });

    loop {
        let next = queue.pop().unwrap();
        // println!("current: {:?}", next);
        if done.insert(next) {
            // if next.time > 4 {
            //     break next.time;
            // }
            if next.loc == end {
                break next;
            }
            for m in next.options(end, size, &mut push_d) {
                if cached.movable(m, start, end, size) {
                    // println!("moving {:?}", m);
                    queue.push(m);
                }
            }
        }
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

    let start = (1, 0);
    let size = (field[0].len(), field.len());
    let end = (size.0 - 2, size.1 - 1);

    let mut cached = Cache::new(bliz);
    let next = run(&mut cached, 0, start, end, size);

    // let mut locations = Vec::new();
    // let mut c = Some(next.idx);
    // while let Some(i) = c {
    //     locations.push(push_d.buf[i].1);
    //     c = push_d.buf[i].0;
    // }
    // locations.reverse();
    //
    // for (i, l) in locations.iter().enumerate() {
    //     let bliz = cached.get(i);
    //     println!("-------------------------- {}: l {:?}", i, l);
    //
    //     for y in 0..size.1 - 2 {
    //         for x in 0..size.0 - 2 {
    //             let c: usize = [
    //                 bliz.up[x][y] as usize,
    //                 bliz.down[x][y] as usize,
    //                 bliz.left[y][x] as usize,
    //                 bliz.right[y][x] as usize,
    //             ]
    //             .into_iter()
    //             .sum();
    //
    //             let ch = if c < 2 {
    //                 if bliz.up[x][y] {
    //                     '^'
    //                 } else if bliz.down[x][y] {
    //                     'v'
    //                 } else if bliz.right[y][x] {
    //                     '>'
    //                 } else if bliz.left[y][x] {
    //                     '<'
    //                 } else {
    //                     '.'
    //                 }
    //             } else {
    //                 (b'0' + c as u8) as char
    //             };
    //
    //             if *l == (x + 1, y + 1) {
    //                 print!("\x1b[93m");
    //                 print!("{}", ch);
    //                 print!("\x1b[0m");
    //             } else {
    //                 print!("{}", ch);
    //             }
    //         }
    //
    //         print!("\n");
    //     }
    // }

    if P1 {
        println!("Part 1: {}", next.time);
    }
    if P2 {
        let next = run(&mut cached, next.time, end, start, size);
        let next = run(&mut cached, next.time, start, end, size);
        println!("Part 2: {}", next.time);
    }
    Some(())
}
