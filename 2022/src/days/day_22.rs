use std::{
    collections::{HashMap, HashSet},
    io::BufRead,
};

use crate::parse;

type T = isize;
fn det<const A: usize>(a: [[T; A]; A]) -> T {
    if A == 1 {
        return a[0][0];
    }
    if A == 2 {
        return a[0][0] * a[1][1] - a[0][1] * a[1][0];
    }
    let (a, b, c, d, e, f, g, h, i) = (
        a[0][0], a[0][1], a[0][2], a[1][0], a[1][1], a[1][2], a[2][0], a[2][1], a[2][2],
    );
    a * e * i + b * f * g + c * d * h - c * e * g - b * d * i - a * f * h
}

// [ a b c ]
// [ d e f ]
// [ g h i ]
fn inv(a: [[T; 3]; 3]) -> [[T; 3]; 3] {
    let de = det(a);
    let (a, b, c, d, e, f, g, h, i) = (
        a[0][0], a[0][1], a[0][2], a[1][0], a[1][1], a[1][2], a[2][0], a[2][1], a[2][2],
    );

    [
        [
            det([[e, f], [h, i]]) / de,
            det([[d, f], [g, i]]) / de,
            det([[d, e], [g, h]]) / de,
        ],
        [
            det([[b, c], [h, i]]) / de,
            det([[a, c], [g, i]]) / de,
            det([[a, b], [g, h]]) / de,
        ],
        [
            det([[b, c], [e, f]]) / de,
            det([[a, c], [d, f]]) / de,
            det([[a, b], [d, e]]) / de,
        ],
    ]
}

fn mul<const A: usize, const B: usize, const C: usize>(
    a: [[T; A]; B],
    b: [[T; B]; C],
) -> [[T; A]; C] {
    let mut o = [[0; A]; C];

    for row in 0..A {
        for col in 0..C {
            let mut acc = 0;

            for i in 0..B {
                acc += a[i][row] * b[col][i];
            }

            o[col][row] = acc;
        }
    }
    o
}

#[cfg(test)]
mod tests {

    use crate::days::day_22::mul;

    use super::Dir;
    fn p(x: isize, y: isize, z: isize) -> [[isize; 1]; 3] {
        [[x], [y], [z]]
    }

    #[test]
    fn test1() {
        let rot = [[0, 1], [1, 0]];
        let p = [[5], [0]];
        let p2 = mul(p, rot);

        assert_eq!(p2, [[0], [5]]);
    }

    #[test]
    fn test2() {
        let rot = [[0, 1], [1, 0]];
        let p = [[1, 0], [0, 1]];
        let p2 = mul(p, rot);

        assert_eq!(p2, [[0, 1], [1, 0]]);
    }

    #[test]
    fn test_down() {
        let rot = Dir::S.rot();
        let x = p(1, 0, 0);
        let pt = mul(x, rot);

        assert_eq!(pt, p(0, -1, 0));

        let x = p(0, 0, 1);
        let pt = mul(x, rot);

        assert_eq!(pt, p(0, 0, 1));
    }

    #[test]
    fn test_up() {
        let rot = Dir::N.rot();
        let x = p(1, 0, 0);
        let pt = mul(x, rot);

        assert_eq!(pt, p(0, 1, 0));

        let x = p(0, 0, 1);
        let pt = mul(x, rot);

        assert_eq!(pt, p(0, 0, 1));
    }

    #[test]
    fn test_right() {
        let id = [[1, 0, 0], [0, 1, 0], [0, 0, 1]];
        let rot = mul(Dir::E.rot(), id);
        let x = p(1, 0, 0);
        let pt = mul(x, rot);
        let pt2 = mul(pt, rot);
        let pt3 = mul(pt2, rot);
        let pt4 = mul(pt3, rot);

        assert_eq!(pt, p(0, 0, 1));
        assert_eq!(pt2, p(-1, 0, 0));
        assert_eq!(pt3, p(0, 0, -1));
        assert_eq!(pt4, p(1, 0, 0));

        let x = p(0, 1, 0);
        let pt = mul(x, rot);

        assert_eq!(pt, p(0, 1, 0));
    }
}

#[derive(Clone, Copy, Default, Debug)]
enum Dir {
    #[default]
    E,
    W,
    N,
    S,
}

impl Dir {
    fn score(&self) -> usize {
        match self {
            Dir::E => 0,
            Dir::W => 2,
            Dir::N => 3,
            Dir::S => 1,
        }
    }

    fn clw(&self) -> Self {
        match self {
            Dir::E => Dir::S,
            Dir::W => Dir::N,
            Dir::N => Dir::E,
            Dir::S => Dir::W,
        }
    }

    fn cclw(&self) -> Self {
        match self {
            Dir::E => Dir::N,
            Dir::W => Dir::S,
            Dir::N => Dir::W,
            Dir::S => Dir::E,
        }
    }

    fn apply(&self, (x, y): Coord) -> Coord {
        match self {
            Dir::E => (x + 1, y),
            Dir::W => (x - 1, y),
            Dir::N => (x, y - 1),
            Dir::S => (x, y + 1),
        }
    }

    #[rustfmt::skip]
    fn rot(&self) -> Rot {
        let l: Rot = [
                [0, 0, -1],
                [0, 1, 0],
                [1, 0, 0]];
        let d: Rot = [
                [0, 1, 0],
                [-1,0, 0],
                [0, 0, 1]];
        match self {
            Dir::W => l,
            Dir::E => mul(l, mul(l, l)),
            Dir::S => mul(d, mul(d, d)),
            Dir::N => d
        }
    }

    #[rustfmt::skip]
    fn rot2(&self) -> Rot {
        let l: Rot = [
                [1, 0, 0],
                [0, 0, -1],
                [0, 1, 0]
        ];
        match self {
            Dir::E => l,
            Dir::W => mul(l, mul(l, l)),
            _ => panic!(),
        }
    }
}

type Board = Vec<Vec<u8>>;
type Coord = (usize, usize);
fn find_next(pos: Coord, dir: Dir, board: &Board) -> Option<Coord> {
    let mut n = dir.apply(pos);
    // Overflow!
    if board[n.1].len() < n.0 || board[n.1][n.0] == b' ' {
        n = match dir {
            Dir::E => (0, n.1),
            Dir::W => (board[n.1].len() - 1, n.1),
            Dir::N => (n.0, board.len() - 1),
            Dir::S => (n.0, 0),
        };

        while board[n.1].len() < n.0 || board[n.1][n.0] == b' ' {
            n = dir.apply(n);
        }
    }

    if board[n.1][n.0] == b'#' {
        None
    } else {
        Some(n)
    }
}

type Rot = [[isize; 3]; 3];
type P3 = (isize, isize, isize);
#[derive(Clone, Debug)]
struct Cube<const L: usize> {
    collisions: HashSet<P3>,
    all: HashMap<P3, (usize, usize)>,
    sides: Vec<(P3, Rot, (usize, usize))>,
}

type Frame = Vec<Vec<u8>>;

fn expand_coll<const L: usize>(
    coll: &mut HashSet<P3>,
    all: &mut HashMap<P3, (usize, usize)>,
    frame: &Frame,
    frame_l: (usize, usize),
    rot: Rot,
) -> Option<()> {
    let h = L as isize / 2;

    for ox in 0..L {
        for oy in 0..L {
            let is_thing = frame[oy][ox] == b'#';
            // Insert things
            let mut x = ox as isize - h;
            let mut y = oy as isize - h;
            if x >= 0 {
                x += 1;
            }
            if y >= 0 {
                y += 1;
            }
            let t = [[h + 1, -y, x]];
            let [[a, b, c]] = mul(rot, t);

            if is_thing {
                coll.insert((a, b, c));
            }

            all.insert((a, b, c), (frame_l.1 + ox, frame_l.0 + oy));
        }
    }
    Some(())
}

impl<const L: usize> Cube<L> {
    fn new(board: &Board) -> Self {
        let mut all = HashMap::new();
        let mut coll = HashSet::new();
        let row = [None, None, None, None];
        let mut frames: [[Option<Frame>; 4]; 4] =
            [row.clone(), row.clone(), row.clone(), row.clone()];
        let mut c_f = None;
        let mut sides = Vec::new();

        for i in 0..4 {
            let x = i * L;
            for j in 0..4 {
                let y = j * L;
                if x >= board.len() || y >= board[x].len() || board[x][y] == b' ' {
                    continue;
                }
                if c_f.is_none() {
                    c_f = (j + 1, i + 1).into();
                }

                let frame: Frame = (x..x + L).map(|i| board[i][y..y + L].to_vec()).collect();
                frames[i][j] = frame.into();
            }
        }

        let mut stack = Vec::new();
        stack.push((c_f.unwrap(), [[1, 0, 0], [0, 1, 0], [0, 0, 1]]));

        let mut done = HashSet::new();
        done.insert(c_f.unwrap());

        let h = L as isize / 2;
        while let Some((f, rot)) = stack.pop() {
            let t = [[h + 1, 0, 0]];
            let [[a, b, c]] = mul(rot, t);
            sides.push(((a, b, c), rot, f));

            expand_coll::<L>(
                &mut coll,
                &mut all,
                frames[f.1 - 1][f.0 - 1].as_ref().unwrap(),
                ((f.1 - 1) * L, (f.0 - 1) * L),
                rot.clone(),
            );

            for d in [Dir::E, Dir::W, Dir::S, Dir::N] {
                let n = d.apply(f);
                if n.0 == 0 || n.1 == 0 || n.0 == 5 || n.1 == 5 {
                    continue;
                }

                if !done.contains(&n) && frames[n.1 - 1][n.0 - 1].is_some() {
                    done.insert(n);
                    stack.push((n, mul(rot, d.rot())));
                }
            }
        }

        Self {
            collisions: coll,
            all,
            sides,
        }
    }

    fn get_output(&self, trans: Trans) -> usize {
        let (x, y) = self.all[&trans.pos];
        let (ox, oy) = self.all[&trans.old_pos];
        let (dx, dy) = (x as isize - ox as isize, y as isize - oy as isize);
        let extra = match (dx, dy) {
            (0, 1) => 1,
            (0, -1) => 3,
            (1, 0) => 0,
            (-1, 0) => 2,
            _ => panic!(),
        };
        return (y + 1) * 1000 + 4 * (x + 1) + extra;
    }

    #[allow(unused)]
    fn print2(&self, peep: P3, old: P3) {
        let mut field_row = Vec::new();
        field_row.resize(L * 4, b' ');
        let mut field = Vec::new();
        field.resize(L * 4, field_row);

        for (k, &(y, x)) in &self.all {
            if self.collisions.contains(k) {
                field[x][y] = b'*';
            } else if &peep == k {
                field[x][y] = b'@';
            } else if &old == k {
                field[x][y] = b'o';
            } else {
                field[x][y] = b'.';
            }
        }

        for line in field {
            let line_start = line.iter().take_while(|&x| *x == b' ').count();
            let line_end = line_start
                + line[line_start..]
                    .iter()
                    .take_while(|&x| *x != b' ')
                    .count();
            println!("{}", unsafe {
                std::str::from_utf8_unchecked(&line[..line_end])
            });
            if line.iter().all(|&f| f == b' ') {
                break;
            }
        }
    }

    #[allow(unused)]
    fn print(&self, peep: &P3, old: &P3) {
        println!("Printing pos {:?}", peep);
        let h = (L as isize) / 2;
        let empty = || {
            for r in -h..h + 1 {
                if r == 0 {
                    continue;
                }
                print!(" ");
            }
        };

        let print = |l, d: char| {
            if l == *peep {
                print!("\x1b[93m█\x1b[0m");
            } else if l == *old {
                print!("\x1b[93mo\x1b[0m");
            } else {
                if self.collisions.contains(&l) {
                    print!("#");
                } else {
                    print!("{}", d);
                }
            }
        };

        // TOP
        for r in -h..h + 1 {
            if r == 0 {
                continue;
            }
            empty();

            for c in -h..h + 1 {
                if c == 0 {
                    continue;
                }
                print((r, h + 1, c), 'T');
            }
            println!();
        }

        // FRONT
        for r in (-h..h + 1).rev() {
            if r == 0 {
                continue;
            }

            for c in -h..h + 1 {
                if c == 0 {
                    continue;
                }
                print((c, r, -h - 1), 'L');
            }

            for c in -h..h + 1 {
                if c == 0 {
                    continue;
                }
                print((h + 1, r, c), 'F');
            }

            for c in (-h..h + 1).rev() {
                if c == 0 {
                    continue;
                }
                print((c, r, h + 1), 'R');
            }
            println!();
        }

        // BOT
        for r in -h..h + 1 {
            if r == 0 {
                continue;
            }
            empty();

            for c in -h..h + 1 {
                if c == 0 {
                    continue;
                }
                print((-r, -h - 1, c), 'D');
            }
            println!();
        }

        // BACK
        for r in -h..h + 1 {
            if r == 0 {
                continue;
            }
            empty();

            for c in -h..h + 1 {
                if c == 0 {
                    continue;
                }
                print((-h - 1, r, c), 'B');
            }
            println!();
        }
    }
}

type DirTy = [[isize; 1]; 3];

#[derive(Copy, Clone, Debug)]
struct Trans {
    old_pos: P3,
    pos: P3,
    dir: DirTy,
    g_trans: [[isize; 3]; 3],
}

impl Trans {
    fn mov(&self) -> Self {
        let dir = mul(self.dir, self.g_trans);
        let mut tr = (
            self.pos.0 + dir[0][0],
            self.pos.1 + dir[1][0],
            self.pos.2 + dir[2][0],
        );

        if tr.0 == 0 || tr.1 == 0 || tr.2 == 0 {
            tr = (tr.0 + dir[0][0], tr.1 + dir[1][0], tr.2 + dir[2][0]);
        }

        Self {
            old_pos: self.pos,
            pos: tr,
            dir: self.dir,
            g_trans: self.g_trans,
        }
    }

    fn rot(&self, l: char) -> Self {
        let dir = match l {
            'R' => mul(self.dir, Dir::E.rot2()),
            'L' => mul(self.dir, Dir::W.rot2()),
            _ => panic!(),
        };
        Self {
            old_pos: self.old_pos,
            pos: self.pos,
            dir,
            g_trans: self.g_trans,
        }
    }

    fn apply(&self, dir: Dir) -> Self {
        let g_trans = mul(dir.rot(), self.g_trans);
        Self {
            old_pos: self.old_pos,
            pos: self.pos,
            dir: self.dir,
            g_trans,
        }
    }

    fn find_turn(&self, l: isize) -> Self {
        for d in [Dir::N, Dir::S, Dir::E, Dir::W] {
            let this = self.apply(d).mov();

            if this.valid_pos(l) {
                return this;
            }
        }
        todo!();
    }
    fn valid_pos(&self, l: isize) -> bool {
        [self.pos.0, self.pos.1, self.pos.2]
            .into_iter()
            .filter(|x| x.abs() >= l)
            .count()
            <= 1
    }
}

pub fn solve<const P1: bool, const P2: bool>(buf: impl BufRead) -> Option<()> {
    let mut lines: Vec<String> = buf.lines().flatten().collect();
    let mut movement = lines.pop()?.into_bytes();
    let moves: Vec<Result<usize, char>> = parse(&mut movement)?;

    if P1 {
        let mut board: Board = lines
            .iter()
            .map(|x| {
                let mut o = x.as_bytes().to_vec();
                o.insert(0, b' ');
                o.push(b' ');
                o
            })
            .collect();
        board.insert(0, Vec::new());

        let mut pos = (1, 1);
        let mut dir = Dir::default();
        pos = find_next(pos, dir, &board)?;

        for m in &moves {
            match m {
                Ok(i) => {
                    for _ in 0..*i {
                        if let Some(x) = find_next(pos, dir, &board) {
                            pos = x;
                        } else {
                            break;
                        }
                    }
                }
                Err(c) => match c {
                    'R' => dir = dir.clw(),
                    'L' => dir = dir.cclw(),
                    _ => panic!(),
                },
            }
        }

        println!(
            "Part 1: {:?} {:?} = {}",
            pos,
            dir,
            pos.1 * 1000 + 4 * pos.0 + dir.score()
        );
    }

    if P2 {
        const S: usize = 50;

        let board: Board = lines.iter().map(|x| x.as_bytes().to_vec()).collect();
        let cube = Cube::<S>::new(&board);

        let l = S as isize / 2 + 1;
        let mut trans = Trans {
            pos: (l, l - 1, -l + 1),
            old_pos: (l, l - 1, -l + 1),
            dir: [[0], [0], [1]],
            g_trans: [[1, 0, 0], [0, 1, 0], [0, 0, 1]],
        };

        for m in moves.into_iter() {
            match m {
                Ok(i) => {
                    for _ in 0..i {
                        let mut tr = trans.mov();

                        if !tr.valid_pos(l) {
                            tr = tr.find_turn(l);
                        }

                        if cube.collisions.contains(&tr.pos) {
                            break;
                        }
                        trans = tr;
                    }
                }
                Err(c) => {
                    trans = trans.rot(c);
                }
            }
        }

        println!("Part 2: {}", cube.get_output(trans));
    }
    Some(())
}
