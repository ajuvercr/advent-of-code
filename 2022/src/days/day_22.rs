use std::{collections::HashSet, io::BufRead};

use crate::parse;

type T = isize;
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

        println!("p2 {:?}", p2);
        assert_eq!(p2, [[0], [5]]);
    }

    #[test]
    fn test2() {
        let rot = [[0, 1], [1, 0]];
        let p = [[1, 0], [0, 1]];
        let p2 = mul(p, rot);

        println!("p2 {:?}", p2);
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
            Dir::E => l,
            Dir::W => mul(l, mul(l, l)),
            Dir::N => mul(d, mul(d, d)),
            Dir::S => d
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
struct Cube {
    collisions: HashSet<P3>,
}

type Frame = Vec<Vec<u8>>;

fn expand_coll<const L: usize>(coll: &mut HashSet<P3>, frame: &Frame, rot: Rot) -> Option<()> {
    let h = L as isize / 2;

    for x in 0..L {
        for y in 0..L {
            // Insert things
            if frame[y][x] == b'#' {
                let mut x = x as isize - h;
                let mut y = y as isize - h;
                if x >= 0 {
                    x += 1;
                }
                if y >= 0 {
                    y += 1;
                }
                let t = [[h + 1, y, x]];
                let [[a, b, c]] = mul(rot, t);
                coll.insert((a, b, c));
            }
        }
    }
    Some(())
}

impl Cube {
    fn new<const L: usize>(board: &Board) -> Self {
        let mut coll = HashSet::new();
        let row = [None, None, None, None];
        let mut frames: [[Option<Frame>; 4]; 4] =
            [row.clone(), row.clone(), row.clone(), row.clone()];
        let mut c_f = None;

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

        let mut i = 6;
        while let Some((f, rot)) = stack.pop() {
            if i == 0 {
                break;
            }
            i -= 1;
            println!("frame {:?}", frames[f.1 - 1][f.0 - 1]);
            expand_coll::<L>(
                &mut coll,
                frames[f.1 - 1][f.0 - 1].as_ref().unwrap(),
                rot.clone(),
            );

            for d in [Dir::E, Dir::W, Dir::S, Dir::N] {
                let n = d.apply(f);
                if n.0 == 0 || n.1 == 0 || n.0 == 4 || n.1 == 4 {
                    continue;
                }

                if !done.contains(&n) && frames[n.1 - 1][n.0 - 1].is_some() {
                    done.insert(n);
                    stack.push((n, mul(rot, d.rot())));
                }
            }
        }

        Self { collisions: coll }
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

        for m in moves {
            match m {
                Ok(i) => {
                    // println!("handle move {} (dir {:?})", i, dir);
                    for _ in 0..i {
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
        let board: Board = lines.iter().map(|x| x.as_bytes().to_vec()).collect();
        let cube = Cube::new::<4>(&board);
        println!("cube {:?}", cube);
        println!("Part 2");
    }
    Some(())
}
