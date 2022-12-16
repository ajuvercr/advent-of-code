use std::{collections::HashMap, io::BufRead};

use crate::{parse, Parse};

trait DFS: Sized {
    type Option;
    type Res: PartialOrd;

    fn prune(&self, _: &Self::Option) -> bool {
        false
    }
    fn options(&self) -> (Vec<Self::Option>, Self::Res);
    fn handle(&mut self, option: Self::Option) -> Option<Self>;
}

fn dfs<T: DFS>(mut start: T, out: &mut Option<T::Res>) {
    let (options, r) = start.options();

    if let Some(x) = out.as_mut() {
        if &r > x {
            *x = r;
        }
    } else {
        *out = Some(r);
    }

    for option in options {
        if let Some(ne) = start.handle(option) {
            dfs(ne, out);
        }
    }
}

#[derive(Clone, Copy)]
struct Pipes<'a, const N: usize> {
    current: [usize; N],
    time: [isize; N],
    flow: isize,
    pipes: &'a [Pipe],
    opened: u64,
}

type Move = Neigh;
// #[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
// enum Move {
//     Move(Neigh),
//     Noop,
// }

#[derive(Debug, Clone, PartialEq, PartialOrd)]
struct Res {
    flow: isize,
    opened: Vec<Neigh>,
}

impl<'a, const N: usize> Pipes<'a, N> {
    fn apply(&self, mov: Neigh, i: usize) -> Self {
        let mut this = *self;

        // Move
        this.time[i] -= mov.dist;
        this.current[i] = mov.index;

        // Open
        if this.opened & (1 << mov.index) == 0 {
            this.time[i] -= 1;
            this.opened = this.opened | (1 << this.current[i]);
            this.flow = this.flow + this.time[i] * this.pipes[this.current[i]].rate;
        }

        this
    }

    fn get_moves(&self, i: usize) -> Vec<Move> {
        let current = self.current[i];
        let time = self.time[i];
        let this = &self.pipes[current];
        this.options
            .iter()
            .flat_map(|n| {
                if n.dist + 1 > time || self.opened & (1 << n.index) != 0 {
                    None
                } else {
                    Some(*n)
                    // Some(Move::Move(*n))
                }
            })
            .collect()
    }
}

impl<'a> DFS for Pipes<'a, 1> {
    type Option = Move;

    type Res = isize;

    fn options(&self) -> (Vec<Self::Option>, Self::Res) {
        (self.get_moves(0), self.flow)
    }

    fn handle(&mut self, option: Self::Option) -> Option<Self> {
        self.apply(option, 0).into()
        // if let Move::Move(m) = option {
        //     Some(self.apply(m, 0))
        // } else {
        //     None
        // }
    }
}

impl<'a> DFS for Pipes<'a, 2> {
    type Option = (Option<Move>, Option<Move>);

    type Res = isize;

    fn options(&self) -> (Vec<Self::Option>, Self::Res) {
        let a_moves = self.get_moves(0);
        let a_len = a_moves.len();
        let mut a_moves = a_moves.into_iter();

        let b_moves = self.get_moves(1);
        let b_len = b_moves.len();
        let mut b_moves = b_moves.into_iter();
        let mut out = Vec::with_capacity(a_len + b_len);

        let mut ma = a_moves.next();
        let mut mb = b_moves.next();
        loop {
            match (ma, mb) {
                (Some(a), Some(b)) if self.time[0] - a.dist < self.time[1] - b.dist => {
                    out.push((Some(a), Some(b)));
                    ma = a_moves.next();
                }
                (Some(a), Some(b)) => {
                    out.push((Some(a), Some(b)));
                    mb = b_moves.next();
                }
                (Some(a), None) => {
                    out.push((Some(a), None));
                    out.extend(a_moves.map(|x| (Some(x), None)));
                    break;
                }
                (None, Some(b)) => {
                    out.push((None, Some(b)));
                    out.extend(b_moves.map(|x| (None, Some(x))));
                    break;
                }
                _ => break,
            }
        }
        (out, self.flow)
    }

    fn handle(&mut self, option: Self::Option) -> Option<Self> {
        match option {
            (None, None) => None,
            (None, Some(b)) => Some(self.apply(b, 1)),
            (Some(a), None) => Some(self.apply(a, 0)),
            (Some(a), Some(b)) => Some(self.apply(a, 0).apply(b, 1)),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Default)]
struct Neigh {
    dist: isize,
    index: usize,
}

struct Pipe {
    rate: isize,
    options: Vec<Neigh>,
}

#[derive(Debug)]
struct P<'a>(&'a [u8], isize, Vec<&'a [u8]>);
impl<'a> Parse<'a> for P<'a> {
    fn parse(buf: &mut crate::Cursor<'a>) -> Option<Self> {
        buf.next_n("Valve ".len());
        let ident = buf.next_n(2);
        buf.next_n(" has flow rate=".len());
        let flow = isize::parse(buf)?;
        buf.next_n("; tunnels lead to valve".len());
        let n = buf.next();
        if *n == b's' {
            buf.next();
        }

        let mut neigh = Vec::new();
        loop {
            neigh.push(buf.next_n(2));
            let n = buf.next();
            if *n != b',' {
                break;
            }
            buf.next();
        }

        Some(P(ident, flow, neigh))
    }
}

fn floyd_warshall(edges: &Vec<Vec<usize>>) -> Vec<Vec<isize>> {
    let mut row = Vec::new();
    row.resize(edges.len(), 100000000000);
    let mut out = Vec::new();
    out.resize(edges.len(), row);
    for i in 0..edges.len() {
        out[i][i] = 0;
    }
    for (i, pip) in edges.iter().enumerate() {
        for o in pip {
            out[i][*o] = 1;
        }
    }

    for k in 0..edges.len() {
        for i in 0..edges.len() {
            for j in 0..edges.len() {
                out[i][j] = out[i][j].min(out[i][k] + out[k][j]);
            }
        }
    }

    out
}

pub fn solve<const P1: bool, const P2: bool>(mut buf: impl BufRead) -> Option<()> {
    let mut map: HashMap<&[u8], usize> = HashMap::new();

    let mut bytes = Vec::new();
    buf.read_to_end(&mut bytes).ok()?;
    let parsed: Vec<P> = parse(&bytes)?;

    parsed.iter().enumerate().for_each(|(i, x)| {
        map.insert(x.0, i);
    });
    let pipes: Vec<_> = parsed
        .iter()
        .map(|P(_, _, children)| children.into_iter().map(|id| map[id]).collect::<Vec<_>>())
        .collect();

    let dists = floyd_warshall(&pipes);

    let pipes: Vec<_> = parsed
        .iter()
        .map(|P(ident, flow, _)| {
            let this = map[ident];
            let options = parsed
                .iter()
                .flat_map(|P(n, f, _)| {
                    if *f == 0 || ident == n {
                        None
                    } else {
                        let idx = map[n];
                        Some(Neigh {
                            index: idx,
                            dist: dists[this][idx],
                        })
                    }
                })
                .collect();

            Pipe {
                rate: *flow,
                options,
            }
        })
        .collect();

    {
        let current = "AA";
        let start = map[current.as_bytes()];

        if P1 {
            let time = 30;
            let pipe = Pipes {
                time: [time],
                flow: 0,
                current: [start],
                pipes: &pipes,
                opened: 0,
            };

            let mut res = None;
            dfs(pipe, &mut res);
            println!("Part 1: {:?}", res.unwrap());
        }
        if P2 {
            let time = 26;
            let pipe = Pipes {
                time: [time, time],
                flow: 0,
                current: [start, start],
                pipes: &pipes,
                opened: 0,
            };

            let mut res = None;
            dfs(pipe, &mut res);
            println!("Part 2: {:?}", res.unwrap());
        }
    }
    Some(())
}
