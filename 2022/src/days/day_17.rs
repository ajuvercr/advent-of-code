use std::io::BufRead;
type Point = (usize, usize);

#[derive(Debug, Clone, Copy)]
struct Piece {
    origin: Point,
    shape: &'static [Point],
    width: usize,
    height: usize,
    p: P,
}

#[derive(Debug, Clone, Copy)]
enum P {
    H,
    V,
    C,
    L,
    B,
}
impl P {
    fn ch(&self) -> char {
        match self {
            P::H => '-',
            P::V => '|',
            P::C => '+',
            P::L => 'L',
            P::B => 'B',
        }
    }
}

impl PartialEq for P {
    fn eq(&self, _: &Self) -> bool {
        true
    }
}
impl Eq for P {}

const PIECES: &[Piece] = &[
    Piece {
        //  Horizontal line
        origin: (2, 0),
        shape: &[(0, 0), (1, 0), (2, 0), (3, 0)],
        width: 4,
        height: 1,
        p: P::H,
    },
    Piece {
        // Cross
        origin: (2, 0),
        shape: &[(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)],
        width: 3,
        height: 3,
        p: P::C,
    },
    Piece {
        // L  shape
        origin: (2, 0),
        shape: &[(0, 2), (1, 2), (2, 0), (2, 1), (2, 2)],
        width: 3,
        height: 3,
        p: P::L,
    },
    Piece {
        // Vertical line
        origin: (2, 0),
        shape: &[(0, 0), (0, 1), (0, 2), (0, 3)],
        width: 1,
        height: 4,
        p: P::V,
    },
    Piece {
        // Box shape
        origin: (2, 0),
        shape: &[(0, 0), (1, 0), (0, 1), (1, 1)],
        width: 2,
        height: 2,
        p: P::B,
    },
];

#[derive(Default, Debug)]
struct Board {
    field: Vec<[Option<P>; 7]>,
}
impl Board {
    fn set_piece(&mut self, piece: &Piece) {
        let l = self.field.len();
        for s in piece.shape {
            let (x, y) = (piece.origin.0 + s.0, piece.origin.1 + s.1);
            self.field[l - 1 - y][6 - x] = Some(piece.p);
        }
    }

    fn collided(&self, piece: &Piece) -> bool {
        let l = self.field.len();
        for s in piece.shape {
            let (x, y) = (piece.origin.0 + s.0, piece.origin.1 + s.1);
            if self.field[l - 1 - y][6 - x].is_some() {
                return true;
            }
        }
        false
    }

    fn blank_lines(&self) -> usize {
        let mut out = 0;
        for line in self.field.iter().rev() {
            if line.iter().any(|&x| x.is_some()) {
                break;
            }
            out += 1;
        }

        out
    }

    fn handle_wind(&self, piece: &mut Piece, wind: u8) {
        match wind {
            b'>' => {
                if piece.origin.0 + piece.width < 7 {
                    piece.origin.0 += 1;
                    //  Check if it collided
                    if self.collided(piece) {
                        piece.origin.0 -= 1;
                    }
                }
            }
            b'<' => {
                if piece.origin.0 > 0 {
                    piece.origin.0 -= 1;
                    if self.collided(piece) {
                        piece.origin.0 += 1;
                    }
                }
            }
            _ => panic!("Unknown Winddd"),
        }
    }

    fn handle_fall(&mut self, piece: &mut Piece) -> Option<()> {
        piece.origin.1 += 1;
        if piece.origin.1 == self.field.len() || self.collided(piece) {
            piece.origin.1 -= 1;
            self.set_piece(piece);
            return None;
        }
        Some(())
    }

    fn fall_next<'a, 'b>(
        &mut self,
        pieces: &mut impl Iterator<Item = &'a Piece>,
        winds: &mut impl Iterator<Item = &'b u8>,
    ) -> Option<()> {
        let mut piece = *pieces.next()?;
        if self.blank_lines() < piece.height + 3 {
            for _ in 0..(piece.height + 3) - self.blank_lines() {
                self.field.push([None; 7]);
            }
        } else {
            for _ in 0..self.blank_lines() - 3 - piece.height {
                self.handle_fall(&mut piece);
            }
        }
        loop {
            let wind = *winds.next()?;
            self.handle_wind(&mut piece, wind);
            if self.handle_fall(&mut piece).is_none() {
                break;
            }
        }
        Some(())
    }

    fn height(&self) -> usize {
        self.field.len() - self.blank_lines()
    }
}

fn print_board(board: &[[Option<P>; 7]]) {
    for line in board.iter().rev() {
        let line: String = line
            .iter()
            .map(|&x| x.map(|x| x.ch()).unwrap_or('.'))
            .collect();
        println!("{}", line);
    }
}

fn find_repeat<State, Res>(
    state: &mut State,
    mut once: impl FnMut(&mut State) -> Res,
    check: impl Fn(&State, &Res, &Res) -> bool,
) -> usize {
    let mut reses = Vec::new();
    reses.push(once(state));
    reses.push(once(state));
    let mut tr = 1;

    while !check(state, &reses[tr - 1], &reses[tr * 2 - 1]) {
        println!("Trying {}", tr);
        reses.push(once(state));
        reses.push(once(state));
        tr += 1;
    }

    tr
}

fn check_eq<T: Eq>(a: &[T], b: &[T]) -> bool {
    if a.len() != b.len() {
        return false;
    }
    for i in 10..a.len().min(5000) {
        if &a[i] != &b[i] {
            return false;
        }
    }

    return true;
}

pub fn solve<const P1: bool, const P2: bool>(mut buf: impl BufRead) -> Option<()> {
    let mut bytes = Vec::new();
    buf.read_to_end(&mut bytes).ok()?;
    bytes.pop();

    let mut wind = bytes.iter().cycle();
    let mut pieces = PIECES.iter().cycle();
    let mut board = Board::default();

    let modulus = (PIECES.len() * bytes.len()) as u128;
    for _ in 0..modulus {
        board.fall_next(&mut pieces, &mut wind);
    }
    let start = board.height();

    let one = |board: &mut Board| {
        for _ in 0..modulus {
            board.fall_next(&mut pieces, &mut wind);
        }
        board.height()
    };

    let check = |board: &Board, &mid: &usize, &end: &usize| {
        check_eq(&board.field[start..mid], &board.field[mid..end])
    };

    let repeat = find_repeat(&mut board, one, check) as u128;
    let start = start as u128;
    let end = board.height() as u128;

    let rows_per_repeat = (end - start) / 2;
    let count_per_repeat = repeat * modulus;

    let total = 1000000000000u128;
    let times = total / count_per_repeat;
    let ending_count = total - times * count_per_repeat;
    let middle = rows_per_repeat * times;

    for _ in 0..ending_count {
        board.fall_next(&mut pieces, &mut wind);
    }

    if P1 {
        println!("Part 1: {} {}", start, middle);
    }
    if P2 {
        let part2 = start + middle + (board.height() as u128 - end);
        println!(
            "Part 2: {} (expected 1514285714288) (diff {})",
            part2,
            1514285714288u128.abs_diff(part2)
        );
    }
    Some(())
}
