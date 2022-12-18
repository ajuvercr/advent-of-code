use std::io::BufRead;
type Point = (usize, usize);

#[derive(Debug, Clone, Copy)]
struct Piece {
    origin: Point,
    shape: &'static [Point],
    width: usize,
    height: usize,
}

const PIECES: &[Piece] = &[
    Piece {
        //  Horizontal line
        origin: (2, 0),
        shape: &[(0, 0), (1, 0), (2, 0), (3, 0)],
        width: 4,
        height: 1,
    },
    Piece {
        // Cross
        origin: (2, 0),
        shape: &[(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)],
        width: 3,
        height: 3,
    },
    Piece {
        // L  shape
        origin: (2, 0),
        shape: &[(0, 2), (1, 2), (2, 0), (2, 1), (2, 2)],
        width: 3,
        height: 3,
    },
    Piece {
        // Vertical line
        origin: (2, 0),
        shape: &[(0, 0), (0, 1), (0, 2), (0, 3)],
        width: 1,
        height: 4,
    },
    Piece {
        // Box shape
        origin: (2, 0),
        shape: &[(0, 0), (1, 0), (0, 1), (1, 1)],
        width: 2,
        height: 2,
    },
];

#[derive(Default, Debug, Clone)]
struct Board {
    field: Vec<[bool; 7]>,
    locations: Vec<Point>,
}

impl Board {
    fn set_piece(&mut self, piece: &Piece) {
        let l = self.field.len();
        for s in piece.shape {
            let (x, y) = (piece.origin.0 + s.0, piece.origin.1 + s.1);
            self.field[l - 1 - y][6 - x] = true;
        }
        self.locations.push(piece.origin);
    }

    fn collided(&self, piece: &Piece) -> bool {
        let l = self.field.len();
        for s in piece.shape {
            let (x, y) = (piece.origin.0 + s.0, piece.origin.1 + s.1);
            if self.field[l - 1 - y][6 - x] {
                return true;
            }
        }
        false
    }

    fn blank_lines(&self) -> usize {
        let mut out = 0;
        for line in self.field.iter().rev() {
            if line.iter().any(|&x| x) {
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
                self.field.push([false; 7]);
            }
        } else {
            for _ in 0..(self.blank_lines() - 3 - piece.height) {
                self.field.pop();
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

fn find_repeat<State, Res: Copy>(
    state: &mut State,
    mut once: impl FnMut(&mut State) -> Res,
    check: impl Fn(&State, &Res, &Res, &Res) -> bool,
) -> (Res, Res, Res) {
    let mut reses = Vec::new();
    reses.push(once(state));
    reses.push(once(state));
    reses.push(once(state));
    let mut tr = 1;

    while !check(
        state,
        &reses[tr - 1],
        &reses[tr * 2 - 1],
        &reses[tr * 3 - 1],
    ) {
        reses.push(once(state));
        reses.push(once(state));
        reses.push(once(state));
        tr += 1;
    }

    (reses[tr - 1], reses[tr * 2 - 1], reses[tr * 3 - 1])
}

type D = (usize, usize);
pub fn solve<const P1: bool, const P2: bool>(mut buf: impl BufRead) -> Option<()> {
    let mut bytes = Vec::new();
    buf.read_to_end(&mut bytes).ok()?;
    bytes.pop();

    let mut wind = bytes.iter().cycle();
    let mut pieces = PIECES.iter().cycle();
    let mut board = Board::default();

    let modulus = PIECES.len() as u128;

    let one = |board: &mut Board| {
        for _ in 0..modulus {
            board.fall_next(&mut pieces, &mut wind);
        }
        (board.locations.len(), board.height())
    };

    let check = |board: &Board, &(start, _): &D, &(mid, _): &D, &(end, _): &D| {
        &board.locations[start..mid] == &board.locations[mid..end]
    };

    let (start, mid, end) = find_repeat(&mut board, one, check);
    let rows_per_repeat = (mid.1 - start.1) as u128;
    let count_per_repeat = (mid.0 - start.0) as u128;

    let count_for = |target: u128| {
        let mut board = board.clone();
        let mut pieces = pieces.clone();
        let mut wind = wind.clone();

        let todo = target - start.0 as u128;
        let times = todo / count_per_repeat;
        let ending_count = todo - times * count_per_repeat;
        let middle = rows_per_repeat * times;
        for _ in 0..ending_count {
            board.fall_next(&mut pieces, &mut wind);
        }
        start.1 as u128 + middle + (board.height() - end.1) as u128
    };

    if P1 {
        let part1 = count_for(2022u128);
        println!("Part 1: {:?}", part1);
    }
    if P2 {
        let part2 = count_for(1000000000000u128);
        println!("Part 2: {}", part2);
    }
    Some(())
}
