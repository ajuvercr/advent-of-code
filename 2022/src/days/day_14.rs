use std::io::BufRead;

use crate::{parse, Parse};

type Point = (usize, usize);
type Field = Vec<Vec<u8>>;

const EMPTY: u8 = b'.';
const WALL: u8 = b'#';
const SAND: u8 = b'O';

#[derive(Debug, Clone)]
struct Line {
    line: Vec<Point>,
}

impl Line {
    fn write_line(&self, field: &mut Field) {
        for (&(sx, sy), &(ex, ey)) in self.line.iter().zip(self.line.iter().skip(1)) {
            field[sy][sx] = WALL;
            field[ey][ex] = WALL;
            if sy == ey {
                for x in sx..ex {
                    field[sy][x] = WALL;
                }
                for x in ex..sx {
                    field[sy][x] = WALL;
                }
            } else if sx == ex {
                for y in sy..ey {
                    field[y][sx] = WALL;
                }
                for y in ey..sy {
                    field[y][sx] = WALL;
                }
            } else {
                println!("Shouldn't happen");
            }
        }
    }
}

impl<'a> Parse<'a> for Line {
    fn parse(buf: &mut crate::Cursor) -> Option<Self> {
        let mut line = Vec::new();
        loop {
            let x = usize::parse(buf)?;
            buf.next();
            let y = usize::parse(buf)?;
            line.push((x, y));

            if *buf.next() == b' ' {
                buf.next_n(3);
            } else {
                break;
            }
        }
        Some(Self { line })
    }
}

fn align(parsed: &mut Vec<Line>) -> (Point, Point) {
    let (min_x, max_x, min_y, max_y) = parsed
        .iter()
        .flat_map(|y| y.line.iter())
        .copied()
        .fold((usize::MAX, 0, 0, 0), |(mix, max, miy, may), (x, y)| {
            (mix.min(x), max.max(x), miy.min(y), may.max(y))
        });

    parsed.iter_mut().for_each(|x| {
        x.line
            .iter_mut()
            .for_each(|x| *x = (x.0 - min_x, x.1 - min_y))
    });

    ((max_x - min_x, max_y - min_y), (500 - min_x, 0 - min_y))
}

fn calculate_next_send(path: &mut Vec<Point>, field: &Field) -> Option<Point> {
    loop {
        let (x, y) = path.last()?;
        // try down
        let row_i = y.wrapping_add(1);
        let row_left = x.wrapping_sub(1);
        let row_right = x.wrapping_add(1);
        let row_down = &field.get(row_i)?;
        if *row_down.get(*x)? == EMPTY {
            path.push((*x, row_i));
            continue;
        }
        if *row_down.get(row_left)? == EMPTY {
            path.push((row_left, row_i));
            continue;
        }
        if *row_down.get(row_right)? == EMPTY {
            path.push((row_right, row_i));
            continue;
        }
        return (*x, *y).into();
    }
}

fn print_field(field: &Vec<Vec<u8>>) {
    for line in field {
        println!("{}", unsafe { std::str::from_utf8_unchecked(line) });
    }
}

fn solve_input(mut parsed: Vec<Line>) -> usize {
    let ((max_x, max_y), start) = align(&mut parsed);

    let mut field_row = Vec::new();
    field_row.resize(max_x + 1, EMPTY);
    let mut field: Field = Vec::new();
    field.resize(max_y + 1, field_row);

    for line in &parsed {
        line.write_line(&mut field);
    }

    solve_count(&mut field, start)
}

fn solve_count(field: &mut Field, start: Point) -> usize {
    let mut count = 0;
    let mut path = vec![start];
    while let Some(x) = calculate_next_send(&mut path, &field) {
        field[x.1][x.0] = SAND;
        path.pop();
        count += 1;
    }
    count
}

pub fn solve<const P1: bool, const P2: bool>(mut buf: impl BufRead) -> Option<()> {
    let mut bytes = Vec::new();
    buf.read_to_end(&mut bytes).ok()?;
    let parsed: Vec<Line> = parse(bytes)?;
    let mut parsed2: Vec<Line> = parsed.clone();

    if P1 {
        println!("Part 1: {}", solve_input(parsed));
    }
    if P2 {
        let max_y = parsed2
            .iter()
            .flat_map(|x| x.line.iter())
            .map(|(_, y)| *y)
            .max()
            .unwrap()
            + 2;
        parsed2.push(Line {
            line: vec![(500 - max_y, max_y), (500 + max_y, max_y)],
        });
        println!("Part 2: {}", solve_input(parsed2));
    }
    Some(())
}
