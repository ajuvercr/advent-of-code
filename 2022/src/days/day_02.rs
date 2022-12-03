use std::io::BufRead;

#[cfg(test)]
mod tests {
    use super::*;

    const ROCK: Action = 0;
    const PAPER: Action = 1;
    const SCISSORS: Action = 2;

    #[test]
    fn rock_defeats_scissors() {
        assert_eq!(wins(ROCK, SCISSORS), 6);
        assert_eq!(wins(ROCK, ROCK), 3);
        assert_eq!(wins(SCISSORS, ROCK), 0);
    }

    #[test]
    fn required_to_win() {
        assert_eq!(req_to_win(ROCK, 0), SCISSORS);
        assert_eq!(req_to_win(ROCK, 1), ROCK);
        assert_eq!(req_to_win(ROCK, 2), PAPER);
    }
}

type Action = i32;

fn wins(a: Action, b: Action) -> u32 {
    if a == b {
        return 3;
    }

    // Cycle one
    if (b + 1) % 3 == a {
        return 6;
    }

    return 0;
}

fn req_to_win(a: Action, b: Action) -> Action {
    // Cycle back
    (a + b - 1 + 3) % 3
}

fn action_points(a: Action) -> u32 {
    (a + 1) as u32
}

fn from_char(x: char) -> Action {
    match x {
        'A' | 'X' => 0,
        'B' | 'Y' => 1,
        'C' | 'Z' => 2,
        _ => panic!(),
    }
}

pub fn solve<const P1: bool, const P2: bool>(buf: impl BufRead) -> Option<()> {
    let mut lines = buf.lines();
    let mut total = c!(u32::default, P1);
    let mut total2 = c!(u32::default, P2);

    while let Some(Ok(l)) = lines.next() {
        let mut chs = l.chars();
        let oponent: Action = from_char(chs.next()?);
        chs.next();
        let me: Action = from_char(chs.next()?);

        if P1 {
            total += action_points(me) + wins(me, oponent);
        }

        if P2 {
            let me = req_to_win(oponent, me);
            total2 += action_points(me) + wins(me, oponent);
        }
    }

    if P1 {
        println!("Part 1 {}", total);
    }

    if P2 {
        println!("Part 2 {}", total2);
    }

    Some(())
}
