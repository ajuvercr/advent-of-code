use std::collections::hash_map::{self, HashMap};
use std::fs;

type Loc = (isize, isize);

fn fold_simple(
    (p, mut m): (Loc, HashMap<Loc, usize>),
    (_, v): (usize, char),
) -> (Loc, HashMap<Loc, usize>) {
    let pn = match v {
        'v' => (p.0, p.1 - 1),
        '^' => (p.0, p.1 + 1),
        '>' => (p.0 + 1, p.1),
        '<' => (p.0 - 1, p.1),
        _ => {
            eprintln!("Unknown character '{}'", v);
            return (p, m);
        }
    };
    if let Some(v) = m.get_mut(&pn) {
        *v += 1;
    } else {
        m.insert(pn, 1);
    }

    (pn, m)
}

fn fold_double(
    ((p1, p2), m): ((Loc, Loc), HashMap<Loc, usize>),
    (i, v): (usize, char),
) -> ((Loc, Loc), HashMap<Loc, usize>) {
    if i % 2 == 0 {
        let (p1, m) = fold_simple((p1, m), (i, v));
        ((p1, p2), m)
    } else {
        let (p2, m) = fold_simple((p2, m), (i, v));
        ((p1, p2), m)
    }
}

fn count_houses<B, F>(i: &str, init: B, f: F) -> usize
where
    F: Fn((B, HashMap<Loc, usize>), (usize, char)) -> (B, HashMap<Loc, usize>),
{
    let map = {
        let mut inner = hash_map::HashMap::new();
        inner.insert((0, 0), 1);
        inner
    };

    let init = (init, map);
    let (_, map) = i.chars().enumerate().fold::<(_, _), _>(init, f);
    map.len()
}

pub fn solve(file: &str) {
    let inp = fs::read_to_string(file).unwrap();

    println!("part 1: {}", count_houses(&inp, (0, 0), fold_simple));
    println!(
        "part 2: {}",
        count_houses(&inp, ((0, 0), (0, 0)), fold_double)
    );
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn simple() {
        let t = count_houses(">", (0, 0), fold_simple);
        assert_eq!(t, 2);

        let dt = count_houses("^v", ((0, 0), (0, 0)), fold_double);
        assert_eq!(dt, 3);
    }
    #[test]
    fn square() {
        let t = count_houses("^>v<", (0, 0), fold_simple);
        assert_eq!(t, 4);
        let dt = count_houses("^>v<", ((0, 0), (0, 0)), fold_double);
        assert_eq!(dt, 3);
    }
    #[test]
    fn simple_happy() {
        let t = count_houses("^v^v^v^v^v", (0, 0), fold_simple);
        assert_eq!(t, 2);
        let dt = count_houses("^v^v^v^v^v", ((0, 0), (0, 0)), fold_double);
        assert_eq!(dt, 11);
    }
}
