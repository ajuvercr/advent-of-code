use std::{cmp::Ordering, io::BufRead};

use crate::{parse, Char, Parse, Puncuated, WS};

#[derive(Debug, Clone, PartialEq, Eq)]
enum Input {
    Numb(usize),
    Arr(Vec<Input>),
}
impl Input {
    fn single(input: Input) -> Self {
        Self::Arr(vec![input])
    }
    fn num(input: usize) -> Self {
        Self::Numb(input)
    }
}
impl Ord for Input {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Input::Numb(x), Input::Numb(y)) => x.cmp(&y),
            (Input::Arr(s1), Input::Arr(s2)) => {
                for (x, y) in s1.iter().zip(s2.iter()) {
                    let o = x.cmp(y);
                    if o != Ordering::Equal {
                        return o;
                    }
                }
                s1.len().cmp(&s2.len())
            }
            (Input::Numb(x), y) => Input::single(Input::num(*x)).cmp(y),
            (x, Input::Numb(y)) => x.cmp(&Input::single(Input::num(*y))),
        }
    }
}

impl PartialOrd for Input {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<'a> Parse<'a> for Input {
    fn parse(buf: &mut crate::Cursor) -> Option<Self> {
        if let Some(x) = usize::parse(buf) {
            Some(Self::Numb(x))
        } else {
            Char::<b'['>::parse(buf)?;
            let item: Puncuated<Input, Char<b','>> = Puncuated::parse(buf)?;
            Char::<b']'>::parse(buf)?;
            Some(Self::Arr(item.into_inner()))
        }
    }
}

#[derive(derive::Parse, Debug)]
struct Test {
    left: Input,
    _w1: WS,
    right: Input,
    _w2: WS,
}

impl Test {
    fn right_order(&self) -> bool {
        let cmped = self.left.cmp(&self.right);
        match cmped {
            Ordering::Greater => false,
            Ordering::Less => true,
            _ => panic!("Didn't expect equal ordering"),
        }
    }
}

pub fn solve<const P1: bool, const P2: bool>(mut buf: impl BufRead) -> Option<()> {
    let mut bytes = Vec::new();
    buf.read_to_end(&mut bytes).ok()?;
    let parsed: Vec<Test> = parse(bytes)?;
    if P1 {
        let count: usize = parsed
            .iter()
            .enumerate()
            .flat_map(|(i, x)| x.right_order().then_some(i + 1))
            .sum();
        println!("Part 1: {}", count);
    }
    if P2 {
        let mut all: Vec<Input> = parsed.into_iter().flat_map(|x| [x.left, x.right]).collect();
        let start = Input::single(Input::single(Input::num(2)));
        let end = Input::single(Input::single(Input::num(6)));
        all.push(start.clone());
        all.push(end.clone());
        all.sort();
        let start = all.iter().position(|x| x == &start).unwrap() + 1;
        let end = all.iter().position(|x| x == &end).unwrap() + 1;
        println!("Part 2: {}", start * end);
    }
    Some(())
}

#[cfg(test)]
mod tests {
    use crate::parse;

    use super::Test;

    fn test_str(st: &[u8]) -> bool {
        let x: Test = parse(st.to_vec()).unwrap();
        x.right_order()
    }

    #[test]
    fn test_1() {
        assert_eq!(test_str(b"[1,1,3,1,1] [1,1,5,1,1]"), true);
    }
    #[test]
    fn test_2() {
        assert_eq!(test_str(b"[[1],[2,3,4]] [[1],4]"), true);
    }
    #[test]
    fn test_3() {
        assert_eq!(test_str(b"[9] [[8,7,6]]"), false);
    }
    #[test]
    fn test_4() {
        assert_eq!(test_str(b"[] [3]"), true);
    }
    #[test]
    fn test_5() {
        assert_eq!(test_str(b"[[[]]] [[]]"), false);
    }
    #[test]
    fn test_6() {
        assert_eq!(
            test_str(b"[1,[2,[3,[4,[5,6,7]]]],8,9] [1,[2,[3,[4,[5,6,0]]]],8,9]"),
            false
        );
    }
}
