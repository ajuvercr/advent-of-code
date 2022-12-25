use std::{collections::VecDeque, io::BufRead};

#[cfg(test)]
mod tests {
    use crate::days::day_25::{from_snafu, to_snafu};
    #[test]
    fn test_from_snafu_simple() {
        assert_eq!(from_snafu(b"1"), 1);
        assert_eq!(from_snafu(b"2"), 2);
        assert_eq!(from_snafu(b"1="), 3);
        assert_eq!(from_snafu(b"1-"), 4);
    }
    #[test]
    fn test_to_snafu_simple() {
        assert_eq!(&to_snafu(1), "1");
        assert_eq!(&to_snafu(2), "2");
        assert_eq!(&to_snafu(3), "1=");
        assert_eq!(&to_snafu(4), "1-");
        assert_eq!(&to_snafu(5), "10");
    }

    #[test]
    fn test_to_snafu() {
        assert_eq!(to_snafu(2022), "1=11-2");
        assert_eq!(to_snafu(314159265), "1121-1110-1=0");
    }

    #[test]
    fn test_from_snafu() {
        assert_eq!(from_snafu(b"1=-0-2"), 1747);
        assert_eq!(from_snafu(b"12111"), 906);
    }
}

fn from_snafu(nun: &[u8]) -> i64 {
    let mut out = 0;

    for i in 0..nun.len() {
        let b = nun[i];
        let b = match b {
            b'2' => 2,
            b'1' => 1,
            b'0' => 0,
            b'-' => -1,
            b'=' => -2,
            x => panic!("didn't expect {}", x),
        };
        out = out * 5 + b;
    }

    out
}

fn to_snafu(mut num: i64) -> String {
    let mut base_5 = VecDeque::new();

    while num > 0 {
        base_5.push_front(num % 5);
        num = num / 5;
    }

    let mut carry = 0;
    let mut i = 0;

    while i < base_5.len() {
        let k = base_5.len() - 1 - i;
        i += 1;
        let n = base_5[k] + carry;
        let new_n = if n > 2 {
            carry = 1;
            n - 5
        } else {
            carry = 0;
            n
        };
        base_5[k] = new_n;
    }

    if carry > 0 {
        base_5.push_front(carry);
    }

    base_5
        .into_iter()
        .map(|x| match x {
            -1 => '-',
            -2 => '=',
            0 => '0',
            1 => '1',
            2 => '2',
            x => panic!("Didn't expect {}", x),
        })
        .collect()
}

pub fn solve<const P1: bool, const P2: bool>(buf: impl BufRead) -> Option<()> {
    let num: i64 = buf.split(b'\n').flatten().map(|x| from_snafu(&x)).sum();
    if P1 {
        println!("Part 1: {}", to_snafu(num));
    }
    if P2 {
        println!("Part 2");
    }
    Some(())
}
