use std::io::BufRead;

use crate::{parse, Char};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
struct Num {
    value: i128,
    jump: usize,
    order: usize,
}

impl Num {
    fn forward(v: i128, order: usize, count: i128) -> Self {
        let count = count - 1;
        let jump = if v < 0 {
            (((v - 1) % count) + count + 1) % count
        } else {
            v % count
        };

        Self {
            jump: jump as usize,
            order,
            value: v,
        }
    }
}

pub fn solve<const P1: bool, const P2: bool>(mut buf: impl BufRead) -> Option<()> {
    let mut bytes = Vec::new();
    buf.read_to_end(&mut bytes).ok()?;
    let parsed: Vec<(i128, Char<b'\n'>)> = parse(&bytes)?;
    let m = if P2 { 811589153 } else { 1 };
    let l = parsed.len() as i128;
    let mut parsed: Vec<_> = parsed
        .into_iter()
        .enumerate()
        .map(|(i, (x, _))| Num::forward(x * m, i, l))
        .collect();

    let to_handle = parsed.len();
    let mut handled = 0;

    let mut i = 0;
    let mut k = if P2 { 10 } else { 1 };
    while k > 0 {
        // Loop get index
        while parsed[i].order != handled {
            i += 1;
            if i == parsed.len() {
                i = 0;
            }
        }

        let v = parsed.remove(i);
        let x = (i + v.jump) % to_handle;
        if x < i {
            parsed.insert(x + 1, v);
        } else {
            parsed.insert(x, v);
        }

        handled += 1;

        if handled == to_handle {
            handled = 0;
            k -= 1;
        }
    }

    let zero_ids = parsed.iter().take_while(|x| x.value != 0).count();
    let x = parsed[(zero_ids + 1000) % parsed.len()].value;
    let y = parsed[(zero_ids + 2000) % parsed.len()].value;
    let z = parsed[(zero_ids + 3000) % parsed.len()].value;

    if P1 {
        println!("Part 1: {}", x + y + z);
    }
    if P2 {
        println!("Part 2: {}", x + y + z);
    }
    Some(())
}
