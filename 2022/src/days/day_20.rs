use std::io::BufRead;

use crate::{parse, Char};

const C: &'static [&'static [i64]] = &[
    &[1, 2, -3, 3, -2, 0, 4],
    &[2, 1, -3, 3, -2, 0, 4],
    &[1, -3, 2, 3, -2, 0, 4],
    &[1, 2, 3, -2, -3, 0, 4],
    &[1, 2, -2, -3, 0, 3, 4],
    &[1, 2, -3, 0, 3, 4, -2],
    &[1, 2, -3, 0, 3, 4, -2],
    &[1, 2, -3, 4, 0, 3, -2],
];

pub fn solve<const P1: bool, const P2: bool>(mut buf: impl BufRead) -> Option<()> {
    let mut bytes = Vec::new();
    buf.read_to_end(&mut bytes).ok()?;
    let parsed: Vec<(i64, Char<b'\n'>)> = parse(&bytes)?;
    let mut parsed: Vec<_> = parsed
        .into_iter()
        .enumerate()
        .map(|(i, (x, _))| (x, i as i64))
        .collect();

    let index_arr: Vec<usize> = (0..parsed.len()).collect();

    let to_handle = parsed.len() as i64;
    let mut handled = 0;
    println!("parsed {:?}", parsed);

    let mut i = 0;
    let mut k = 1;
    while k > 0 {
        // Loop get index
        while parsed[i].1 != handled {
            i += 1;
            if i == parsed.len() {
                i = 0;
            }
        }

        handled += 1;

        let (v, z) = parsed[i];
        let mut x = ((i as i64 + v) % to_handle + to_handle) % to_handle;
        if v.signum() == x.signum() {
            x += x.signum();
        }
        parsed.insert(x as usize, (v, z));

        if x <= i as i64 {
            let v2 = parsed.remove(i + 1).0;
            debug_assert_eq!(v, v2);
        } else {
            let v2 = parsed.remove(i).0;
            debug_assert_eq!(v, v2);
        }

        if handled == to_handle {
            handled = 0;
            k -= 1;
        }
    }

    let zero_ids = parsed.iter().take_while(|x| x.0 != 0).count();
    println!("zero id {}", zero_ids);
    let x = parsed[(zero_ids + 1000) % parsed.len()].0;
    let y = parsed[(zero_ids + 2000) % parsed.len()].0;
    let z = parsed[(zero_ids + 3000) % parsed.len()].0;

    if P1 {
        // Too high 9756
        println!("Part 1: {} {} {} = {} (expectec 8372)", x, y, z, x + y + z);
    }
    if P2 {
        println!("Part 2");
    }
    Some(())
}
