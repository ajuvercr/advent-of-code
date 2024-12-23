use std::{
    collections::{HashMap, HashSet},
    isize,
};

fn mix(secret: isize, value: isize) -> isize {
    secret ^ value
}

fn prune(secret: isize) -> isize {
    secret % 16777216
}

fn step1(secret: isize) -> isize {
    prune(mix(secret, secret * 64))
}
fn step2(secret: isize) -> isize {
    prune(mix(secret, secret / 32))
}

fn step3(secret: isize) -> isize {
    prune(mix(secret, secret * 2048))
}

fn next(secret: isize) -> isize {
    step3(step2(step1(secret)))
}

fn part1_next(mut secret: isize) -> isize {
    for _ in 0..2000 {
        secret = next(secret);
    }
    secret
}

fn part2_next<'a>(
    mut secret: isize,
    buf: &'a mut Vec<isize>,
    map: &mut HashMap<[isize; 4], isize>,
    keys: &mut HashSet<[isize; 4]>,
) {
    let mut last = 0;
    for _ in 0..2001 {
        secret = next(secret);

        let v = secret % 10;
        let diff = v - last;
        last = v;

        buf.push(diff);
        let l = buf.len();
        if l > 3 {
            let key = [buf[l - 4], buf[l - 3], buf[l - 2], buf[l - 1]];
            keys.insert(key);
            let e = map.entry(key);
            e.or_insert(v);
        }
    }
}

#[allow(unused)]
pub fn do_it(input: &str) -> isize {
    let mut part1 = 0;
    for n in input.split('\n').flat_map(|x| isize::from_str_radix(x, 10)) {
        part1 += part1_next(n)
    }
    part1
}

#[allow(unused)]
pub fn part2(input: &str) -> isize {
    let mut keys = HashSet::new();
    let mut maps = Vec::new();
    let mut buf = Vec::new();
    for secret in input.split('\n').flat_map(|x| isize::from_str_radix(x, 10)) {
        buf.clear();
        let mut map = HashMap::new();
        part2_next(secret, &mut buf, &mut map, &mut keys);
        maps.push(map);
    }

    let mut most = 0;
    println!("{} Keys", keys.len());
    for key in keys {
        let mut test = 0;
        for map in &maps {
            let v = map.get(&key).copied().unwrap_or(0);
            test += v;
        }
        if test > most {
            most = test;
        }
    }

    most
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_mix() {
        assert_eq!(mix(42, 15), 37);
    }

    #[test]
    fn test_prune() {
        assert_eq!(prune(100000000), 16113920);
    }

    #[test]
    fn test_step() {
        let mut secret = 123;
        secret = next(secret);
        assert_eq!(secret, 15887950);
        secret = next(secret);
        assert_eq!(secret, 16495136);
        secret = next(secret);
        assert_eq!(secret, 527345);
        secret = next(secret);
        assert_eq!(secret, 704524);
    }

    #[test]
    fn test_example() {
        let part1 = do_it(
            "1
10
100
2024",
        );
        assert_eq!(part1, 37327623);
    }

    #[test]
    fn test_example_2() {
        let part1 = part2(
            "1
2
3
2024",
        );
        assert_eq!(part1, 23);
    }
}
