use std::io::BufRead;

pub fn solve<const PART1: bool, const PART2: bool>(input: impl BufRead) -> Option<()> {
    let mut lines = input.lines();

    let mut max = c!(u32::default, PART1);

    let mut m1 = c!(u32::default, PART1);
    let mut m2 = c!(u32::default, PART1);
    let mut m3 = c!(u32::default, PART1);

    let mut do1 = |c: u32| {
        if c > max {
            max = c;
        }
    };

    let mut do2 = |c: u32| {
        if c > m1 {
            m3 = m2;
            m2 = m1;
            m1 = c;
        } else if c > m2 {
            m3 = m2;
            m2 = c;
        } else if c > m3 {
            m3 = c;
        }
    };

    let mut current = 0;
    while let Some(Ok(line)) = lines.next() {
        if let Ok(x) = line.parse::<u32>() {
            current += x;
        } else {
            if PART1 {
                do1(current);
            }

            if PART2 {
                do2(current);
            }

            current = 0;
        }
    }

    if PART1 {
        do1(current);
        println!("part1 {}", max);
    }

    if PART2 {
        do2(current);
        println!("part2 {}", m1 + m2 + m3);
    }

    Some(())
}
