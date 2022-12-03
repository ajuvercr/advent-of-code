use std::io::{stdin, BufRead};

pub fn main() {
    let mut lines = stdin().lock().lines();

    let mut m1 = 0;
    let mut m2 = 0;
    let mut m3 = 0;

    let mut current = 0;
    while let Some(Ok(line)) = lines.next() {
        if let Ok(x) = line.parse::<u32>() {
            current += x;
        } else {
            if current > m1 {
                m3 = m2;
                m2 = m1;
                m1 = current;
            } else if current > m2 {
                m3 = m2;
                m2 = current;
            } else if current > m3 {
                m3 = current;
            }

            current = 0;
        }
    }

    if current > m1 {
        m1 = current;
    } else if current > m2 {
        m2 = current;
    } else if current > m3 {
        m3 = current;
    }

    println!("{} ", m1 + m2 + m3);
}
