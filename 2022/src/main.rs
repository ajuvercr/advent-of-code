use std::io::stdin;

mod days;

pub fn main() {
    days::d03::solve(stdin().lock()).unwrap();
}
