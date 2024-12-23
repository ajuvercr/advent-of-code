use std::fs::read;

use days::day23::{part1, part2};

mod days;

fn main() {
    let c = read("../input/23.txt").unwrap();
    let st = std::str::from_utf8(c.as_ref()).unwrap();
    println!("Part1 {}", part1(st));
    println!("Part2 {}", part2(st));
    // println!("Part2 {}", part2(st));
}
