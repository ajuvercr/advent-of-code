const st: &'static str = include_str!("../../myfile.txt");
const bytes_ref: &'static [u8] = include_bytes!("../../myfile.txt");

use aoc_2022::*;

pub fn main() {
    let bytes = bytes_ref.to_vec();
    let start = std::time::Instant::now();
    let punc: Puncuated<u64, Char<b','>> = aoc_2022::parse(bytes).unwrap();
    let elaps = start.elapsed();
    println!("time used {:?} {}", elaps, punc.into_inner().len());

    let start = std::time::Instant::now();
    let parsed: Vec<u64> = st.split(',').flat_map(|x| x.parse()).collect();
    let elaps = start.elapsed();
    println!("time used {:?} {}", elaps, parsed.len());
}
