use std::fs;
pub fn solve(filename: &str) {
    let contents = fs::read_to_string(filename).expect("Something went wrong reading the file");
    let ds: Vec<isize> = contents
        .chars()
        .filter_map(|x| match x {
            '(' => Some(1),
            ')' => Some(-1),
            _ => None,
        })
        .collect();

    let part2 = ds
        .iter()
        .scan(0, |x, y| {
            *x += *y;
            Some(*x)
        })
        .take_while(|x| *x >= 0)
        .count();

    println!("part 1: {}", ds.iter().sum::<isize>());
    println!("part 2: {}", part2 + 1);
}
