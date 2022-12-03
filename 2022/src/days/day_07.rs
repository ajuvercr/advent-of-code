use std::io::BufRead;

pub fn solve<const P1: bool, const P2: bool>(_buf: impl BufRead) -> Option<()> {
    if P1 {
        println!("Part 1");
    }
    if P2 {
        println!("Part 2");
    }
    Some(())
}
