#[derive(Debug)]
enum Current {
    Normal(char),
    Hex(char, char),
}

use crate::parser::*;
use std::fs;
use std::mem;
impl Parser for Current {
    fn parse<'a>(s: &'a str) -> Option<(Self, &'a str)> {
        let mut chars = s.chars();

        match chars.next()? {
            '\\' => match chars.next()? {
                'x' => (Current::Hex(chars.next()?, chars.next()?), &s[4..]).into(),
                x => (Current::Normal(x), &s[2..]).into(),
            },
            x => Some((Current::Normal(x), &s[1..])),
        }
    }
}

fn handle_line(line: &str) -> usize {
  let parsed: Vec<Current> = wparse(&line[1..line.len()-1]).unwrap();
  line.len() - parsed.len()
}


pub fn solve(file: &str) {
    let content = fs::read_to_string(file).unwrap();
    let out : usize = content.lines().map(handle_line).sum();
    println!("part 1 {}", out);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test1() {
        assert_eq!(handle_line("\"\""), 2);
        assert_eq!(handle_line("\"abc\""), 2);
        assert_eq!(handle_line("\"aaa\\\"aaa\""), 3);
    }
}

