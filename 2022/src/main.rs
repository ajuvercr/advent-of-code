#![feature(drain_filter)]
#![feature(exact_size_is_empty)]
#![feature(linked_list_cursors)]
use std::{
    borrow::Cow,
    env,
    fs::File,
    io::{stdin, BufRead, BufReader},
};

use chrono::Datelike;

mod days;
pub mod parser;
pub use parser::*;

fn current_day() -> u16 {
    chrono::offset::Local::now().day() as u16
}

fn file(day: u16) -> Cow<'static, str> {
    Cow::from(format!("input/{:02}.txt", day))
}

fn reader(input: Cow<str>) -> Box<dyn BufRead> {
    if input == "-" {
        return Box::new(stdin().lock());
    } else {
        return Box::new(BufReader::new(File::open(input.as_ref()).unwrap()));
    }
}

pub fn main() {
    let args: Vec<String> = env::args().collect();

    let day: u16 = args
        .get(1)
        .and_then(|x| x.parse::<u16>().ok())
        .unwrap_or_else(current_day);

    let part: u16 = args.get(2).and_then(|x| x.parse::<u16>().ok()).unwrap_or(3);

    let file = args.get(3).map(Cow::from).unwrap_or_else(|| file(day));
    let buf: Box<dyn BufRead> = reader(file);

    start(day, part, buf);
}

fn start(day: u16, part: u16, reader: Box<dyn BufRead>) -> Option<()> {
    println!("Day {}", day);
    match part {
        1 => days::solve::<true, false>(day, reader),
        2 => days::solve::<false, true>(day, reader),
        3 => days::solve::<true, true>(day, reader),
        _ => panic!(),
    }
}
