use std::{io::BufRead, mem::MaybeUninit};

macro_rules! c {
    ($f:expr, $do:expr) => {
        super::construct::<_, $do>($f)
    };
}

mod day_01;
mod day_02;
mod day_03;

fn construct<T, const DO: bool>(f: impl FnOnce() -> T) -> T {
    if DO {
        f()
    } else {
        unsafe {
            let o = MaybeUninit::<T>::uninit();
            o.assume_init()
        }
    }
}

pub fn solve<const PART1: bool, const PART2: bool>(day: u16, input: impl BufRead) -> Option<()> {
    match day {
        1 => day_01::solve::<PART1, PART2>(input),
        2 => day_02::solve::<PART1, PART2>(input),
        3 => day_03::solve::<PART1, PART2>(input),

        _ => panic!(),
    }
}
