use std::{io::BufRead, mem::MaybeUninit};

macro_rules! c {
    ($f:expr, $do:expr) => {
        super::construct::<_, $do>($f)
    };
}

macro_rules! f {
    ($b:ident, $exp:expr) => {
        if $b {
            $exp;
        }
    };
}

mod day_01;
mod day_02;
mod day_03;
mod day_04;
mod day_05;
mod day_06;
mod day_07;
mod day_08;
mod day_09;
mod day_10;
mod day_11;
mod day_12;
mod day_13;
mod day_14;
mod day_15;
mod day_16;
mod day_17;
mod day_18;
mod day_19;
mod day_20;
mod day_21;
mod day_22;
mod day_23;
mod day_24;
mod day_25;

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
        4 => day_04::solve::<PART1, PART2>(input),
        5 => day_05::solve::<PART1, PART2>(input),
        6 => day_06::solve::<PART1, PART2>(input),
        7 => day_07::solve::<PART1, PART2>(input),
        8 => day_08::solve::<PART1, PART2>(input),
        9 => day_09::solve::<PART1, PART2>(input),
        10 => day_10::solve::<PART1, PART2>(input),
        11 => day_11::solve::<PART1, PART2>(input),
        12 => day_12::solve::<PART1, PART2>(input),
        13 => day_13::solve::<PART1, PART2>(input),
        14 => day_14::solve::<PART1, PART2>(input),
        15 => day_15::solve::<PART1, PART2>(input),
        16 => day_16::solve::<PART1, PART2>(input),
        17 => day_17::solve::<PART1, PART2>(input),
        18 => day_18::solve::<PART1, PART2>(input),
        19 => day_19::solve::<PART1, PART2>(input),
        20 => day_20::solve::<PART1, PART2>(input),
        21 => day_21::solve::<PART1, PART2>(input),
        22 => day_22::solve::<PART1, PART2>(input),
        23 => day_23::solve::<PART1, PART2>(input),
        24 => day_24::solve::<PART1, PART2>(input),
        25 => day_25::solve::<PART1, PART2>(input),

        _ => panic!(),
    }
}
