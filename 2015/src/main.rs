mod day_1;
mod day_2;
mod day_3;
mod day_5;
mod day_6;
mod day_7;
mod day_8;
pub mod parser;

use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    let day = args.get(1).map(String::as_str).unwrap_or("1");
    let location = args.get(2).cloned().unwrap_or(format!("input/{}.txt", day));

    match day {
        "3" => day_3::solve(&location),
        "8" => day_8::solve(&location),
        "6" => day_6::solve(&location),
        "5" => day_5::solve(&location),
        "1" => day_1::solve(&location),
        "2" => day_2::solve(&location),
        "7" => day_7::solve(&location),
        _ => {}
    }
}
