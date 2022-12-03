use std::io::{stdin, BufRead};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum Action {
    Rock,
    Paper,
    Sciccor,
}

impl Action {
    pub fn part2(&self, strat: Self) -> Self {
        if strat == Action::Paper {
            return *self;
        }

        if strat == Action::Rock {
            return match self {
                Action::Rock => Action::Sciccor,
                Action::Paper => Action::Rock,
                Action::Sciccor => Action::Paper,
            };
        }

        return match self {
            Action::Rock => Action::Paper,
            Action::Paper => Action::Sciccor,
            Action::Sciccor => Action::Rock,
        };
    }

    pub fn points(&self) -> usize {
        match self {
            Action::Rock => 1,
            Action::Paper => 2,
            Action::Sciccor => 3,
        }
    }

    pub fn wins(&self, other: &Self) -> usize {
        if self == other {
            return 3;
        }

        match (self, other) {
            (Action::Rock, Action::Sciccor) => 6,
            (Action::Sciccor, Action::Paper) => 6,
            (Action::Paper, Action::Rock) => 6,
            _ => 0,
        }
    }
}

impl From<char> for Action {
    fn from(x: char) -> Self {
        match x {
            'A' | 'X' => Self::Rock,
            'B' | 'Y' => Self::Paper,
            'C' | 'Z' => Self::Sciccor,
            _ => panic!(),
        }
    }
}

pub fn solve() -> Option<()> {
    let mut lines = stdin().lock().lines();
    let mut total = 0;
    let mut total2 = 0;
    while let Some(Ok(l)) = lines.next() {
        let mut chs = l.chars();
        let oponent: Action = chs.next()?.into();
        chs.next();
        let me: Action = chs.next()?.into();
        let me2 = oponent.part2(me);

        total += me.points() + me.wins(&oponent);
        total2 += me2.points() + me2.wins(&oponent);
    }
    println!("Total {}", total);
    println!("Total2 {}", total2);
    Some(())
}
