use std::io::BufRead;

#[derive(Default)]
struct BadgeOrganizer {
    pub prios: u32,
    index: u16,
    current_badge: u64,
}

impl BadgeOrganizer {
    fn bump(&mut self, badge: u64) {
        if self.index == 0 {
            self.current_badge = badge;
        } else {
            self.current_badge &= badge;
        }

        self.index += 1;

        if self.index == 3 {
            self.index = 0;
            self.prios += self.current_badge.trailing_zeros();
        }
    }
}

fn input_to_number(input: char) -> u16 {
    if input >= 'a' && input <= 'z' {
        return input as u16 - 'a' as u16 + 1;
    }
    if input >= 'A' && input <= 'Z' {
        return input as u16 - 'A' as u16 + 27;
    }
    panic!();
}

pub fn solve(input: impl BufRead) -> Option<()> {
    let mut lines = input.lines();
    let mut prios = 0;
    let mut organizer = BadgeOrganizer::default();

    while let Some(Ok(l)) = lines.next() {
        let mut total: u64 = 0;
        let mut left: u64 = 0;
        let mut right: u64 = 0;

        for (i, x) in l.char_indices() {
            let num = input_to_number(x);
            total |= 1 << num;
            if i < l.len() / 2 {
                left |= 1 << num;
            } else {
                right |= 1 << num;
            }
        }

        organizer.bump(total);
        prios += (left & right).trailing_zeros();
    }

    println!("part 1 {}", prios);
    println!("part 2 {}", organizer.prios);

    Some(())
}
