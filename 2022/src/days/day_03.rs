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

pub fn solve<const P1: bool, const P2: bool>(buf: impl BufRead) -> Option<()> {
    let mut lines = buf.lines();
    let mut prios = c!(u32::default, P1);
    let mut organizer = c!(BadgeOrganizer::default, P2);

    while let Some(Ok(l)) = lines.next() {
        let mut total: u64 = c!(u64::default, P2);

        let mut left: u64 = c!(u64::default, P1);
        let mut right: u64 = c!(u64::default, P1);

        for (i, x) in l.char_indices() {
            let num = input_to_number(x);

            if P1 {
                if i < l.len() / 2 {
                    left |= 1 << num;
                } else {
                    right |= 1 << num;
                }
            }

            if P2 {
                total |= 1 << num;
            }
        }
        if P1 {
            prios += (left & right).trailing_zeros();
        }

        if P2 {
            organizer.bump(total);
        }
    }

    if P1 {
        println!("part 1 {}", prios);
    }
    if P2 {
        println!("part 2 {}", organizer.prios);
    }

    Some(())
}
