use std::{collections::VecDeque, io::BufRead};

struct Window<const LEN: u8> {
    same_count: u8,
    set: [u8; 256],
    window: VecDeque<u8>,
}

impl<const LEN: u8> Window<LEN> {
    fn new() -> Self {
        Self {
            same_count: 0,
            set: [0u8; 256],
            window: VecDeque::new(),
        }
    }
    fn ingest(&mut self, item: u8) -> bool {
        if self.window.len() == LEN as usize {
            let front = self.window.pop_front().unwrap();
            self.degest(front);
        }

        let c = self.set[item as usize];
        if c == 0 {
            self.same_count += 1;
        }
        self.set[item as usize] += 1;
        self.window.push_back(item);

        self.same_count == LEN
    }

    fn degest(&mut self, item: u8) {
        let c = self.set[item as usize];
        if c == 1 {
            self.same_count -= 1;
        }
        self.set[item as usize] -= 1;
    }
}

pub fn solve<const P1: bool, const P2: bool>(mut buf: impl BufRead) -> Option<()> {
    let mut buffer = Vec::new();
    buf.read_to_end(&mut buffer).ok()?;

    if P1 {
        let mut window = Window::<4>::new();
        let count = buffer.iter().take_while(|&x| !window.ingest(*x)).count();
        println!("Part 1: {}", count + 1);
    }
    if P2 {
        let mut window = Window::<14>::new();
        let count = buffer.iter().take_while(|&x| !window.ingest(*x)).count();
        println!("Part 2: {}", count + 1);
    }
    Some(())
}
