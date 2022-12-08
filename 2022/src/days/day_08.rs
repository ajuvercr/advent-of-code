use std::io::BufRead;

#[derive(Default)]
pub struct Row {
    max: usize,
    counts: [usize; 11],
}

impl Row {
    fn push(&mut self, el: usize) -> bool {
        let out = el > self.max;

        self.max = self.max.max(el);
        self.counts[el] += 1;
        out
    }

    fn pop(&mut self, el: usize) -> bool {
        self.counts[el] -= 1;

        if self.counts[el] == 0 && el == self.max {
            for i in (0..el).rev() {
                self.max = i;
                if self.counts[i] > 0 {
                    break;
                }
            }
        }

        el > self.max
    }
}

type RowPair = (Row, Row);
pub fn solve<const P1: bool, const P2: bool>(mut buf: impl BufRead) -> Option<()> {
    let mut rows: Vec<RowPair> = Vec::new();
    let mut cols: Vec<RowPair> = Vec::new();

    let mut row_i = 0;
    let mut col_i = 0;

    let mut bytes = Vec::new();
    buf.read_to_end(&mut bytes).ok()?;
    rows.push(RowPair::default());
    for &byte in &bytes {
        // If you are still on the first row, columns need to be made
        if row_i == 0 {
            cols.push(RowPair::default());
        }
        if byte == b'\n' {
            row_i += 1;
            col_i = 0;
            rows.push(RowPair::default());
            continue;
        }

        let el = (byte - b'0') as usize + 1;
        // Push element on the right side
        rows[row_i].1.push(el);
        cols[col_i].1.push(el);

        col_i += 1;
    }

    row_i = 0;
    col_i = 0;
    let mut count = 0;
    for &byte in &bytes {
        if byte == b'\n' {
            row_i += 1;
            col_i = 0;
            continue;
        }

        let el = (byte - b'0') as usize + 1;
        let vis_left = rows[row_i].0.push(el);
        let vis_right = rows[row_i].1.pop(el);
        let vis_top = cols[col_i].0.push(el);
        let vis_bot = cols[col_i].1.pop(el);

        if vis_left || vis_right || vis_top || vis_bot {
            count += 1;
        }
        col_i += 1;
    }

    if P1 {
        println!("Part 1: {}", count);
    }
    if P2 {
        println!("Part 2");
    }
    Some(())
}
