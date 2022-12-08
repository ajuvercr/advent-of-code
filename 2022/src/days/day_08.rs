use std::io::BufRead;

fn create_vec<T: Clone>(size: usize, t: T) -> Vec<T> {
    let mut out = Vec::with_capacity(size);
    out.resize(size, t);
    out
}

#[derive(Default)]
pub struct Part1 {
    max: u8,
}

impl Part1 {
    fn update(&mut self, index: usize, _: usize, el: u8, row: &mut u128) {
        let el = el + 1;
        if el > self.max {
            *row = *row | 1 << index;
        }
        self.max = self.max.max(el);
    }

    fn clear(&mut self) {
        self.max = 0;
    }
}

type ViewDistance = [usize; 10];
#[derive(Default)]
pub struct Part2 {
    vd: ViewDistance,
}

impl Part2 {
    fn update(&mut self, index: usize, j: usize, el: u8, row: &mut [usize]) {
        row[index] *= j - self.vd[el as usize];
        for i in 0..(el + 1) {
            self.vd[i as usize] = j;
        }
    }

    fn clear(&mut self) {
        self.vd.fill(0);
    }
}

macro_rules! f {
    ($b:ident, $exp:expr) => {
        if $b {
            $exp;
        }
    };
}

pub fn solve<const P1: bool, const P2: bool>(buf: impl BufRead) -> Option<()> {
    let mut rows: Vec<Vec<u8>> = buf.split(b'\n').flatten().collect();
    rows.iter_mut()
        .for_each(|x| x.iter_mut().for_each(|y| *y = *y - b'0'));

    let col: Vec<u8> = create_vec(rows.len(), 0);
    let mut cols: Vec<Vec<u8>> = create_vec(rows[0].len(), col);

    for (i, x) in rows.iter().enumerate() {
        for (j, y) in x.iter().enumerate() {
            cols[j][i] = *y;
        }
    }

    let inv = rows.len() - 1;

    let mut s1 = create_vec(rows.len(), 0);
    let mut s2 = create_vec(cols.len(), create_vec(rows.len(), 1));

    let mut part1 = Part1::default();
    let mut part2 = Part2::default();

    for (i, row) in rows.iter().enumerate() {
        for (j, &el) in row.iter().enumerate() {
            f!(P1, part1.update(j, j, el, &mut s1[i]));
            f!(P2, part2.update(j, j, el, &mut s2[i]));
        }
        f!(P1, part1.clear());
        f!(P2, part2.clear());
    }

    for (i, row) in rows.iter().enumerate() {
        for (j, &el) in row.iter().rev().enumerate() {
            f!(P1, part1.update(inv - j, j, el, &mut s1[i]));
            f!(P2, part2.update(inv - j, j, el, &mut s2[i]));
        }
        f!(P1, part1.clear());
        f!(P2, part2.clear());
    }

    for (i, row) in cols.iter().enumerate() {
        for (j, &el) in row.iter().enumerate() {
            f!(P1, part1.update(i, j, el, &mut s1[j]));
            f!(P2, part2.update(i, j, el, &mut s2[j]));
        }
        f!(P1, part1.clear());
        f!(P2, part2.clear());
    }

    for (i, row) in cols.iter().enumerate() {
        for (j, &el) in row.iter().rev().enumerate() {
            f!(P1, part1.update(i, j, el, &mut s1[inv - j]));
            f!(P2, part2.update(i, j, el, &mut s2[inv - j]));
        }
        f!(P1, part1.clear());
        f!(P2, part2.clear());
    }

    if P1 {
        let mut part1 = 0;
        for i in 0..cols.len() {
            part1 += s1[i].count_ones();
        }
        println!("Part 1: {}", part1);
    }

    if P2 {
        let mut max = 0;
        for i in 0..cols.len() {
            for j in 0..cols.len() {
                let scenic = s2[i][j];
                if scenic > max {
                    max = scenic;
                }
            }
        }

        println!("Part 2: {}", max);
    }
    Some(())
}
