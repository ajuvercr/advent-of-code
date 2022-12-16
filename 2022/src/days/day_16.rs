use std::io::BufRead;

trait DFS: Sized {
    type Option;
    type Res: PartialOrd;

    fn options(&self) -> Result<Vec<Self::Option>, Self::Res>;
    fn handle(&mut self, option: Self::Option) -> Self;
}

fn dfs<T: DFS>(mut start: T) -> Option<T::Res> {
    match start.options() {
        Ok(options) => {
            let mut out = None;
            for option in options {
                let ne = start.handle(option);
                if let Some(t) = dfs(ne) {
                    if let Some(m) = out.as_ref() {
                        if &t > m {
                            out = Some(t);
                        }
                    } else {
                        out = Some(t);
                    }
                }
            }
            out
        }
        Err(r) => Some(r),
    }
}

struct Pipes<'a> {
    flow: usize,
    time: usize,
    current: usize,
    pipes: &'a [Pipe],
    opened: u64,
}

enum Action {
    Move(usize),
    Open,
}

impl<'a> DFS for Pipes<'a> {
    type Option = Action;

    type Res = usize;

    fn options(&self) -> Result<Vec<Self::Option>, Self::Res> {
        if self.time == 0 {
            return Err(self.flow);
        }

        let this = &self.pipes[self.current];
        let mut options: Vec<Action> = this.options.iter().map(|i| Action::Move(*i)).collect();

        if (self.opened & (1 << self.current)) == 0 {
            options.push(Action::Open);
        }

        Ok(options)
    }

    fn handle(&mut self, option: Self::Option) -> Self {
        match option {
            Action::Move(i) => Self {
                flow: self.flow,
                time: self.time - 1,
                current: i,
                pipes: self.pipes,
                opened: self.opened,
            },
            Action::Open => {
                let opened = self.opened | (1 << self.current);
                let flow = self.flow + self.time * self.pipes[self.current].rate;
                Self {
                    flow,
                    opened,
                    time: self.time - 1,
                    current: self.current,
                    pipes: self.pipes,
                }
            }
        }
    }
}

struct Pipe {
    rate: usize,
    options: Vec<usize>,
}

pub fn solve<const P1: bool, const P2: bool>(_buf: impl BufRead) -> Option<()> {
    if P1 {
        println!("Part 1");
    }
    if P2 {
        println!("Part 2");
    }
    Some(())
}
