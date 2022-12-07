use std::{collections::HashMap, io::BufRead};

use derive::Parse;

use crate::parser::{parse, Char, NotWS, Word, WS};

type Nl = Char<b'\n'>;

type File = Result<(usize, WS, Word<NotWS>), (Word<NotWS>, WS, Word<NotWS>)>;

#[derive(Parse, Debug)]
enum Load {
    Cd(WS, Word<NotWS>, Nl),
    Ls(Nl, Vec<(File, Nl)>),
}

type Command = (Char<b'$'>, WS, Load);

struct Fs {
    parent: usize,
    dirs: HashMap<String, usize>,
    files: Vec<usize>,
}

impl Fs {
    fn new(parent: usize) -> Self {
        Self {
            parent,
            dirs: HashMap::new(),
            files: Vec::new(),
        }
    }
}

struct FsTracker {
    current: usize,
    fs: Vec<Fs>,
}

impl FsTracker {
    fn new() -> Self {
        Self {
            current: 0,
            fs: vec![Fs::new(0)],
        }
    }

    fn handle(&mut self, command: Command) {
        let mut target = self.fs.len();
        let current = &mut self.fs[self.current];
        match command.2 {
            Load::Cd(_, to, _) => {
                if to.0 == ".." {
                    self.current = current.parent;
                } else if let Some(i) = current.dirs.get(&to.0) {
                    self.current = *i;
                }
            }
            Load::Ls(_, files) => {
                let listed_files = files
                    .iter()
                    .flat_map(|(file, _)| file.as_ref().ok().clone());
                current.files.extend(listed_files.map(|(s, _, _)| s));

                let listed_dirs = files.into_iter().flat_map(|(file, _)| file.err());

                let mut new_files = Vec::new();
                for (_, _, to) in listed_dirs {
                    if current.dirs.get(&to.0).is_none() {
                        current.dirs.insert(to.0.clone(), target);
                        target += 1;
                        new_files.push(Fs::new(self.current));
                    }
                }

                self.fs.extend(new_files);
            }
        }
    }

    fn calculate_sizes(&self, current: usize, vec: &mut Vec<usize>) -> usize {
        let current = &self.fs[current];
        let parents: usize = current
            .dirs
            .values()
            .map(|x| self.calculate_sizes(*x, vec))
            .sum();
        let files: usize = current.files.iter().sum();
        vec.push(parents + files);
        return parents + files;
    }
}

pub fn solve<const P1: bool, const P2: bool>(mut buf: impl BufRead) -> Option<()> {
    let mut bytes = Vec::new();
    buf.read_to_end(&mut bytes).ok();

    let parsed: Vec<Command> = parse(bytes)?;

    let mut fs = FsTracker::new();
    for command in parsed.into_iter().skip(1) {
        fs.handle(command);
    }

    let mut sizes = Vec::new();
    let root = fs.calculate_sizes(0, &mut sizes);

    if P1 {
        let total: usize = sizes.iter().filter(|&x| *x < 100000).sum();
        println!("Part 1: {}", total);
    }
    if P2 {
        let free_space = 70000000 - root;
        let required = 30000000 - free_space;
        let total: usize = *sizes.iter().filter(|&x| *x > required).min()?;
        println!("Part 2: {}", total);
    }

    Some(())
}
