use std::collections::{HashMap, HashSet};

#[derive(Default, Debug)]
struct Maps<'a> {
    connections: HashMap<&'a str, HashSet<&'a str>>,
    idx: HashMap<&'a str, usize>,
}

impl<'a> Maps<'a> {
    pub fn count(&self) -> Vec<(&'a str, &'a str, &'a str)> {
        let mut out = Vec::new();
        for (k, v) in &self.connections {
            if v.len() < 2 {
                continue;
            }

            for k2 in v {
                if k > k2 {
                    continue;
                }
                out.extend(
                    self.connections[k2]
                        .iter()
                        .filter(|x| *x > k2)
                        .filter(|x| v.contains(*x))
                        .map(|x| (*k, *k2, *x)),
                );
            }
        }
        out
    }

    fn remove(&mut self, key: &str) {
        self.connections.remove(key);
        for v in self.connections.values_mut() {
            v.remove(key);
        }
    }

    pub fn find_clique(&mut self) {
        loop {
            if let Some(lowest) = self.connections.iter().min_by_key(|x| x.1.len()) {
                if lowest.1.len() == self.connections.len() - 1 {
                    break;
                }
                self.remove(*lowest.0);
            }
        }
    }
}

fn parse<'a>(inp: &'a str) -> Maps<'a> {
    let mut map = Maps::default();
    for (a, b) in inp.split('\n').flat_map(|x| x.split_once('-')) {
        map.connections.entry(a).or_default().insert(b);
        map.connections.entry(b).or_default().insert(a);

        if !map.idx.contains_key(a) {
            map.idx.insert(a, map.idx.len());
        }

        if !map.idx.contains_key(b) {
            map.idx.insert(b, map.idx.len());
        }
    }

    map
}

pub fn part1(inp: &str) -> usize {
    let map = parse(inp);
    let mut cliquest = map.count();
    cliquest.sort();
    cliquest
        .into_iter()
        .filter(|(x, y, z)| x.starts_with('t') || y.starts_with('t') || z.starts_with('t'))
        .count()
}

pub fn part2(inp: &str) -> String {
    let mut map = parse(inp);
    map.find_clique();
    let mut keys: Vec<_> = map.connections.keys().collect();
    keys.sort();
    let mut out = keys[0].to_string();

    for key in keys.iter().skip(1) {
        out += ",";
        out += key;
    }

    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn build_clique() {
        let inp = "kh-tc
qp-kh
de-cg
ka-co
yn-aq
qp-ub
cg-tb
vc-aq
tb-ka
wh-tc
yn-cg
kh-ub
ta-co
de-co
tc-td
tb-wq
wh-td
ta-ka
td-qp
aq-cg
wq-ub
ub-vc
de-ta
wq-aq
wq-vc
wh-yn
ka-de
kh-ta
co-tc
wh-qp
tb-vc
td-yn";
        assert_eq!(part1(inp), 7);
    }

    #[test]
    fn part2_test() {
        let inp = "kh-tc
qp-kh
de-cg
ka-co
yn-aq
qp-ub
cg-tb
vc-aq
tb-ka
wh-tc
yn-cg
kh-ub
ta-co
de-co
tc-td
tb-wq
wh-td
ta-ka
td-qp
aq-cg
wq-ub
ub-vc
de-ta
wq-aq
wq-vc
wh-yn
ka-de
kh-ta
co-tc
wh-qp
tb-vc
td-yn";
        assert_eq!(part2(inp), String::from("co,de,ka,ta"));
    }
}

