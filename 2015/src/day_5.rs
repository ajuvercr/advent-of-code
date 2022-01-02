fn check_vowels(i: &&str) -> bool {
    i.chars()
        .filter(|&c| c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u')
        .count()
        >= 3
}

fn check_double(i: &&str) -> bool {
    i.chars()
        .scan('\0', |s, c| {
            let out = *s == c;
            *s = c;
            Some(out)
        })
        .filter(|x| *x)
        .next()
        .is_some()
}

const RANDOM: [(char, char); 4] = [('a', 'b'), ('c', 'd'), ('p', 'q'), ('x', 'y')];
fn check_random(i: &&str) -> bool {
    let f = i.chars();
    let ff = i.chars().skip(1);
    !f.zip(ff).any(|x| RANDOM.contains(&x))
}

fn check_double_double(i: &&str) -> bool {
    use std::collections::HashMap;

    let mut map = HashMap::new();

    let f = i.chars();
    let ff = i.chars().skip(1);
    for (i, x) in f.zip(ff).enumerate() {
        if let Some(j) = map.get(&x) {
            if *j < (i - 1) {
                return true;
            }
        } else {
            map.insert(x, i);
        }
    }
    return false;
}

fn check_triple(i: &&str) -> bool {
    let f = i.chars();
    let ff = i.chars().skip(2);
    f.zip(ff).find(|(x, y)| x == y).is_some()
}

use std::fs;
pub fn solve(file: &str) {
    let content = fs::read_to_string(file).unwrap();
    let nice = content
        .lines()
        .filter(check_random)
        .filter(check_vowels)
        .filter(check_double)
        .count();
    let nice_2 = content
        .lines()
        .filter(check_double_double)
        .filter(check_triple)
        .count();
    println!("part 1 {}", nice);
    println!("part 2 {}", nice_2);
}
