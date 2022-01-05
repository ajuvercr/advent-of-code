use std::str::FromStr;

pub trait Parser: Sized {
    fn parse<'a>(s: &'a str) -> Option<(Self, &'a str)>;
}

pub struct Wrapper<T> {
    pub t: T,
}

pub fn wparse<T: Parser>(s: &str) -> Option<T> {
    let wrapper: Wrapper<T> = s.parse().ok()?;
    Some(wrapper.t)
}

impl<T: Parser> FromStr for Wrapper<T> {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (t, r) = T::parse(s).ok_or(())?;

        if r.trim().len() == 0 {
            Ok(Wrapper { t })
        } else {
            Err(())
        }
    }
}

pub struct WS;
impl Parser for WS {
    fn parse<'a>(s: &'a str) -> Option<(Self, &'a str)> {
        let s = s.trim_start();
        (WS, s).into()
    }
}

impl Parser for char {
    fn parse<'a>(s: &'a str) -> Option<(Self, &'a str)> {
        s.chars().next().map(|x: char| (x, &s[1..]))
    }
}

pub struct Word(pub String);
impl Parser for Word {
    fn parse<'a>(s: &'a str) -> Option<(Self, &'a str)> {
        let w: String = s
            .chars()
            .take_while(|&x| !x.is_ascii_whitespace())
            .collect();
        if w.len() == 0 {
            return None;
        }

        let s = &s[w.len()..];
        (Word(w), s).into()
    }
}

macro_rules! parse_integer {
    ($t:ty) => {
        impl Parser for $t {
            fn parse<'a>(s: &'a str) -> Option<(Self, &'a str)> {
                let start = if s.starts_with("-") { 1 } else { 0 };
                let idx = s[start..]
                    .find(|c: char| !(c.is_ascii_digit()))
                    .unwrap_or(s.len());
                let (i, o) = s.split_at(idx);

                i.parse().ok().map(|x| (x, o))
            }
        }
    };
}
parse_integer!(usize);
parse_integer!(u16);
parse_integer!(u32);
parse_integer!(u64);
parse_integer!(isize);
parse_integer!(i32);
parse_integer!(i64);

impl<T: Parser> Parser for Vec<T> {
    fn parse<'a>(s: &'a str) -> Option<(Self, &'a str)> {
        let mut out = Vec::new();
        let mut s = s;
        while let Some(x) = T::parse(s) {
            s = x.1.trim_start();
            out.push(x.0);
        }
        (out, s).into()
    }
}

pub trait FStr {
    fn mparse<'a, T: Parser>(&'a self) -> Option<(T, &'a Self)>;
}

impl FStr for str {
    fn mparse<'a, T: Parser>(&'a self) -> Option<(T, &'a Self)> {
        T::parse(self)
    }
}
