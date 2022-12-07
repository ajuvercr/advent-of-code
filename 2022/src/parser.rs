use std::{marker::PhantomData, ops::Index};

pub trait U8Helper: Copy {
    fn is_digit_ref(&self) -> bool {
        self.is_digit()
    }
    fn is_alpha_ref(&self) -> bool {
        self.is_alpha()
    }
    fn is_white_ref(&self) -> bool {
        self.is_white()
    }

    fn is_digit(self) -> bool;
    fn is_alpha(self) -> bool;
    fn is_white(self) -> bool;
    fn lower(self) -> Self;
    fn eq_ignore_case(self, other: Self) -> bool;
}

impl U8Helper for u8 {
    fn is_digit(self) -> bool {
        self <= b'9' && self >= b'0'
    }

    fn is_alpha(self) -> bool {
        (self >= b'a' && self <= b'z') || (self >= b'A' && self <= b'Z')
    }

    fn is_white(self) -> bool {
        self == b' ' || self == b'\n'
    }

    fn eq_ignore_case(self, other: Self) -> bool {
        self.lower() == other.lower()
    }

    fn lower(self) -> Self {
        if self >= b'A' && self <= b'Z' {
            self - b'A' + b'a'
        } else {
            self
        }
    }
}

pub struct Cr<T> {
    buf: Vec<T>,
    pos: usize,
}

impl<T: std::fmt::Debug> Cr<T> {
    pub fn new(buf: Vec<T>) -> Self {
        Self { buf, pos: 0 }
    }

    pub fn position(&self) -> usize {
        self.pos
    }

    pub fn set_position(&mut self, pos: usize) {
        self.pos = pos;
    }

    pub fn next_n(&mut self, length: usize) -> &[T] {
        let o = &self.buf[self.pos..self.pos + length];
        self.pos += length;
        o
    }

    pub fn next(&mut self) -> &T {
        let o = &self.buf[self.pos];
        self.pos += 1;
        o
    }

    pub fn is_empty(&self) -> bool {
        self.pos == self.buf.len()
    }
}

impl<T> Index<usize> for Cr<T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        &self.buf[index + self.pos]
    }
}

impl<T: Copy> Cr<T> {
    pub fn next_while_slice<F: FnMut(&T) -> bool>(&mut self, mut f: F) -> &[T] {
        let len = self.buf[self.pos..].iter().take_while(|&x| f(x)).count();
        let o = &self.buf[self.pos..self.pos + len];
        self.pos += len;
        o
    }

    pub fn next_while<F: FnMut(&T) -> bool>(&mut self, mut f: F, scratch: &mut [T]) -> usize {
        let max = scratch.len();
        let mut t = 0;

        loop {
            if t == max {
                break;
            }

            let o = &self.buf[self.pos + t];
            if !f(o) {
                break;
            }

            scratch[t] = *o;
            t += 1;
        }

        t
    }
}

pub type Cursor = Cr<u8>;

pub trait Parse<'a>: Sized {
    fn parse(buf: &mut Cursor) -> Option<Self>;
}

pub trait ParserTest {
    fn new() -> Self;
    fn test(buf: &Cursor) -> bool;
}

pub trait CharTest {
    fn test(ch: &u8) -> bool;
}

pub fn parse<'a, T: Parse<'a>>(input: Vec<u8>) -> Option<T> {
    let mut cursor = Cursor::new(input);

    let x = T::parse(&mut cursor)?;
    if cursor.is_empty() {
        Some(x)
    } else {
        None
    }
}

impl<'a, T> Parse<'a> for T
where
    T: ParserTest,
{
    fn parse(buf: &mut Cursor) -> Option<Self> {
        if T::test(&buf) {
            Some(T::new())
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Done;
impl ParserTest for Done {
    fn new() -> Self {
        Done
    }

    fn test(buf: &Cursor) -> bool {
        buf.is_empty()
    }
}

impl<'a, T: Parse<'a>> Parse<'a> for Option<T> {
    fn parse(buf: &mut Cursor) -> Option<Self> {
        let ret = buf.position();
        match T::parse(buf) {
            Some(x) => Some(Some(x)),
            None => {
                buf.set_position(ret);
                Some(None)
            }
        }
    }
}

impl<'a, T: Parse<'a>, R: Parse<'a>> Parse<'a> for Result<T, R> {
    fn parse(buf: &mut Cursor) -> Option<Self> {
        let ret = buf.position();
        if let Some(x) = T::parse(buf) {
            return Some(Ok(x));
        }
        buf.set_position(ret);
        let o = R::parse(buf)?;
        Some(Err(o))
    }
}

impl<'a, T: Parse<'a>> Parse<'a> for Vec<T> {
    fn parse(buf: &mut Cursor) -> Option<Self> {
        let mut out = Vec::new();

        if buf.is_empty() {
            return out.into();
        }

        while let Some(Some(x)) = Option::<T>::parse(buf) {
            out.push(x);
            if buf.is_empty() {
                break;
            }
        }

        Some(out)
    }
}

macro_rules! imp {
    ($($opt:ident,)*) => {
        impl<'a, $($opt: Parse<'a>),*>  Parse<'a> for ($($opt), *) {
            fn parse(buf: &mut Cursor) -> Option<Self> {
                Some((
                    $($opt::parse(buf)?, )*
                ))
            }
        }
    };
}

imp!(A, B,);
imp!(A, B, C,);
imp!(A, B, C, D,);
imp!(A, B, C, D, E,);
imp!(A, B, C, D, E, F,);

macro_rules! us {
    ($ty:ty) => {
        impl<'a> Parse<'a> for $ty {
            fn parse(buf: &mut Cursor) -> Option<Self> {
                let digits = buf.next_while_slice(u8::is_digit_ref);
                if digits.len() == 0 {
                    return None;
                }
                let mut out = 0;
                for d in digits {
                    out = out * 10 + (*d as u8 - b'0') as $ty;
                }

                Some(out)
            }
        }
    };
}
us!(u64);
us!(usize);
us!(u32);
us!(u16);
us!(u8);

macro_rules! is {
    ($ty:ty) => {
        impl<'a> Parse<'a> for $ty {
            fn parse(buf: &mut Cursor) -> Option<Self> {
                if buf.is_empty() {
                    return None;
                }
                let tot = if <Option<Char<b'-'>>>::parse(buf)?.is_some() {
                    -1
                } else {
                    1
                };

                let digits = buf.next_while_slice(u8::is_digit_ref);
                if digits.len() == 0 {
                    return None;
                }
                let mut out = 0;
                for d in digits {
                    out = out * 10 + (*d - b'0') as $ty;
                }

                Some(tot * out)
            }
        }
    };
}

is!(i64);
is!(i32);
is!(isize);
is!(i16);
is!(i8);

impl<'a> Parse<'a> for char {
    fn parse(buf: &mut Cursor) -> Option<Self> {
        if buf.is_empty() {
            return None;
        }
        Some(*buf.next() as char)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Char<const C: u8>;
impl<'a, const C: u8> Parse<'a> for Char<C> {
    fn parse(buf: &mut Cursor) -> Option<Self> {
        if buf.is_empty() {
            return None;
        }
        if buf[0] == C {
            buf.next();
            Some(Char)
        } else {
            None
        }
    }
}

#[derive(Debug)]
pub struct NotWS;
impl CharTest for NotWS {
    fn test(ch: &u8) -> bool {
        !ch.is_ascii_whitespace()
    }
}

#[derive(Debug)]
pub struct AlphaTest;
impl CharTest for AlphaTest {
    fn test(ch: &u8) -> bool {
        ch.is_alpha_ref()
    }
}

#[derive(Debug)]
pub struct AlphaNumTest;
impl CharTest for AlphaNumTest {
    fn test(ch: &u8) -> bool {
        let ch = *ch;
        ch.is_alpha() || ch.is_digit()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Word<T: CharTest>(pub String, PhantomData<T>);

impl<'a, T: CharTest> Parse<'a> for Word<T> {
    fn parse(buf: &mut Cursor) -> Option<Self> {
        let letters = buf.next_while_slice(T::test);
        if letters.len() == 0 {
            return None;
        }
        let inner: Vec<u8> = letters.into_iter().copied().collect();
        Some(Self(
            unsafe { String::from_utf8_unchecked(inner) },
            PhantomData,
        ))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct WS;

impl<'a> Parse<'a> for WS {
    fn parse(buf: &mut Cursor) -> Option<Self> {
        buf.next_while_slice(u8::is_white_ref);
        Some(Self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Puncuated<T, S> {
    items: Vec<T>,
    pd: PhantomData<S>,
}

impl<T, S> Puncuated<T, S> {
    pub fn into_inner(self) -> Vec<T> {
        self.items
    }
}

impl<'a, T, S> Parse<'a> for Puncuated<T, S>
where
    T: Parse<'a>,
    S: Parse<'a>,
{
    fn parse(buf: &mut Cursor) -> Option<Self> {
        let mut items = Vec::new();
        // first parse element
        if let Some(item) = <T>::parse(buf) {
            items.push(item);
        }

        let mut pos = buf.position();
        loop {
            // parse sep
            if <S>::parse(buf).is_none() {
                break;
            }

            if let Some(item) = <T>::parse(buf) {
                items.push(item);
                pos = buf.position();
            } else {
                buf.set_position(pos);
                break;
            }
        }

        Some(Self {
            items,
            pd: PhantomData,
        })
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    #[test]
    fn simple_test() {
        let st = String::from("1").into_bytes();
        let item: Option<char> = parse(st);
        assert_eq!(item, Some('1'))
    }

    #[test]
    fn simple_test_num() {
        let st = String::from("123").into_bytes();
        let item: Option<u64> = parse(st);
        assert_eq!(item, Some(123))
    }

    #[test]
    fn simple_test_inum() {
        let st = String::from("-123").into_bytes();
        let item: Option<i64> = parse(st.clone());
        assert_eq!(item, Some(-123))
    }

    #[test]
    fn simple_test_option() {
        let st = String::from("-123").into_bytes();

        let mut cursor = Cursor::new(st);

        let failed: Option<u64> = u64::parse(&mut cursor);
        let item: Option<i64> = i64::parse(&mut cursor);

        assert_eq!(failed, None);
        assert_eq!(item, Some(-123));
    }

    #[test]
    fn simple_test_tuple() {
        let st = String::from("42-123").into_bytes();

        let x: Option<(u32, i32)> = parse(st);
        assert_eq!(x, Some((42, -123)));
    }

    #[test]
    fn simple_test_result() {
        let st = String::from("1-123").into_bytes();
        let x: Option<Result<(usize, Done), (usize, isize)>> = parse(st);
        assert_eq!(x, Some(Err((1, -123))));
    }

    #[derive(derive::Parse, Debug, PartialEq, Eq)]
    struct Point {
        x: usize,
        y: Char<b'c'>,
    }

    #[test]
    fn simple_test_derive() {
        let st = String::from("123c").into_bytes();
        let x: Option<Point> = parse(st);
        assert_eq!(x, Some(Point { x: 123, y: Char }));
    }

    #[derive(derive::Parse, Debug, PartialEq, Eq)]
    enum Test {
        P,
        A,
    }

    #[test]
    fn simple_test_enum() {
        let st = String::from("a").into_bytes();
        let x: Option<Test> = parse(st);
        assert_eq!(x, Some(Test::A));

        let st = String::from("P").into_bytes();
        let x: Option<Test> = parse(st);
        assert_eq!(x, Some(Test::P));
    }

    #[test]
    fn simmple_test_punctuated() {
        let st = String::from("a,b,c,d").into_bytes();
        let x: Option<Puncuated<Word<AlphaTest>, Char<b','>>> = parse(st);
        assert!(x.is_some());

        let words: Vec<_> = x
            .as_ref()
            .unwrap()
            .items
            .iter()
            .map(|x| x.0.as_str())
            .collect();

        assert_eq!(words, vec!["a", "b", "c", "d"]);
    }

    #[derive(derive::Parse, Debug, PartialEq, Eq)]
    enum TestDeep {
        A(WS, u64),
        B { count: isize },
    }

    #[test]
    fn simple_test_enum_deep() {
        let st = String::from("a  64").into_bytes();
        let x: Option<TestDeep> = parse(st);
        assert_eq!(x, Some(TestDeep::A(WS, 64)));

        let st = String::from("B-32").into_bytes();
        let x: Option<TestDeep> = parse(st);
        assert_eq!(x, Some(TestDeep::B { count: -32 }));
    }
}
