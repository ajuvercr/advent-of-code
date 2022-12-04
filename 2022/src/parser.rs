use std::ops::Index;

pub struct Cr<T: 'static, D> {
    _buf: Vec<D>,
    buf: &'static [T],
    pos: usize,
}

impl<T: 'static, D> Cr<T, D> {
    pub fn new(buf: Vec<D>) -> Self {
        let ptr = buf.as_ptr();
        let ptr: *const T = ptr.cast();
        let slice = unsafe { std::slice::from_raw_parts(ptr, buf.len()) };
        Self {
            _buf: buf,
            buf: slice,
            pos: 0,
        }
    }

    pub fn position(&self) -> usize {
        self.pos
    }

    pub fn set_position(&mut self, pos: usize) {
        self.pos = pos;
    }

    pub fn next(&mut self) -> &T {
        let o = &self.buf[self.pos];
        self.pos += 1;
        o
    }
}

impl<T: Copy, V: Index<usize, Output = T>> Cr<T, V> {
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

type Cursor = Cr<char, Vec<char>>;

pub trait Parse<'a>: Sized {
    fn parse(buf: &mut Cursor, scratch: &mut [u8]) -> Option<Self>;
}

pub fn parse<'a, T: Parse<'a>>(input: Vec<u8>) -> Option<T> {
    let l = input.len();
    let c = input.capacity();
    let ptr = input.as_ptr();
    let ptr: *const char = ptr.cast();
    let slice = unsafe { std::slice::from_raw_parts(ptr, 1) };
    todo!();
    // let mut cursor = Cursor::new(input);
    // let mut scratch = [0u8; 1024];
    // T::parse(&mut cursor, &mut scratch)
}

impl<'a, T: Parse<'a>> Parse<'a> for Option<T> {
    fn parse(buf: &mut Cursor, s: &mut [u8]) -> Option<Self> {
        let ret = buf.position();
        match T::parse(buf, s) {
            Some(x) => Some(Some(x)),
            None => {
                buf.set_position(ret);
                Some(None)
            }
        }
    }
}

impl<'a, T: Parse<'a>> Parse<'a> for Vec<T> {
    fn parse(buf: &mut Cursor, s: &mut [u8]) -> Option<Self> {
        let mut out = Vec::new();

        while let Some(Some(x)) = Option::<T>::parse(buf, s) {
            out.push(x);
        }

        Some(out)
    }
}

macro_rules! imp {
    ($($opt:ident,)*) => {
        impl<'a, $($opt: Parse<'a>),*>  Parse<'a> for ($($opt), *) {
            fn parse(buf: &mut Cursor, s: &mut [u8]) -> Option<Self> {
                Some((
                    $($opt::parse(buf, s)?, )*
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

impl<'a> Parse<'a> for u64 {
    fn parse(buf: &mut Cursor, s: &mut [u8]) -> Option<Self> {
        let out = 0;
        Some(out)
    }
}
