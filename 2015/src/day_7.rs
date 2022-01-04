use crate::parser::*;
#[derive(Debug)]
enum BinOp {
    And,
    Or,
    LShift,
    RShift,
}
impl BinOp {
    fn apply(&self, v1: u16, v2: u16) -> u16 {
        match self {
            BinOp::And => v1 & v2,
            BinOp::Or => v1 | v2,
            BinOp::LShift => v1 << v2,
            BinOp::RShift => v1 >> v2,
        }
    }
}
impl Parser for BinOp {
    fn parse<'a>(_s: &'a str) -> Option<(Self, &'a str)> {
        let (_, s) = _s.mparse::<WS>()?;
        let (w, s) = s.mparse::<Word>()?;
        let (_, s) = s.mparse::<WS>()?;

        let this = match w.0.as_str() {
            "AND" => BinOp::And,
            "OR" => BinOp::Or,
            "LSHIFT" => BinOp::LShift,
            "RSHIFT" => BinOp::RShift,
            _ => return None,
        };
        (this, s).into()
    }
}
#[derive(Debug)]
enum UnOp {
    Map,
    Not,
}
impl UnOp {
    fn apply(&self, x: u16) -> u16 {
        match self {
            UnOp::Map => x,
            UnOp::Not => !x,
        }
    }
}
impl Parser for UnOp {
    fn parse<'a>(_s: &'a str) -> Option<(Self, &'a str)> {
        let (_, s) = _s.mparse::<WS>()?;
        let (w, s) = s.mparse::<Word>()?;
        let (_, s) = s.mparse::<WS>()?;

        let this = match w.0.as_str() {
            "NOT" => UnOp::Not,
            _ => return (UnOp::Map, _s).into(),
        };
        (this, s).into()
    }
}
#[derive(Debug)]
enum Value {
    Lit(u16),
    Reg(String),
}
impl Parser for Value {
    fn parse<'a>(s: &'a str) -> Option<(Self, &'a str)> {
        s.mparse::<u16>()
            .map(|(x, s)| (Value::Lit(x), s))
            .or_else(|| s.mparse::<Word>().map(|(x, s)| (Value::Reg(x.0), s)))
    }
}
impl Value {
    fn get(&self, state: &State) -> Option<u16> {
        match self {
            Value::Lit(x) => Some(*x),
            Value::Reg(ref x) => state.get(x.as_str()).cloned(),
        }
    }
}

#[derive(Debug)]
enum Lhs {
    Bin(BinOp, Value, Value),
    Un(UnOp, Value),
}
impl Lhs {
    fn parse_bin(s: &str) -> Option<(Self, &str)> {
        let (v1, s) = s.mparse()?;
        let (_, s) = s.mparse::<WS>()?;
        let (o, s) = s.mparse()?;
        let (_, s) = s.mparse::<WS>()?;
        let (v2, s) = s.mparse()?;
        (Lhs::Bin(o, v1, v2), s).into()
    }

    fn parse_un(s: &str) -> Option<(Self, &str)> {
        let (op, s) = s.mparse::<UnOp>()?;
        let (_, s) = s.mparse::<WS>()?;
        let (v, s) = s.mparse()?;
        (Lhs::Un(op, v), s).into()
    }
}
impl Parser for Lhs {
    fn parse<'a>(s: &'a str) -> Option<(Self, &'a str)> {
        Lhs::parse_bin(s).or_else(|| Lhs::parse_un(s))
    }
}

#[derive(Debug)]
struct Inst {
    lhs: Lhs,
    target: String,
}
impl Parser for Inst {
    fn parse<'a>(s: &'a str) -> Option<(Self, &'a str)> {
        let (_, s) = s.mparse::<WS>()?;
        let (lhs, s) = s.mparse()?;
        let (_, s) = s.mparse::<WS>()?;
        let (_, s) = s.mparse::<Word>()?;
        let (_, s) = s.mparse::<WS>()?;
        let (target, s) = s.mparse::<Word>()?;
        let (_, s) = s.mparse::<WS>()?;

        (
            Inst {
                lhs,
                target: target.0,
            },
            s,
        )
            .into()
    }
}
type State<'a> = std::collections::HashMap<&'a str, u16>;

impl Inst {
    fn apply<'a>(&'a self, state: &mut State<'a>) -> bool {
        if state.contains_key(self.target.as_str()) {
            return false;
        }

        if let Some(x) = self.calc_value(state) {
            state.insert(self.target.as_str(), x);
            return true;
        }
        false
    }

    fn calc_value(&self, state: &mut State<'_>) -> Option<u16> {
        match self.lhs {
            Lhs::Un(ref a, ref v) => a.apply(v.get(state)?).into(),
            Lhs::Bin(ref a, ref v1, ref v2) => a.apply(v1.get(state)?, v2.get(state)?).into(),
        }
    }
}

use std::fs;
pub fn solve(file: &str) {
    let content = fs::read_to_string(file).unwrap();
    //let insts: Vec<_> = content.lines().flat_map(wparse::<Inst>).collect();
    let insts: Vec<Inst> = wparse(&content).unwrap();

    let a = {
        let mut state = State::new();

        while insts.iter().any(|x| x.apply(&mut state)) {}
        *state.get("a").unwrap()
    };
    println!("part 1: {}", a);

    let a = {
        let mut state = State::new();
        state.insert("b", a);
        while insts.iter().any(|x| x.apply(&mut state)) {}

        *state.get("a").unwrap()
    };
    println!("part 2: {}", a);
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_this() {
        assert!(wparse::<Lhs>("123").is_some());
        assert!(wparse::<Lhs>("NOT x").is_some());
        assert!(wparse::<Lhs>("x AND y").is_some());
        assert!(wparse::<Inst>("x AND y -> xs").is_some());
    }
    #[test]
    fn test_long() {
        let c = r#"
kg OR kf -> kh
ep OR eo -> eq
44430 -> b
NOT gs -> gt
dd OR do -> dp
eg AND ei -> ej
y AND ae -> ag
jx AND jz -> ka
lf RSHIFT 2 -> lg
z AND aa -> ac
dy AND ej -> el
bj OR bi -> bk
kk RSHIFT 3 -> km
        "#;

        assert!(wparse::<Vec<Inst>>(c).is_some());
    }
}
