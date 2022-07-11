use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use chumsky::{prelude::*, stream::Stream};
use std::collections::HashMap;
use std::fmt;

// For good error reporting.
pub type Span = std::ops::Range<usize>;
pub type Spanned<T> = (T, Span);

// ------------------------------------------------
//          Core language and evaluation.
// ------------------------------------------------

#[derive(Clone)]
pub enum Expr {
    LVar(String),
    LAbs(String, Box<Spanned<Self>>),
    LApp(Box<Spanned<Self>>, Box<Spanned<Self>>),
}
pub use Expr::*;

type Symbol = String;
type Env = HashMap<String, Value>;

#[derive(Clone)]
pub struct Closure {
    env: Env,
    var: Option<Symbol>,
    body: Expr,
}

type Value = Closure;

pub fn extend(mut rho: Env, x: Symbol, v: Value) -> Env {
    rho.insert(x, v);
    return rho;
}

pub fn val(rho: &Env, e: Expr) -> Result<Value, ReportKind> {
    match e {
        LAbs(x, b) => Ok(Closure {
            env: rho.clone(),
            var: Some(x),
            body: *b.clone(),
        }),
        LVar(x) => Ok(rho.get(&x).unwrap().clone()),
        LApp(e1, e2) => {
            let v1 = val(&rho, *e1).unwrap();
            let v2 = val(&rho, *e2).unwrap();
            apply(v1, v2)
        }
    }
}

pub fn apply(clos: Closure, arg: Value) -> Result<Closure, ReportKind> {
    match clos {
        Closure {
            env: rho,
            var: v,
            body: b,
        } => Ok(val(&extend(rho, v.unwrap(), arg), b)?),
    }
}

// ------------------------------------------------
//              Lexing and parsing.
// ------------------------------------------------

enum Token {
    LParen,
    RParen,
    Str(String),
    Dot,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::Str(s) => write!(f, "{}", s),
            Token::Dot => write!(f, "."),
        }
    }
}

fn lexer() -> impl Parser<char, Vec<(Token, Span)>, Error = Simple<char>> {
    let str_ = text::ident().map(|ident: String| Token::Str(ident));
    let dot_ = just('.').map(|_| Token::Dot);
    let lparen_ = just('(').map(|_| Token::LParen);
    let rparen_ = just(')').map(|_| Token::RParen);
    let token = lparen_.or(rparen_).or(dot_).or(str_);
    token.map_with_span(|tok, span| (tok, span)).repeated()
}
