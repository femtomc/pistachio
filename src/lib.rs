use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use chumsky::{prelude::*, stream::Stream};
use std::collections::HashMap;
use std::fmt;

// ------------------------------------------------
//             For good error reporting.
// ------------------------------------------------

pub type Span = std::ops::Range<usize>;
pub type Spanned<T> = (T, Span);

pub fn create_report(src: &str, errs: Vec<Simple<char>>) -> () {
    errs.into_iter().for_each(|e| {
        let report = Report::build(ReportKind::Error, (), e.span().start);

        let report = match e.reason() {
            chumsky::error::SimpleReason::Unclosed { span, delimiter } => {
                report.with_message(format!(
                    "Unclosed delimiter {}",
                    delimiter.fg(Color::Yellow)
                ))
            }
            chumsky::error::SimpleReason::Unexpected => report
                .with_message(format!(
                    "{}, expected one of the following elements:\n\t{}",
                    if e.found().is_some() {
                        "Unexpected token in input"
                    } else {
                        "Unexpected end of input"
                    },
                    if e.expected().len() == 0 {
                        "something else".to_string()
                    } else {
                        e.expected()
                            .map(|expected| match expected {
                                Some(expected) => expected.to_string(),
                                None => "end of input".to_string(),
                            })
                            .collect::<Vec<_>>()
                            .join(" ")
                    }
                ))
                .with_label(
                    Label::new(e.span())
                        .with_message(format!(
                            "Unexpected token {}",
                            e.found().unwrap().fg(Color::Red)
                        ))
                        .with_color(Color::Red),
                ),
            chumsky::error::SimpleReason::Custom(msg) => {
                report.with_message(msg).with_label(
                    Label::new(e.span())
                        .with_message(format!("{}", msg.fg(Color::Red)))
                        .with_color(Color::Red),
                )
            }
        };

        report.finish().print(Source::from(&src)).unwrap();
    });
}

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
    body: Spanned<Expr>,
}

type Value = Closure;

pub fn extend(mut rho: Env, x: Symbol, v: Value) -> Env {
    rho.insert(x, v);
    return rho;
}

pub fn val(rho: &Env, e: Spanned<Expr>) -> Result<Value, ReportKind> {
    match e {
        (LAbs(x, b), _) => Ok(Closure {
            env: rho.clone(),
            var: Some(x),
            body: *b.clone(),
        }),
        (LVar(x), _) => Ok(rho.get(&x).unwrap().clone()),
        (LApp(e1, e2), _) => {
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum Token {
    Space,
    LParen,
    RParen,
    Str(String),
    Dot,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Space => write!(f, " "),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::Str(s) => write!(f, "{}", s),
            Token::Dot => write!(f, "."),
        }
    }
}

fn lexer() -> impl Parser<char, Vec<(Token, Span)>, Error = Simple<char>> {
    let space_ = just(' ').map(|_| Token::Space);
    let str_ = text::ident().map(|ident: String| Token::Str(ident));
    let dot_ = just('.').map(|_| Token::Dot);
    let lparen_ = just('(').map(|_| Token::LParen);
    let rparen_ = just(')').map(|_| Token::RParen);
    let token = space_
        .or(lparen_)
        .or(rparen_)
        .or(dot_)
        .or(str_)
        .recover_with(skip_then_retry_until([]));
    let comment = just("--").then(take_until(just('\n'))).padded();
    token
        .map_with_span(|tok, span| (tok, span))
        .padded_by(comment.repeated())
        .repeated()
}

fn parser() -> impl Parser<Token, Spanned<Expr>, Error = Simple<Token>> {
    recursive(|expr| {
        let lvar_ = select! {
            Token::Str(ident) => Expr::LVar(ident.clone()),
        };

        let ident_ = select! { Token::Str(ident) => ident.clone() };

        let labs_ = ident_
            .then_ignore(just(Token::Dot))
            .then(expr.clone())
            .map(|(n, e)| Expr::LAbs(n, e));

        let lapp_ = just(Token::LParen)
            .ignore_then(expr.clone())
            .then_ignore(just(Token::Space))
            .then(expr.clone())
            .then_ignore(just(Token::RParen))
            .map(|(e1, e2)| Expr::LApp(e1, e2));

        lapp_
            .or(labs_)
            .or(lvar_)
            .map_with_span(|expr, span| (expr, span))
    })
}

// ------------------------------------------------
//                    Testing.
// ------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lexer_0() {
        let test_str = "(x.y)";
        let (tokens, errs) = lexer().parse_recovery_verbose(test_str);
        assert!(errs.is_empty());
    }

    #[test]
    fn lexer_1() {
        let test_str = "(x.y )";
        let (tokens, errs) = lexer().parse_recovery(test_str);
        assert!(errs.is_empty());
    }

    #[test]
    fn parser_1() {
        let test_str = "(x.y )";
        let (tokens, errs) = lexer().parse_recovery(test_str);
        assert!(errs.is_empty());
        let len = test_str.chars().count();
        let (expr, parse_errs) = parser().parse_recovery(Stream::from_iter(
            len..len + 1,
            tokens.unwrap().into_iter(),
        ));
        assert!(parse_errs.is_empty());
    }
}
