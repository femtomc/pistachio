use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use chumsky::{prelude::*, stream::Stream};
use std::collections::HashMap;
use std::fmt;

// ------------------------------------------------
//             For good error reporting
// ------------------------------------------------

type Span = std::ops::Range<usize>;
type Spanned<T> = (T, Span);

fn create_reports<T>(errs: &[Simple<T>]) -> Vec<Report>
where
    T: PartialEq + Eq + std::hash::Hash + std::fmt::Display,
{
    errs.iter()
        .map(|e| {
            let report = Report::build(ReportKind::Error, (), e.span().start);

            let report = match e.reason() {
                chumsky::error::SimpleReason::Unclosed {
                    span: _,
                    delimiter,
                } => report.with_message(format!(
                    "Unclosed delimiter {}",
                    delimiter.fg(Color::Yellow)
                )),
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

            report.finish()
        })
        .collect()
}

fn print_reports(src: &str, reports: Vec<Report>) {
    reports
        .into_iter()
        .for_each(|r| r.print(Source::from(src)).unwrap());
}

// ------------------------------------------------
//          Core language and evaluation
// ------------------------------------------------

#[derive(Debug, Clone)]
enum Expr {
    LInt(i64),
    LVar(String),
    LAbs(String, Box<Spanned<Self>>),
    LApp(Box<Spanned<Self>>, Box<Spanned<Self>>),
    LLet(String, Box<Spanned<Self>>, Box<Spanned<Self>>),
}
use Expr::*;

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LInt(v) => write!(f, "{}", v),
            LVar(v) => write!(f, "{}", v),
            LAbs(v, b) => write!(f, "{}.{}", v, b.0),
            LApp(e1, e2) => write!(f, "({} {})", e1.0, e2.0),
            LLet(v, e, b) => write!(f, "let {} = {} in \n{}", v, e.0, b.0),
        }
    }
}

type Symbol = String;
type Env = HashMap<String, Value>;

#[derive(Debug, Clone)]
struct Clos {
    env: Env,
    var: Option<Symbol>,
    body: Spanned<Expr>,
}

#[derive(Debug, Clone)]
enum Value {
    Int(i64),
    NVar(String),
    NApp(Box<Spanned<Value>>, Box<Spanned<Value>>),
    Clos(Clos),
}

fn extend(mut rho: Env, x: Symbol, v: Value) -> Env {
    rho.insert(x, v);
    rho
}

fn unspan<T>(v: Spanned<T>) -> T {
    let (actual, _span) = v;
    actual
}

fn val(rho: &Env, e: Spanned<Expr>) -> Result<Spanned<Value>, Report> {
    match e {
        (LInt(v), span) => Ok((Value::Int(v), span)),
        (LVar(x), span) => Ok((rho.get(&x).unwrap().clone(), span)),
        (LAbs(x, b), span) => Ok((
            Value::Clos(Clos {
                env: rho.clone(),
                var: Some(x),
                body: *b,
            }),
            span,
        )),
        (LApp(e1, e2), span) => {
            let v1 = val(rho, *e1)?;
            let v2 = val(rho, *e2)?;
            let new = unspan(apply(v1, v2)?);
            Ok((new, span))
        }
        (LLet(v, e, b), span) => {
            let evaluated = unspan(val(rho, *e)?);
            let rho_new = extend(rho.clone(), v, evaluated);
            let new = unspan(val(&rho_new, *b)?);
            Ok((new, span))
        }
    }
}

fn apply(
    fun: Spanned<Value>,
    arg: Spanned<Value>,
) -> Result<Spanned<Value>, Report> {
    match fun {
        (
            Value::Clos(Clos {
                env: rho,
                var: v,
                body: b,
            }),
            _span,
        ) => {
            let new_rho = extend(rho, v.unwrap(), arg.0);
            let new = val(&new_rho, b)?;
            Ok(new)
        }
        (_, _) => {
            let new_span = std::ops::Range {
                start: fun.1.start,
                end: arg.1.end,
            };
            Ok((Value::NApp(Box::new(fun), Box::new(arg)), new_span))
        }
    }
}

fn gensym(x: &str) -> String {
    format!("{}*", x)
}

fn freshen(used: &[String], x: &str) -> String {
    match used.contains(&x.to_string()) {
        true => freshen(used, &gensym(x)),
        false => x.to_string(),
    }
}

fn read_back(
    used: &mut Vec<String>,
    v: Spanned<Value>,
) -> Result<Spanned<Expr>, Report> {
    match v {
        (
            Value::Clos(Clos {
                env: rho,
                var: x,
                body: b,
            }),
            span,
        ) => {
            let name = x.unwrap();
            let y = freshen(used, &name);
            let neutral = Value::NVar(y.clone());
            used.push(y.to_string());
            let new_rho = extend(rho, name, neutral);
            let v = val(&new_rho, b)?;
            let quoted = read_back(used, v)?;
            Ok((LAbs(y, Box::new(quoted)), span))
        }
        (Value::NVar(x), span) => Ok((LVar(x), span)),
        (Value::NApp(n1, n2), span) => {
            let e1 = read_back(used, *n1)?;
            let e2 = read_back(used, *n2)?;
            Ok((LApp(Box::new(e1), Box::new(e2)), span))
        }
        (Value::Int(v), span) => Ok((LInt(v), span)),
    }
}

fn norm(rho: &Env, e: Spanned<Expr>) -> Result<Spanned<Expr>, Report> {
    let mut v = Vec::new();
    let e = val(rho, e)?;
    read_back(&mut v, e)
}

// ------------------------------------------------
//              Lexing and parsing
// ------------------------------------------------

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum Token {
    LParen,
    RParen,
    Dot,
    Ident(String),
    Operator(String),
    Keyword(String),
    Literal(i64),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::Dot => write!(f, "."),
            Token::Ident(s) => write!(f, "{}", s),
            Token::Operator(s) => write!(f, "{}", s),
            Token::Keyword(s) => write!(f, "{}", s),
            Token::Literal(v) => write!(f, "{}", v),
        }
    }
}

fn lexer() -> impl Parser<char, Vec<(Token, Span)>, Error = Simple<char>> {
    let dot_ = just('.').map(|_| Token::Dot);
    let lparen_ = just('(').map(|_| Token::LParen);
    let rparen_ = just(')').map(|_| Token::RParen);
    let ident_ = text::ident().map(|ident: String| match ident.as_str() {
        "let" => Token::Keyword("let".to_string()),
        "in" => Token::Keyword("in".to_string()),
        _ => Token::Ident(ident),
    });
    let operator_ = one_of("+-*/!=")
        .repeated()
        .at_least(1)
        .collect::<String>()
        .map(Token::Operator);
    let int_ = text::int::<char, Simple<char>>(10)
        .from_str()
        .unwrapped()
        .map(Token::Literal);
    let token = lparen_
        .or(rparen_)
        .or(dot_)
        .or(operator_)
        .or(int_)
        .or(ident_)
        .recover_with(skip_then_retry_until([]));
    let comment = just("--").then(take_until(just('\n'))).padded();
    token
        .map_with_span(|tok, span| (tok, span))
        .padded()
        .padded_by(comment.repeated())
        .repeated()
}

fn parser(
) -> impl Parser<Token, Box<Spanned<Expr>>, Error = Simple<Token>> + Clone {
    recursive(|expr| {
        let ident_ =
            select! { Token::Ident(ident) => ident }.labelled("Identifier");

        let lvar_ = select! {
            Token::Ident(ident) => Expr::LVar(ident),
        }
        .labelled("LVar");

        let llit_ =
            select! { Token::Literal(v) => Expr::LInt(v) }.labelled("Literal");

        let labs_ = ident_
            .then_ignore(just(Token::Dot))
            .then(expr.clone())
            .map(|(n, e)| LAbs(n, e));

        let lapp_ = expr
            .clone()
            .then(expr.clone())
            .delimited_by(just(Token::LParen), just(Token::RParen))
            .map(|(e1, e2)| LApp(e1, e2));

        let llet_ = just(Token::Keyword("let".to_string()))
            .ignore_then(ident_)
            .then_ignore(just(Token::Operator("=".to_string())))
            .then(expr.clone())
            .then_ignore(just(Token::Keyword("in".to_string())))
            .then(expr.clone())
            .map(|((name, val), body)| Expr::LLet(name, val, body));

        llet_
            .or(lapp_)
            .or(labs_)
            .or(lvar_)
            .or(llit_)
            .map_with_span(|expr, span| Box::new((expr, span)))
            .recover_with(skip_then_retry_until([]))
    })
}

fn run_parser(input: &str) -> Result<Spanned<Expr>, Vec<Report>> {
    let (tokens, lexer_errs) = lexer().parse_recovery(input);
    let len = input.chars().count();
    let (expr, _parse_errs) = parser().parse_recovery(Stream::from_iter(
        len..len + 1,
        tokens.unwrap().into_iter(),
    ));
    let mut lexer_reports = create_reports(&lexer_errs);
    let mut parser_reports = create_reports(&lexer_errs);
    lexer_reports.append(&mut parser_reports);
    match lexer_reports.is_empty() {
        true => Ok(*expr.unwrap()),
        false => Err(lexer_reports),
    }
}

// ------------------------------------------------
//                    Toplevel
// ------------------------------------------------

fn run_program(prog: &str) -> Result<Spanned<Expr>, Vec<Report>> {
    match run_parser(prog) {
        Ok(expr) => {
            let env = Env::new();
            match norm(&env, expr) {
                Ok(e) => Ok(e),
                Err(e) => Err(vec![e]),
            }
        }
        Err(e) => Err(e),
    }
}

// ------------------------------------------------
//                    Testing
// ------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lexer_0() {
        let test_str = "(x.y)";
        let (_tokens, errs) = lexer().parse_recovery_verbose(test_str);
        assert!(errs.is_empty());
    }

    #[test]
    fn lexer_1() {
        let test_str = "(x.y )";
        let (_tokens, errs) = lexer().parse_recovery(test_str);
        assert!(errs.is_empty());
    }

    #[test]
    fn parser_0() {
        let test_str = "(x.x y)";
        let (tokens, errs) = lexer().parse_recovery(test_str);
        assert!(errs.is_empty());
        let len = test_str.chars().count();
        let (_expr, parse_errs) = parser().parse_recovery(Stream::from_iter(
            len..len + 1,
            tokens.unwrap().into_iter(),
        ));
        let reports = create_reports(&parse_errs);
        print_reports(test_str, reports);
        assert!(parse_errs.is_empty());
    }

    #[test]
    fn parser_1() {
        let test_str = "let z = y.(x.x y) in (z x)";
        let (tokens, errs) = lexer().parse_recovery(test_str);
        assert!(errs.is_empty());
        let len = test_str.chars().count();
        let (_expr, parse_errs) = parser().parse_recovery(Stream::from_iter(
            len..len + 1,
            tokens.unwrap().into_iter(),
        ));
        let reports = create_reports(&parse_errs);
        print_reports(test_str, reports);
        assert!(parse_errs.is_empty());
    }

    #[test]
    fn parser_2() {
        let test_str = "let z = 10 in (x.x z)";
        let (tokens, errs) = lexer().parse_recovery(test_str);
        assert!(errs.is_empty());
        let len = test_str.chars().count();
        let (_expr, parse_errs) = parser().parse_recovery(Stream::from_iter(
            len..len + 1,
            tokens.unwrap().into_iter(),
        ));
        let reports = create_reports(&parse_errs);
        print_reports(test_str, reports);
        assert!(parse_errs.is_empty());
    }

    #[test]
    fn val_0() {
        let test_str = "let z = 10 in (x.x z)";
        match run_program(test_str) {
            Ok(e) => println!("{}", unspan(e)),
            Err(reports) => print_reports(test_str, reports),
        }
    }

    #[test]
    fn val_1() {
        let test_str = "let z = x.x in (z 10)";
        match run_program(test_str) {
            Ok(e) => println!("{}", unspan(e)),
            Err(reports) => print_reports(test_str, reports),
        }
    }
}
