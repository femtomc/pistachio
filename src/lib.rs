use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use chumsky::{prelude::*, stream::Stream};
use std::collections::HashMap;
use std::fmt;

// ------------------------------------------------
//             For good error reporting
// ------------------------------------------------

type Span = std::ops::Range<usize>;
type Spanned<T> = (T, Span);

fn create_reports<T>(src: &str, errs: &[Simple<T>]) -> Vec<Report>
where
    T: PartialEq + Eq + std::hash::Hash + std::fmt::Display,
{
    errs.into_iter()
        .map(|e| {
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

            report.finish()
        })
        .collect()
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
struct Closure {
    env: Env,
    var: Option<Symbol>,
    body: Spanned<Expr>,
}

#[derive(Debug, Clone)]
enum Value {
    Int(i64),
    Closure(Closure),
}

fn extend(mut rho: Env, x: Symbol, v: Value) -> Env {
    rho.insert(x, v);
    return rho;
}

fn val(rho: &Env, e: Spanned<Expr>) -> Result<Value, Report> {
    match e {
        (LInt(v), _) => Ok(Value::Int(v)),
        (LVar(x), _) => Ok(rho.get(&x).unwrap().clone()),
        (LAbs(x, b), _) => Ok(Value::Closure(Closure {
            env: rho.clone(),
            var: Some(x),
            body: *b.clone(),
        })),
        (LApp(e1, e2), _) => {
            let v1 = val(&rho, *e1)?;
            let v2 = val(&rho, *e2)?;
            apply(v1, v2)
        }
        (LLet(v, e, b), _) => {
            let evaluated = val(rho, *e)?;
            let rho_new = extend(rho.clone(), v, evaluated);
            val(&rho_new, *b)
        }
    }
}

fn apply(clos: Value, arg: Value) -> Result<Value, Report> {
    match clos {
        Value::Closure(Closure {
            env: rho,
            var: v,
            body: b,
        }) => Ok(val(&extend(rho, v.unwrap(), arg), b)?),
        _ => Err({
            Report::build(ReportKind::Error, (), 0)
                .with_message("Attempted to apply non-closure value.")
                .finish()
        }),
    }
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
        let ident_ = select! { Token::Ident(ident) => ident.clone() }
            .labelled("Identifier");

        let lvar_ = select! {
            Token::Ident(ident) => Expr::LVar(ident.clone()),
        }
        .labelled("LVar");

        let llit_ = select! { Token::Literal(v) => Expr::LInt(v.clone()) }
            .labelled("Literal");

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
    let (tokens, mut lexer_errs) = lexer().parse_recovery(input);
    let len = input.chars().count();
    let (expr, parse_errs) = parser().parse_recovery(Stream::from_iter(
        len..len + 1,
        tokens.unwrap().into_iter(),
    ));
    let mut lexer_reports = create_reports(input, &lexer_errs);
    let mut parser_reports = create_reports(input, &lexer_errs);
    lexer_reports.append(&mut parser_reports);
    match lexer_reports.is_empty() {
        true => Ok(*expr.unwrap()),
        false => Err(lexer_reports),
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
    fn parser_0() {
        let test_str = "(x.x y)";
        let (tokens, errs) = lexer().parse_recovery(test_str);
        assert!(errs.is_empty());
        let len = test_str.chars().count();
        let (expr, parse_errs) = parser().parse_recovery(Stream::from_iter(
            len..len + 1,
            tokens.unwrap().into_iter(),
        ));
        let reports = create_reports(test_str, &parse_errs);
        reports
            .into_iter()
            .for_each(|r| r.print(Source::from(test_str)).unwrap());
        assert!(parse_errs.is_empty());
    }

    #[test]
    fn parser_1() {
        let test_str = "let z = y.(x.x y) in (z x)";
        let (tokens, errs) = lexer().parse_recovery(test_str);
        assert!(errs.is_empty());
        let len = test_str.chars().count();
        let (expr, parse_errs) = parser().parse_recovery(Stream::from_iter(
            len..len + 1,
            tokens.unwrap().into_iter(),
        ));
        let reports = create_reports(test_str, &parse_errs);
        reports
            .into_iter()
            .for_each(|r| r.print(Source::from(test_str)).unwrap());
        assert!(parse_errs.is_empty());
    }

    #[test]
    fn parser_2() {
        let test_str = "let z = 10 in (x.x z)";
        let (tokens, errs) = lexer().parse_recovery(test_str);
        println!("{:?}", tokens);
        assert!(errs.is_empty());
        let len = test_str.chars().count();
        let (expr, parse_errs) = parser().parse_recovery(Stream::from_iter(
            len..len + 1,
            tokens.unwrap().into_iter(),
        ));
        let reports = create_reports(test_str, &parse_errs);
        reports
            .into_iter()
            .for_each(|r| r.print(Source::from(test_str)).unwrap());
        assert!(parse_errs.is_empty());
    }

    #[test]
    fn val_0() {
        let test_str = "let z = 10 in (x.x z)";
        match run_parser(test_str) {
            Ok(e) => {
                println!("{}", e.0);
                let env = Env::new();
                match val(&env, e) {
                    Ok(v) => println!("{:?}", v),
                    Err(_) => (),
                }
            }
            Err(_) => (),
        }
    }
}
