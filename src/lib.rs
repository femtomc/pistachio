use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use chumsky::{prelude::*, stream::Stream};
use std::alloc::{GlobalAlloc, Layout, System};
use std::collections::HashMap;
use std::fmt;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::time::{Duration, Instant};

// ------------------------------------------------
//         (Global mutable integer) gensym
// ------------------------------------------------

// In general, code should not rely on equality checks for
// the value of this global.

static INT: u64 = 0;

fn gensym() -> String {
    let s = format!("x{}", INT);
    INT += 1;
    s
}

// ------------------------------------------------
//             Allocation diagnostics
// ------------------------------------------------

/// An allocator that counts amount of bytes allocated.
pub struct AllocCounter;

static ALLOCATED: AtomicUsize = AtomicUsize::new(0);

unsafe impl GlobalAlloc for AllocCounter {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        ALLOCATED.fetch_add(layout.size(), Ordering::SeqCst);
        System.alloc(layout)
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        System.dealloc(ptr, layout);
    }
}

pub fn reset_allocated() {
    ALLOCATED.store(0, Ordering::SeqCst);
}

pub fn get_allocated() -> usize {
    ALLOCATED.load(Ordering::SeqCst)
}

static A: AllocCounter = AllocCounter;

#[derive(Debug)]
pub struct PhaseStatistics {
    name: &'static str,
    time: Duration,
    allocs: usize,
}

fn record_phase_statistics<R, F: FnOnce() -> R>(
    stats: &mut Vec<PhaseStatistics>,
    phase_name: &'static str,
    phase: F,
) -> R {
    let allocs_before = get_allocated();
    let start_time = Instant::now();
    let ret = phase();
    let elapsed = start_time.elapsed();
    let allocs_after = get_allocated();
    stats.push(PhaseStatistics {
        name: phase_name,
        time: elapsed,
        allocs: allocs_after - allocs_before,
    });
    ret
}

pub fn print_phase_statistics(phase_statistics: &[PhaseStatistics]) {
    let mut total_elapsed: Duration = Duration::from_micros(0);
    let mut total_allocated: usize = 0;
    for PhaseStatistics { name, time, allocs } in phase_statistics {
        println!("| {}: {} µs, {} bytes", name, time.as_micros(), allocs);
        total_elapsed += *time;
        total_allocated += allocs;
    }
}

// ------------------------------------------------
//                Error reporting
// ------------------------------------------------

type Span = std::ops::Range<usize>;
type Spanned<T> = (T, Span);

pub fn unspan<T>(v: Spanned<T>) -> T {
    let (actual, _span) = v;
    actual
}

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

pub fn print_reports(src: &str, reports: Vec<Report>) {
    reports
        .into_iter()
        .for_each(|r| r.print(Source::from(src)).unwrap());
}

// ------------------------------------------------
//          Core language and evaluation
// ------------------------------------------------

type Identifier = String;

type SBox<T> = Box<Spanned<T>>;

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Expr {
    // Core calculus.
    Var(Identifier),
    Pi(Identifier, SBox<Self>, SBox<Self>),
    Lam(Identifier, SBox<Self>),
    App(SBox<Self>, SBox<Self>),
    Sigma(Identifier, SBox<Self>, SBox<Self>),

    // Pair constructors and eliminators.
    Cons(SBox<Self>, SBox<Self>),
    Car(SBox<Self>),
    Cdr(SBox<Self>),

    // Natural number constructors and induction principle.
    Nat,
    Zero,
    Add1(SBox<Self>),
    IndNat(SBox<Self>, SBox<Self>, SBox<Self>, SBox<Self>),

    Same,
    Replace(SBox<Self>, SBox<Self>, SBox<Self>),
    Trivial,
    Sole,
    Absurd,
    IndAbsurd(SBox<Self>, SBox<Self>),
    Atom,
    Quote(Identifier),
    U,
    The(SBox<Self>, SBox<Self>),
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

type Env = HashMap<Identifier, Value>;
type BindingEnv = HashMap<Identifier, Identifier>;

fn extend<T>(
    mut rho: HashMap<Identifier, T>,
    x: Identifier,
    v: T,
) -> HashMap<Identifier, T> {
    rho.insert(x, v);
    rho
}

fn unspan_map<R, T, F: FnOnce(T, T) -> R>(
    e1: Spanned<T>,
    e2: Spanned<T>,
    f: F,
) -> R {
    let e1_ = unspan(e1);
    let e2_ = unspan(e2);
    f(e1_, e2_)
}

fn alpha_equivalence_check_(
    e1: Expr,
    e2: Expr,
    xs1: BindingEnv,
    xs2: BindingEnv,
) -> bool {
    match (e1, e2) {
        (Var(id1), Var(id2)) => id1 == id2,
        (Lam(x1, b1), Lam(x2, b2)) => {
            let fresh = gensym();
            let bigger1 = extend(xs1, x1.to_string(), fresh);
            let bigger2 = extend(xs2, x1.to_string(), fresh);
            unspan_map(*b1, *b2, |b1, b2| {
                alpha_equivalence_check_(b1, b2, bigger1, bigger2)
            })
        }
        (Pi(x, A1, B1), Pi(y, A2, B2)) => {
            let check1 = unspan_map(*A1, *A2, |A1, A2| {
                alpha_equivalence_check_(A1, A2, xs1, xs2)
            });
            let fresh = gensym();
            let bigger1 = extend(xs1, x.to_string(), fresh);
            let bigger2 = extend(xs2, y.to_string(), fresh);
            let check2 = unspan_map(*B1, *B2, |B1, B2| {
                alpha_equivalence_check_(B1, B2, bigger1, bigger2)
            });
            check1 && check2
        }
        (Sigma(x, A1, B1), Sigma(y, A2, B2)) => {
            let check1 = unspan_map(*A1, *A2, |A1, A2| {
                alpha_equivalence_check_(A1, A2, xs1, xs2)
            });
            let fresh = gensym();
            let bigger1 = extend(xs1, x.to_string(), fresh);
            let bigger2 = extend(xs2, y.to_string(), fresh);
            let check2 = unspan_map(*B1, *B2, |B1, B2| {
                alpha_equivalence_check_(B1, B2, bigger1, bigger2)
            });
            check1 && check2
        }
        (Quote(x), Quote(y)) => x == y,
        (The(e1, e1_), The(e2, e2_)) => {
            unspan_map(*e1, *e2, |e1, e2| match (e1, e2) {
                (Absurd, Absurd) => true,
                _ => false,
            })
        }
        (Cons(op1, e1), Cons(op2, e2)) => {}
        (_, _) => false,
    }
}

fn alpha_equivalence_check(e1: Expr, e2: Expr) -> bool {
    let xs1 = BindingEnv::new();
    let xs2 = BindingEnv::new();
    alpha_equivalence_check_(e1, e2, xs1, xs2)
}

#[derive(Debug, Clone)]
struct Clos {
    env: Env,
    var: Option<Identifier>,
    body: Spanned<Expr>,
}

#[derive(Debug, Clone)]
enum Value {
    Pi(Box<Self>, Clos),
    Lam(Clos),
    Sigma(Box<Self>, Clos),
    Pair(Box<Self>, Box<Self>),
    Nat,
    Zero,
    Add1(Box<Self>),
    Eq(Box<Self>, Box<Self>, Box<Self>),
    Same,
    Trivial,
    Sole,
    Absurd,
    Quote,
    Uni,
    Neutral(Box<Self>, Neutral),
    HOClos(Identifier, fn(Value) -> Value),
}

#[derive(Debug, Clone)]
enum Neutral {
    Var(Identifier),
    App(Box<Self>, Normal),
    Car(Box<Self>),
    Cdr(Box<Self>),
    IndNat(Box<Self>, Normal, Normal, Normal),
    Replace(Box<Self>, Normal, Normal),
    IndAbsurd(Box<Self>, Normal),
}

#[derive(Debug, Clone)]
enum Normal {
    The(Box<Value>, Box<Value>),
}

enum Definition {
    Def(Value, Value),
    Bind(Value),
}

type Context = HashMap<Identifier, Definition>;

fn lookup_type(x: Identifier, gamma: Context) -> Result<Value, Report> {
    let d = gamma.get(&x).unwrap();
    match d {
        Definition::Def(v1, v2) => Ok(*v1),
        Definition::Bind(v1) => Ok(*v1),
        _ => Err(Report::build(ReportKind::Error, (), 0)
            .with_message("Unknown variable.")
            .finish()),
    }
}

fn convert_to_env(gamma: Context) -> Env {
    let m: Env = gamma
        .into_iter()
        .map(|(k, v)| match v {
            Definition::Bind(t) => {
                (k, Value::Neutral(Box::new(t), Neutral::Var(k)))
            }
            Definition::Def(_, value) => (k, value),
        })
        .collect();
    m
}

fn extend_ctx(gamma: Context, x: Identifier, t: Value) -> Context {
    gamma.insert(x, Definition::Bind(t));
    gamma
}

fn val_of_closure(c: Value, v: Value) -> Result<Value, Report> {
    match c {
        Value::Lam(Clos {
            env: rho,
            var: x,
            body: b,
        }) => {
            let new = extend(rho, x.unwrap(), v);
            val(&rho, b)
        }
        Value::HOClos(x, f) => Ok(f(v)),
    }
}

// ------------------------------------------------
//           Bidirectional type system
// ------------------------------------------------

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
        "define" => Token::Keyword("define".to_string()),
        "U" => Token::Keyword("U".to_string()),
        "Nat" => Token::Keyword("Nat".to_string()),
        "zero" => Token::Keyword("zero".to_string()),
        "add1" => Token::Keyword("add1".to_string()),
        "ind_Nat" => Token::Keyword("ind_Nat".to_string()),
        "Sigma" => Token::Keyword("Sigma".to_string()),
        "cons" => Token::Keyword("cons".to_string()),
        "car" => Token::Keyword("car".to_string()),
        "cdr" => Token::Keyword("cdr".to_string()),
        "Pi" => Token::Keyword("Pi".to_string()),
        "lambda" => Token::Keyword("lambda".to_string()),
        "=" => Token::Keyword("=".to_string()),
        "same" => Token::Keyword("same".to_string()),
        "replace" => Token::Keyword("replace".to_string()),
        "Trivial" => Token::Keyword("Trivial".to_string()),
        "sole" => Token::Keyword("sole".to_string()),
        "Absurd" => Token::Keyword("Absurd".to_string()),
        "ind_Absurd" => Token::Keyword("ind_Absurd".to_string()),
        "Atom" => Token::Keyword("Atom".to_string()),
        "quote" => Token::Keyword("quote".to_string()),
        "the" => Token::Keyword("the".to_string()),
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
    let (expr, parse_errs) = parser().parse_recovery(Stream::from_iter(
        len..len + 1,
        tokens.unwrap().into_iter(),
    ));
    let mut lexer_reports = create_reports(&lexer_errs);
    let mut parser_reports = create_reports(&parse_errs);
    lexer_reports.append(&mut parser_reports);
    match lexer_reports.is_empty() {
        true => Ok(*expr.unwrap()),
        false => Err(lexer_reports),
    }
}

// ------------------------------------------------
//                    Toplevel
// ------------------------------------------------

pub fn run_program(
    prog: &str,
) -> Result<(Spanned<Expr>, Vec<PhaseStatistics>), Vec<Report>> {
    let len = prog.chars().count();
    let mut phase_statistics: Vec<PhaseStatistics> = Vec::with_capacity(10);

    // Lexing.
    let (tokens, lexer_errs) =
        record_phase_statistics(&mut phase_statistics, "lexing", || {
            lexer().parse_recovery(prog)
        });

    // Parsing.
    let (expr, parse_errs) =
        record_phase_statistics(&mut phase_statistics, "parsing", || {
            parser().parse_recovery(Stream::from_iter(
                len..len + 1,
                tokens.unwrap().into_iter(),
            ))
        });
    let mut lexer_reports = create_reports(&lexer_errs);
    let mut parser_reports = create_reports(&parse_errs);
    lexer_reports.append(&mut parser_reports);
    if !lexer_reports.is_empty() {
        return Err(lexer_reports);
    }

    // Evaluation.
    let env = Env::new();
    let v = match record_phase_statistics(
        &mut phase_statistics,
        "evaluation",
        || norm(&env, *expr.unwrap()),
    ) {
        Ok(v) => Ok(v),
        Err(e) => Err(vec![e]),
    }?;

    Ok((v, phase_statistics))
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
}
