#![feature(box_patterns)]

mod backend;
mod frontend;

use frontend::anormal::anormal;
use frontend::lexer::{tokenize, Token};
use frontend::lowering::lower_pgm;
use frontend::typecheck::typecheck_pgm;
use std::fs::File;
use std::io::Write;
use std::path::Path;
use std::process::Command;
use std::time::{Duration, Instant};

// Control the code generation path.
#[cfg(feature = "cranelift-codegen")]
use backend::cranelift::codegen;

#[cfg(feature = "llvm-codegen")]
use backend::llvm::codegen;

#[cfg(debug_assertions)]
#[global_allocator]
static A: frontend::diagnostics::AllocCounter = frontend::diagnostics::AllocCounter;

#[derive(Debug)]
struct PassStats {
    name: &'static str,
    time: Duration,
    allocs: usize,
}

fn record_pass_stats<A, F: FnOnce() -> A>(
    stats: &mut Vec<PassStats>,
    pass_name: &'static str,
    pass: F,
) -> A {
    let allocs_before = frontend::diagnostics::get_allocated();
    let start_time = Instant::now();
    let ret = pass();
    let elapsed = start_time.elapsed();
    let allocs_after = frontend::diagnostics::get_allocated();
    stats.push(PassStats {
        name: pass_name,
        time: elapsed,
        allocs: allocs_after - allocs_before,
    });
    ret
}

type ObjectCode = Vec<u8>;

fn compile_expr(
    expr_str: &str,
    out_dir: &str,
    dump_cc: bool,
    dump_cg: bool,
    dump_pass_stats: bool,
) -> Option<ObjectCode> {
    let mut pass_stats: Vec<PassStats> = Vec::with_capacity(10);

    let tokens: Vec<Token> =
        match record_pass_stats(&mut pass_stats, "lexing", || tokenize(expr_str)) {
            Err(err) => {
                println!("Lexer error: {:#?}", err);
                return None;
            }
            Ok(tokens) => tokens,
        };

    let mut ctx = Default::default();

    let expr = match record_pass_stats(&mut pass_stats, "parsing", || {
        frontend::parser::Expr::parse(tokens.into_iter().map(Ok::<_, ()>))
    }) {
        Err(err) => {
            println!("Parser error: {:#?}", err);
            return None;
        }
        Ok(expr) => expr,
    };

    let mut expr = record_pass_stats(&mut pass_stats, "interning", || expr.intern(&mut ctx));

    match record_pass_stats(&mut pass_stats, "type checking", || {
        typecheck_pgm(&mut ctx, &mut expr)
    }) {
        Err(err) => {
            println!("Type error: {:#?}", err);
            return None;
        }
        Ok(()) => {}
    };

    let expr = record_pass_stats(&mut pass_stats, "a-normalization", || {
        anormal(&mut ctx, expr)
    });

    let (funs, main) = record_pass_stats(&mut pass_stats, "closure conversion", || {
        lower_pgm(&mut ctx, expr)
    });

    if dump_cc {
        let mut file = File::create(format!("{}/{}", out_dir, "emitted.cfg")).unwrap();
        let mut s = String::new();
        for fun in &funs {
            fun.pp(&ctx, &mut s).unwrap();
        }
        file.write_all(s.as_bytes());
    }

    let object_code = record_pass_stats(&mut pass_stats, "code generation", || {
        codegen(&mut ctx, &funs, main, out_dir, dump_cg)
    });

    if dump_pass_stats {
        report_pass_stats(out_dir, &pass_stats);
    }

    Some(object_code)
}

fn report_pass_stats(out_dir: &str, pass_stats: &[PassStats]) {
    // TODO: align columns
    // TODO: show percentage of allocs and times of each pass
    // TODO: maintain a counter for max res?
    let mut file = File::create(format!("{}/{}", out_dir, "compiler_stats.csv")).unwrap();
    let mut total_elapsed: Duration = Duration::from_micros(0);
    let mut total_allocated: usize = 0;
    for PassStats { name, time, allocs } in pass_stats {
        file.write_all(format!("{}, {}, {}\n", name, time.as_millis(), allocs,).as_bytes());
        total_elapsed += *time;
        total_allocated += allocs;
    }

    file.write_all(format!("total, {}, {}", total_elapsed.as_millis(), total_allocated).as_bytes());
}

pub fn compile_file(
    path: &str,
    out_dir: Option<&str>,
    dump_cc: bool,
    dump_cg: bool,
    dump_pass_stats: bool,
) -> i32 {
    let out_dir = out_dir.unwrap_or(".");
    let contents = std::fs::read_to_string(path).unwrap();
    match compile_expr(&contents, out_dir, dump_cc, dump_cg, dump_pass_stats) {
        None => 1,
        Some(object_code) => link(path, out_dir, object_code),
    }
}

fn link(path: &str, out_dir: &str, object_code: ObjectCode) -> i32 {
    let path = Path::new(path);
    let file_stem = path.file_stem().unwrap().to_str().unwrap();
    let o_file_name = format!("{}.o", file_stem);

    File::create(&format!("{}/{}", out_dir, o_file_name))
        .unwrap()
        .write_all(&object_code)
        .unwrap();

    // Build runtime
    let output = Command::new("clang")
        .args(&[
            "runtime.c",
            "-g",
            "-c",
            "-o",
            &format!("{}/runtime.o", out_dir),
        ])
        .spawn()
        .unwrap()
        .wait_with_output()
        .unwrap();

    assert!(output.status.success());

    // Link
    let output = Command::new("clang")
        .args(&[
            &o_file_name,
            "runtime.o",
            "-o",
            file_stem,
            "-lm", // link math library
        ])
        .current_dir(out_dir)
        .spawn()
        .unwrap()
        .wait_with_output()
        .unwrap();

    assert!(output.status.success());
    return 0;
}
