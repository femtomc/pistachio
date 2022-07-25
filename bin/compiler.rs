use std::fs;
use std::process::exit;

fn main() {
    //libstachio::diagnostics::diagnostics_setup().unwrap();
    let args: Vec<String> = std::env::args().collect();
    let outdir = ".pi-compiled";
    fs::create_dir_all(outdir).unwrap();
    match args.as_slice() {
        [_, ref file] => {
            exit(libstachio::compile_file(
                file,
                Some(outdir),
                true,
                true,
                true,
            ));
        }
        _ => {
            println!("What do you mean?");
            exit(1);
        }
    }
}
