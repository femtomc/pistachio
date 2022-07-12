
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;


fn main() -> std::io::Result<()> {
    let args: Vec<String> = std::env::args().collect();
    match args.as_slice() {
        [_, ref file] => {
            let file = File::open(file)?;
            let mut buf_reader = BufReader::new(file);
            let mut contents = String::new();
            buf_reader.read_to_string(&mut contents)?;
            match libstachio::run_program(&contents) {
                Ok(v) => println!("{}", libstachio::unspan(v)),
                Err(e) => libstachio::print_reports(&contents, e),
            }
        }
        _ => {
            println!("Error processing arguments to compiler.")
        }
    }
    Ok(())
}
