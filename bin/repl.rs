use reedline::{DefaultPrompt, Reedline, Signal};
use std::io;

fn main() -> io::Result<()> {
    let mut line_editor = Reedline::create();
    let prompt = DefaultPrompt::default();

    loop {
        let sig = line_editor.read_line(&prompt)?;
        match sig {
            Signal::Success(buffer) => match libstachio::run_program(&buffer) {
                Ok((v, phase_statistics)) => {
                    println!("{}", libstachio::unspan(v))
                }
                Err(e) => libstachio::print_reports(&buffer, e),
            },
            Signal::CtrlD | Signal::CtrlC => {
                println!("Bye now!");
                break Ok(());
            }
        }
    }
}
