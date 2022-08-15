mod assembler;
mod logger;

use assembler::Assembler;
use clap::Parser;
use std::time::Instant;

#[derive(Parser, Debug)]
struct Args {
    #[clap()]
    input: String,

    #[clap(short, long, default_value = "a.out")]
    output: String,
}

fn main() {
    let args = Args::parse();

    let timer = Instant::now();
    let errors = Assembler::assemble(args.input, args.output);
    let elapsed = timer.elapsed();

    if errors.is_empty() {
        println!(
            "Time taken: {}s({}ms)",
            elapsed.as_secs_f64(),
            elapsed.as_millis()
        );
    }

    for (error, location) in errors.iter() {
        print!("[Error] {}", error);

        if let Some((file, line_number)) = location {
            println!(" [{}:{}]", file, line_number);
        } else {
            println!();
        }
    }
}
