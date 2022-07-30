mod assembler;
mod logger;

use clap::Parser;

use assembler::Assembler;

#[derive(Parser, Debug)]
struct Args {
    #[clap()]
    input: String,

    #[clap(short, long, default_value = "a.out")]
    output: String,
}

fn main() {
    let args = Args::parse();

    let errors = Assembler::assemble(args.input, args.output);

    for (error, file, line) in errors.iter() {
        println!("[Error] {} {}:{}", error, file, line);
    }
}
