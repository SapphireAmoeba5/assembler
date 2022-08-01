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

    for (error, location) in errors.iter() {
        print!("[Error] {}", error);

        if let Some((file, line_number)) = location {
            println!(" {}:{}", file, line_number);
        } else {
            println!();
        }
    }
}
