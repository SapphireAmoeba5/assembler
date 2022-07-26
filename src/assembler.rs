mod symbol_table;
mod token;

use std::borrow::Cow;
use std::fs::{read_to_string, File};
use std::time::Instant;

use symbol_table::{Constant, Label, Symbol, SymbolTable};

pub struct Assembler {
    input_file: String,
    output_file: String,

    symbol_table: SymbolTable,
}

type AssemblerError = Result<(), Cow<'static, str>>;

impl Assembler {
    /// Returns a vector containing each error produced. Returns 0 length vector on successful compilation
    pub fn assemble(
        input_file: String,
        output_file: String,
    ) -> Vec<(Cow<'static, str>, String, usize)> {
        let mut errors: Vec<(Cow<'static, str>, String, usize)> = Vec::new();

        let mut this = Self {
            input_file: input_file,
            output_file: output_file,

            symbol_table: SymbolTable::new(),
        };
        errors.extend(this.first_pass());
        errors.extend(this.second_pass());

        errors
    }
}

impl Assembler {
    /// The first pass of the assembler determines the address of each label, and other assembler similar assembler constructs
    fn first_pass(&mut self) -> Vec<(Cow<'static, str>, String, usize)> {
        // Create a vector with an arbitrary capacity
        let mut errors: Vec<(Cow<'static, str>, String, usize)> = Vec::with_capacity(10);

        let assembly_source = match read_to_string(&self.input_file) {
            Ok(s) => s,
            Err(e) => {
                println!("Cannot open file. Error: \"{}\"", e);
                return Vec::new();
            }
        };

        errors
    }
    /// The second pass converts the previously processed source into actual machine code
    fn second_pass(&mut self) -> Vec<(Cow<'static, str>, String, usize)> {
        let mut errors: Vec<(Cow<'static, str>, String, usize)> = Vec::new();

        errors
    }

    fn lex_line<'a>(line: &'a str) -> Vec<&'a str> {
        let mut result: Vec<&'a str> = Vec::with_capacity(line.len());

        let mut in_string = false;
        let mut escaped = false;

        let mut start = 0;
        for (i, ch) in line.char_indices() {
            if ch == '"' && !escaped {
                in_string = !in_string;
                escaped = false;
            } else if !in_string {
                if ch.is_whitespace() {
                    let slice = &line[start..i];

                    if slice.trim().is_empty() {
                        start = i + 1;
                        continue;
                    }

                    result.push(slice);
                    start = i + 1;
                }
                if ch == ',' {
                    let slice = &line[start..i];
                    if !slice.trim().is_empty() {
                        result.push(slice);
                    }
                    result.push(&line[i..i + 1]);
                    start = i + 1;
                }
            }
        }
        result.push(&line[start..]);

        result
    }
}
