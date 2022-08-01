use std::borrow::Cow;
use super::{AssemblerResult, AssemblerReturn, symbol_table::SymbolTable, token::Token};


pub struct FirstPass {
    pub symbol_table: SymbolTable,
    pub tokens: Vec<Token>,
    offset: usize,
}

impl FirstPass {
    pub fn begin(input_file: &str) -> AssemblerReturn {
        let assembly_source = match std::fs::read_to_string(input_file) {
            Ok(s) => s,
            Err(e) => return vec![(Cow::from("Unable to open input source file"), None)],
        };

        let mut this = Self {
            symbol_table: SymbolTable::new(),
            tokens: Vec::new(),
            offset: 0,
        };

        this.first_pass(&assembly_source)
    }
}

impl FirstPass {
    fn first_pass(&mut self, source: &str) -> AssemblerReturn {
        let errors: AssemblerReturn = Vec::new();

        for line in source.lines() {
            let lexed_line = Self::lex_line(line);
            println!("{:?}", lexed_line);
        }

        errors
    }
}

impl FirstPass {
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
                if ch == ',' || ch == '[' || ch == ']' {
                    let slice = &line[start..i];
                    if !slice.trim().is_empty() {
                        result.push(slice);
                    }
                    result.push(&line[i..i + 1]);
                    start = i + 1;
                }
            } else if ch == '\\' {
                if !escaped {
                    escaped = true;
                } else {
                    escaped = false;
                }
            }
        }
        result.push(&line[start..]);

        result
    }
}
