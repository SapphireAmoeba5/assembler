mod first_pass;
mod instruction;
mod register;
mod size;
mod symbol_table;
mod token;
mod try_parse;

use crate::debug_println;
use instruction::Instruction;
use size::Size;
use std::borrow::Cow;
use std::time::Instant;
use token::Token;

use symbol_table::{Constant, Label, Symbol, SymbolTable};

use self::first_pass::{FirstPass, TokenEntry};

pub struct Assembler {
    input_file: String,
    output_file: String,

    assembled_data: Vec<u8>,
    first_pass: FirstPass,
}

type AssemblerResult<T> = Result<T, Cow<'static, str>>;
type AssemblerReturn = Vec<(Cow<'static, str>, Option<(String, usize)>)>;

impl Assembler {
    /// Returns a vector containing each error produced. Returns 0 length vector on successful compilation
    pub fn assemble(input_file: String, output_file: String) -> AssemblerReturn {
        let mut errors: AssemblerReturn = Vec::new();

        let mut this = Self {
            input_file: input_file.clone(),
            output_file: output_file,
            assembled_data: Vec::with_capacity(100 * 1024), /* 100 kilobytes of data. */
            first_pass: FirstPass::new(input_file),
        };

        errors.extend(this.first_pass.begin());

        // Skip the assembling process if there are any errors in the first pass
        if !errors.is_empty() {
            return errors;
        }

        errors.extend(this.assemble_impl());

        errors
    }
}

impl Assembler {
    fn assemble_impl(&mut self) -> AssemblerReturn {
        let mut errors: AssemblerReturn = Vec::with_capacity(10);

        for token in self.first_pass.tokens.iter() {
            match self.assemble_token(token) {
                Ok(mut bytes) => {
                    debug_println!("Assembled bytes: {:02x?}", bytes);
                    self.assembled_data.append(&mut bytes);
                }
                Err(e) => {
                    errors.push((e, Some((self.input_file.clone(), token.line_number))));
                }
            }
        }

        errors
    }

    fn assemble_token(&self, token_entry: &TokenEntry) -> AssemblerResult<Vec<u8>> {
        let assembled = match token_entry.token {
            Token::ConstantInteger { value, size } => self.assemble_constant_integer(value, size),
            _ => panic!(
                "Found token that could not turned into bytes! {:#?}",
                token_entry
            ),
        };

        Ok(assembled)
    }
}

impl Assembler {
    fn assemble_constant_integer(&self, value: u64, size: Size) -> Vec<u8> {
        value.to_le_bytes()[0..size as usize].into()
    }

    fn assemble_constant_string(&self, string: &str) -> Vec<u8> {
    }
}
