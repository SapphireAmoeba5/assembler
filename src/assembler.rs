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

use self::first_pass::FirstPass;

pub struct Assembler {
    input_file: String,
    output_file: String,
}

type AssemblerResult<T> = Result<T, Cow<'static, str>>;
type AssemblerReturn = Vec<(Cow<'static, str>, Option<(String, usize)>)>;

impl Assembler {
    /// Returns a vector containing each error produced. Returns 0 length vector on successful compilation
    pub fn assemble(
        input_file: String,
        output_file: String,
    ) -> AssemblerReturn {
        let mut errors: AssemblerReturn = Vec::new();

        let mut this = Self {
            input_file: input_file,
            output_file: output_file,
        };

        errors.extend(FirstPass::begin(&this.input_file));

        errors
    }
}

impl Assembler {}
