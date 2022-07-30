use crate::debug_println;
use std::borrow::Cow;
use std::fs::{read_to_string, File};
use std::str::FromStr;

use super::register::Register;
use super::size::Size;
use super::symbol_table::{Constant, Label, Symbol, SymbolTable};
use super::try_parse::{try_parse_identifier, try_parse_number, try_parse_string};
use super::{AssemblerResult, Instruction, Token};

pub struct FirstPass {
    symbols: SymbolTable,
    tokens: Vec<Token>,
    byte_offset: usize,
}

impl FirstPass {
    pub fn begin(input_file: &str) -> Vec<(Cow<'static, str>, String, usize)> {
        let mut this = Self {
            symbols: SymbolTable::new(),
            tokens: Vec::new(),
            byte_offset: 0,
        };

        this.first_pass(input_file)
    }
}

impl FirstPass {
    /// The first pass will save each identifier to a symbol table, and determine the address of each label, and tokenize the file including converting special characters such as '$' into their correct constants
    fn first_pass(&mut self, input_file: &str) -> Vec<(Cow<'static, str>, String, usize)> {
        // Create a vector with an arbitrary capacity
        let mut errors: Vec<(Cow<'static, str>, String, usize)> = Vec::with_capacity(10);

        let assembly_source = match read_to_string(input_file) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("Cannot open file. Error: \"{}\"", e);
                return Vec::new();
            }
        };

        for (line_idx, line) in assembly_source.lines().enumerate() {
            let line_number = line_idx + 1;
            let lexed = Self::lex_line(line);

            if line.is_empty() {
                continue;
            }

            let tokenized = match self.tokenize_lexed_line(&lexed, line_number) {
                Ok(toks) => {
                    debug_println!("Parsed tokens {:#?}", toks);
                    toks
                }
                Err(e) => {
                    errors.push((e, input_file.into(), line_number));
                    continue;
                }
            };

            self.calculate_new_byte_offset(&tokenized);
            self.tokens.extend(tokenized);
        }

        errors
    }

    fn calculate_new_byte_offset(&mut self, tokenized: &Vec<Token>) -> AssemblerResult<()> {
        for token in tokenized {
            let width = self.get_width_of_token(token)?;
            self.byte_offset += width;
        }

        Ok(())
    }

    fn get_width_of_token(&self, token: &Token) -> AssemblerResult<usize> {
        match token {
            Token::ConstantInteger {
                value: _,
                size: size,
                line_number: _,
            } => Ok(size.clone() as usize),
            Token::ConstantString {
                value: value,
                line_number: _,
            } => Ok(value.len()),
            Token::Identifier {
                identifier: identifier,
                line_number: _,
            } => Ok(8),
            Token::MemoryReserve {
                value: value,
                size: size,
                line_number: _,
            } => {
                let sizeof_value = self.get_width_of_token(value)?;
                let number_of_elements = match &**size {
                    Token::ConstantInteger {
                        value: _,
                        size: size,
                        line_number: _,
                    } => size.clone() as usize,
                    Token::Identifier {
                        identifier: identifier,
                        line_number: _,
                    } => match self.symbols.get(&identifier) {
                        Some(Symbol::SymbolConstant(constant)) => constant.value,
                        Some(Symbol::SymbolLabel(_)) => {
                            return Err(Cow::from(
                                "Cannot use a label as element size of memory reserve",
                            ))
                        }
                        None => {
                            return Err(Cow::from(
                                "Constant must be declared before using it in memory reserve",
                            ))
                        }
                    },

                    _ => return Err(Cow::from(
                        "You can only use a constant to initialize the size of a memory reserve",
                    )),
                };

                Ok(sizeof_value * number_of_elements)
            }
            _ => {Ok(10)}
        }
    }

    fn tokenize_lexed_line(
        &mut self,
        mut lexed_line: &[&str],
        line_number: usize,
    ) -> AssemblerResult<Vec<Token>> {
        let mut tokens: Vec<Token> = Vec::with_capacity(lexed_line.len());

        if lexed_line[0].ends_with(':') {
            let label_name = &lexed_line[0][0..lexed_line[0].len() - 1];
            self.parse_label(label_name, line_number)?;
            lexed_line = &lexed_line[1..];

            if lexed_line.is_empty() {
                return Ok(tokens);
            }
        }

        match Instruction::from_str(lexed_line[0]) {
            Ok(instruction) => {
                tokens.push(self.tokenize_instruction(
                    instruction,
                    &lexed_line[1..],
                    line_number,
                )?);
                return Ok(tokens);
            }
            Err(_) => {}
        }

        for word in lexed_line.iter() {
            let token = self.tokenize_single_word(word, line_number)?;
            tokens.push(token);
        }

        Ok(tokens)
    }

    fn parse_label(&mut self, label_name: &str, line_number: usize) -> AssemblerResult<()> {
        match try_parse_identifier(label_name) {
            Ok(label_name) => {
                debug_println!(
                    "Adding label \"{}\" with byte offset {}",
                    label_name,
                    self.byte_offset
                );
                if self.symbols.add_label(Label::new(
                    label_name.into(),
                    self.byte_offset,
                    line_number,
                )) == false {
                    return Err(Cow::from("Duplicate label identifier name"));
                }
                Ok(())
            }
            Err(e) => Err(e),
        }
    }

    fn tokenize_instruction(
        &mut self,
        instruction: Instruction,
        operands: &[&str],
        line_number: usize,
    ) -> AssemblerResult<Token> {
        let operands = operands.split(|word| word == &",").collect::<Vec<_>>();

        let mut tokenized_operands: Vec<Token> = Vec::with_capacity(operands.len());
        for operand in operands {
            if operand.len() > 1 {
                return Err(Cow::from("Operands must be seperated by a comma"));
            }

            tokenized_operands.push(self.tokenize_single_word(operand[0], line_number)?);
        }

        Ok(Token::Instruction {
            instruction,
            arguments: tokenized_operands,
            line_number,
        })
    }

    // This does not tokenize tokens that require other lexed words to be able to properly tokenize.
    fn tokenize_single_word(&mut self, word: &str, line_number: usize) -> AssemblerResult<Token> {
        match try_parse_number(word) {
            Ok(Some((value, size))) => {
                return Ok(Token::ConstantInteger {
                    value: Self::trunucate_integer_to_size(value, size),
                    size,
                    line_number,
                })
            }
            Err(e) => return Err(e),
            Ok(None) => {}
        }

        match Register::from_str(word) {
            Ok(register) => {
                return Ok(Token::Register {
                    register,
                    line_number,
                })
            }
            _ => {}
        }

        match try_parse_string(word) {
            Ok(Some(value)) => {
                return Ok(Token::ConstantString {
                    value: value.into(),
                    line_number,
                })
            }
            Err(e) => return Err(e),
            Ok(None) => {}
        }

        match try_parse_identifier(word) {
            Ok(identifier) => Ok(Token::Identifier {
                identifier: identifier.into(),
                line_number,
            }),
            Err(e) => Err(e),
        }
    }

    fn trunucate_integer_to_size(integer: u64, size: Size) -> u64 {
        match size {
            Size::One => integer as u8 as u64,
            Size::Two => integer as u16 as u64,
            Size::Four => integer as u32 as u64,
            Size::Eight => integer,
        }
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
