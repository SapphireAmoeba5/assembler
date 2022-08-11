use super::instruction::Instruction;
use super::register::Register;
use super::try_parse::{try_parse_identifier, try_parse_number, try_parse_string};
use super::{symbol_table::SymbolTable, token::Token, AssemblerResult, AssemblerReturn};
use crate::debug_println;
use std::borrow::Cow;
use std::str::FromStr;

#[derive(Debug)]
pub struct TokenEntry {
    pub token: Token,
    pub line_number: usize,
    pub offset: usize,
    pub width: usize,
}

impl TokenEntry {
    pub fn new(token: Token, line_number: usize, offset: usize, width: usize) -> Self {
        Self {
            token,
            line_number,
            offset,
            width,
        }
    }
}

pub struct FirstPass {
    pub input_file: String,

    pub symbol_table: SymbolTable,
    pub tokens: Vec<TokenEntry>,
    offset: usize,
}

impl FirstPass {
    pub fn new(input_file: String) -> Self {
        Self {
            input_file,

            symbol_table: SymbolTable::new(),
            tokens: Vec::new(),
            offset: 0,
        }
    }

    pub fn begin(&mut self) -> AssemblerReturn {
        let assembly_source = match std::fs::read_to_string(&self.input_file) {
            Ok(s) => s,
            Err(e) => return vec![(Cow::from("Unable to open input source file"), None)],
        };

        self.first_pass(&assembly_source)
    }
}

impl FirstPass {
    fn first_pass(&mut self, source: &str) -> AssemblerReturn {
        let mut errors: AssemblerReturn = Vec::with_capacity(10);

        for (line_index, line) in source.lines().filter(|s| !s.is_empty()).enumerate() {
            let line_number = line_index + 1;
            let lexed_line = Self::lex_line(line);

            debug_println!("{:?}", lexed_line);

            let token = match self.parse_lexed_line(&lexed_line) {
                Ok(tokens) => tokens,
                Err(e) => {
                    errors.push((e, Some((self.input_file.clone(), line_number))));
                    continue;
                }
            };
            debug_println!("{:?}\n", token);

            let width = match Self::get_width_of_token(&token) {
                Ok(width) => width,
                Err(e) => {
                    errors.push((e, Some((self.input_file.clone(), line_number))));
                    continue;
                }
            };
            let token_entry = TokenEntry::new(token, line_number, self.offset, width);
            self.tokens.push(token_entry);
            self.offset += width;
        }

        errors
    }

    fn get_width_of_token(token: &Token) -> AssemblerResult<usize> {
        Ok(0)
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
                if ch == ',' || ch == '[' || ch == ']' || ch == '{' || ch == '}' {
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
        let slice = &line[start..];
        if !slice.is_empty() {
            result.push(&line[start..]);
        }

        result
    }

    // Each line should corrospond to a single token. line is garunteed to at least be a size of 1
    fn parse_lexed_line(&mut self, line: &[&str]) -> AssemblerResult<Token> {
        if line[0].starts_with('.') {
            let directive: String = line[0].to_string();
            let arguments: Vec<String> = line[1..].iter().map(|s| (*s).into()).collect();
            return Ok(Token::Directive {
                directive,
                arguments,
            });
        }

        let result = self.memory_reserve_from_lexed(line)?;
        if let Some(reserve) = result {
            return Ok(reserve);
        }

        if line.len() == 1 {
            match try_parse_number(line[0]) {
                Ok(Some((value, size))) => return Ok(Token::ConstantInteger { value, size }),
                Err(e) => return Err(e),
                Ok(None) => {}
            }

            let result = try_parse_number(line[0])?;
            if let Some((value, size)) = result {
                return Ok(Token::ConstantInteger { value, size });
            }

            let result = try_parse_string(line[0])?;
            if let Some(value) = result {
                return Ok(Token::ConstantString {
                    value: value.to_string(),
                });
            }

            match Register::from_str(line[0]) {
                Ok(register) => return Ok(Token::Register { register }),
                Err(_) => {}
            }

            match try_parse_identifier(line[0]) {
                Ok(identifier) => {
                    return Ok(Token::Identifier {
                        identifier: identifier.into(),
                    })
                }
                Err(_) => {}
            }
        }

        let result = self.instruction_from_lexed(line)?;
        if let Some(instruction) = result {
            return Ok(instruction);
        }

        // Flatten the line into a single string to output as an error
        let flat_line = line
            .iter()
            .fold(String::with_capacity(line.len() * 5), |acc, s| {
                if acc.is_empty() {
                    format!("{}", s)
                } else {
                    format!("{} {}", acc, s)
                }
            });
        Err(Cow::from(format!("Error at \"{}\"", flat_line)))
    }

    // Garunteed to return Token::MemoryReserve if function succeeds and returns Some
    fn memory_reserve_from_lexed(&mut self, lexed: &[&str]) -> AssemblerResult<Option<Token>> {
        if lexed[0] != "{" {
            return Ok(None);
        }

        if lexed.iter().rev().next().unwrap() != &"}" {
            return Err(Cow::from("No closing brace for memory reserve"));
        }

        let lexed = &lexed[1..lexed.len() - 1];

        let mut arguments: Vec<Token> = Vec::with_capacity(lexed.len());
        for argument in lexed.split(|argument| argument == &",") {
            let token = self.parse_lexed_line(argument)?;
            arguments.push(token);
        }

        if arguments.len() != 2 {
            return Err(Cow::from(
                "Memory reserve must take in 2 parameters; value and number of elements",
            ));
        }

        let size = Box::from(arguments.pop().unwrap());
        let value = Box::from(arguments.pop().unwrap());

        Ok(Some(Token::MemoryReserve { value, size }))
    }

    // Garunteed to return Token::Instruction if function succeeds and returns Some
    fn instruction_from_lexed(&mut self, lexed: &[&str]) -> AssemblerResult<Option<Token>> {
        let instruction = match Instruction::from_str(lexed[0]) {
            Ok(instruction) => instruction,
            Err(_) => return Ok(None),
        };

        let lexed = &lexed[1..];

        if lexed.is_empty() {
            return Ok(Some(Token::Instruction {
                instruction,
                operands: Vec::new(),
            }));
        }

        let mut operands: Vec<Token> = Vec::with_capacity(lexed.len());
        for operand in lexed.split(|operand| operand == &",") {
            let token = self.parse_lexed_line(operand)?;
            operands.push(token);
        }

        Ok(Some(Token::Instruction {
            instruction,
            operands,
        }))
    }
}
