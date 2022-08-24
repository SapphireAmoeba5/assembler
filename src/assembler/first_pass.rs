mod equation_parse;

use super::expressions::parse;
use super::instruction::Instruction;
use super::register::Register;
use super::size::Size;
use super::symbol_table::Constant;
use super::try_parse::{try_parse_identifier, try_parse_number, try_parse_string};
use super::{
    symbol_table::{Symbol, SymbolTable},
    token::Token,
    AssemblerResult, AssemblerReturn,
};
use crate::assembler::symbol_table::Label;
use crate::debug_println;
use std::borrow::Cow;
use std::str::FromStr;

#[derive(Debug)]
pub struct TokenEntry {
    pub token: Token,
    pub line_number: usize,
    pub offset: u64,
    pub width: u64,
}

impl TokenEntry {
    pub fn new(token: Token, line_number: usize, offset: u64, width: u64) -> Self {
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
    offset: u64,
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
        debug_println!("Parsing:\n{}", source);

        let mut errors: AssemblerReturn = Vec::with_capacity(10);

        for (line_index, line) in source.lines().enumerate() {
            let line_number = line_index + 1;
            let lexed_line = Self::lex_line(line);

            // We need to parse out all labels from the beginning of the line before we continue
            let lexed_line = match self.parse_label_from_lexed(&lexed_line, line_number) {
                Ok(new_lexed_line) => new_lexed_line,
                Err(e) => {
                    let error = (e, Some((self.input_file.clone(), line_number)));
                    errors.push(error);
                    continue;
                }
            };

            if lexed_line.is_empty() {
                continue;
            }

            match self.try_parse_constant_declaration(lexed_line, line_number) {
                Ok(Some(_)) => continue,
                Ok(None) => {}
                Err(e) => {
                    let error = (e, Some((self.input_file.clone(), line_number)));
                    errors.push(error);
                    continue;
                }
            }

            debug_println!("{:?} -- [{}:{}]", lexed_line, self.input_file, line_number);

            let token = match self.parse_lexed_line(&lexed_line) {
                Ok(tokens) => tokens,
                Err(e) => {
                    let error = (e, Some((self.input_file.clone(), line_number)));
                    errors.push(error);
                    continue;
                }
            };
            debug_println!("{:?}\n", token);

            let width = match self.get_width_of_token(&token) {
                Ok(width) => width,
                Err(e) => {
                    let error = (e, Some((self.input_file.clone(), line_number)));
                    errors.push(error);
                    continue;
                }
            };
            let token_entry = TokenEntry::new(token, line_number, self.offset, width);
            self.tokens.push(token_entry);
            self.offset += width;
        }

        errors
    }
}

/// Methods for determining the width of tokens
impl FirstPass {
    fn get_width_of_token(&self, token: &Token) -> AssemblerResult<u64> {
        match token {
            Token::ConstantInteger { value: _, size } => Ok(*size as u64),
            Token::ConstantString { value } => Ok(value.len().try_into().unwrap()),
            Token::MemoryReserve { value, size } => self.get_width_of_memory_reserve(value, size),
            Token::Instruction {
                instruction,
                operands,
            } => self.get_width_of_instruction(*instruction, operands),

            /* Tokens that never take up space */
            Token::Directive { .. } => Ok(0),

            /* Tokens that should not be standalone (because they should be included in other tokens) */
            Token::Index { .. } | Token::Register { .. } => return Err(Cow::from("Invalid token")),

            // Having constant identifiers as tokens are planned to be added.
            // But right now based on how the current parsing system works
            // it can cause an inconsistency where identifiers can be used
            // which there already is when it comes to memory reserves.
            Token::Identifier { identifier } => {
                return Err(Cow::from(
                    "TODO! Implement constant identifiers as standalone tokens",
                ))
            }
        }
    }

    fn get_width_of_memory_reserve(&self, value: &Token, elements: &Token) -> AssemblerResult<u64> {
        let value_width = match value {
            Token::ConstantInteger{value: _, size} => *size,
            Token::Identifier{identifier} => match self.symbol_table.get(identifier) {
                Some(Symbol::SymbolConstant(constant)) => constant.size,
                Some(Symbol::SymbolLabel(label)) => return Err(Cow::from("Cannot use label for memory reserve value")),
                None => return Err(Cow::from(format!("Constant values must be declared before use in memory reserve"))),
            },
            _ => return Err(Cow::from("Invalid type for memory reserve value. Expected integer literal or constant variable")),
        };

        let number_of_elements = match elements {
            Token::ConstantInteger{value, size: _} => *value,
            Token::Identifier{identifier} => match self.symbol_table.get(identifier) {
                Some(Symbol::SymbolConstant(constant)) => constant.value,
                Some(Symbol::SymbolLabel(label)) => return Err(Cow::from("Cannot use label for number of elements in memory reserve")),
                None => return Err(Cow::from(format!("Constant \"{}\" was not declared before use as number of elements for memory reserve", identifier))),
            }
            _ => return Err(Cow::from("Invalid type for number of elements in memory reserve. Expected integer literal or a constant variable")),
        };

        Ok((value_width as u64) * number_of_elements)
    }

    fn get_width_of_instruction(
        &self,
        instruction: Instruction,
        operands: &[Token],
    ) -> AssemblerResult<u64> {
        match instruction {
            /* Instructions that are encoded in 2 bytes, and possibly +1, +2, +4, or +8 bytes depending on the 2nd operand & width of register */
            Instruction::Mov
            | Instruction::Add
            | Instruction::Sub
            | Instruction::Mul
            | Instruction::Div
            | Instruction::Or
            | Instruction::Xor
            | Instruction::And
            | Instruction::Cmp => {
                if operands.len() != 2 {
                    return Err(Cow::from(format!(
                        "Expected 2 operands, but found {}",
                        operands.len()
                    )));
                }

                let register_width = match operands[0] {
                    Token::Register { register } => register.get_size(),
                    _ => return Err(Cow::from("The first operand must be a register")),
                };

                match operands[1] {
                    Token::Register{..} => Ok(2), /* 2 bytes to encode this instruction -- one for the opcode, and the other for the destination, source, and register width */

                    Token::ConstantInteger{..} | Token::Identifier{..} => Ok(2 + register_width as u64), /* Encoding is dependent on the size of the register. Any syntax errors I.E undeclared variable will be checked in the assembly stage */
                    _ => return Err(Cow::from("2nd operand of instruction must be a register, integer literal, or variable")),
                }
            }

            /* Instructions that are garunteed to be encoded in 2 bytes */
            Instruction::Not
            | Instruction::Neg
            | Instruction::Push
            | Instruction::Pop
            | Instruction::Int => Ok(2), /* These instructions are always encoded in 2 bytes */

            /* Instructions that are garunteed to be encoded in 1 byte */
            Instruction::Hlt
            | Instruction::Nop
            | Instruction::Ret
            | Instruction::Pushf
            | Instruction::Popf
            | Instruction::Reti => Ok(1), /* Garunteed to be encoded in 1 byte */

            /* Instructions that are garunteed to be encoded in 11 bytes */
            Instruction::Str | Instruction::Ldr | Instruction::Lea => Ok(11),

            /* Instructions that are garunteed to be encoded in 10 bytes */
            Instruction::Jmp
            | Instruction::Jz
            | Instruction::Jnz
            | Instruction::Jo
            | Instruction::Jno
            | Instruction::Js
            | Instruction::Jns
            | Instruction::Jc
            | Instruction::Jnc
            | Instruction::Jbe
            | Instruction::Ja
            | Instruction::Jl
            | Instruction::Jge
            | Instruction::Jle
            | Instruction::Jg
            | Instruction::Call
            | Instruction::Lidt => Ok(10),
        }
    }
}

impl FirstPass {
    /// Token seperators are included in the output of lex_line
    fn is_token_seperator(ch: char) -> bool {
        ch == ','
            || ch == '['
            || ch == ']'
            || ch == '{'
            || ch == '}'
            || ch == '('
            || ch == ')'
            || ch == '+'
            || ch == '-'
            || ch == '*'
            || ch == '/'
            || ch == '-'
            || ch == '/'
            || ch == '='
    }

    fn lex_line<'a>(line: &'a str) -> Vec<&'a str> {
        let mut result: Vec<&'a str> = Vec::with_capacity(line.len());

        let mut in_string = false;
        let mut escaped = false;
        let mut has_comment = false;

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
                } else if Self::is_token_seperator(ch) {
                    let slice = &line[start..i];
                    if !slice.trim().is_empty() {
                        result.push(slice);
                    }
                    result.push(&line[i..i + 1]);
                    start = i + 1;
                } else if ch == ';' {
                    let slice = &line[start..i];
                    if !slice.trim().is_empty() {
                        result.push(slice);
                    }

                    has_comment = true;
                    break;
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
        if !has_comment && !slice.is_empty() {
            result.push(&line[start..]);
        }

        result
    }

    /// Each line should corrospond to a single token. line is garunteed to at least be a size of 1
    fn parse_lexed_line(&mut self, line: &[&str]) -> AssemblerResult<Token> {
        if line[0].starts_with('.') {
            let directive: String = line[0][1..].to_string();
            let arguments: Vec<String> = line[1..].iter().map(|s| (*s).into()).collect();
            return Ok(Token::Directive {
                directive,
                arguments,
            });
        }

        let result = self.instruction_from_lexed(line)?;
        if let Some(instruction) = result {
            return Ok(instruction);
        }

        let result = self.memory_reserve_from_lexed(line)?;
        if let Some(reserve) = result {
            return Ok(reserve);
        }

        let result = self.index_from_lexed(line)?;
        if let Some(index) = result {
            return Ok(index);
        }

        if line.len() == 1 {
            let result = try_parse_string(line[0])?;
            if let Some(value) = result {
                return Ok(Token::ConstantString {
                    value: value.into_owned(),
                });
            }

            match Register::from_str(line[0]) {
                Ok(register) => return Ok(Token::Register { register }),
                Err(_) => {}
            }
        }

        let (value, size) = self.try_eval(line)?;
        Ok(Token::ConstantInteger { value, size })

        // Flatten the line into a single string to output as an error
        //let flat_line = line
        //    .iter()
        //    .fold(String::with_capacity(line.len() * 5), |acc, s| {
        //        if acc.is_empty() {
        //            format!("{}", s)
        //        } else {
        //            format!("{} {}", acc, s)
        //        }
        //    });
        //Err(Cow::from(format!("Error at \"{}\"", flat_line)))
    }

    /// Garunteed to return Token::MemoryReserve if function succeeds and returns Some
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

    /// Garunteed to return Token::Instruction if function succeeds and returns Some
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

    /// Garunteed to return Token::Index if functions succeeds and returns Some
    fn index_from_lexed(&mut self, mut lexed: &[&str]) -> AssemblerResult<Option<Token>> {
        let size = if let Ok(size) = Size::from_str(lexed[0]) {
            lexed = &lexed[1..];
            size
        } else {
            Size::Eight
        };
        if lexed[0] != "[" {
            return Ok(None);
        }
        if *lexed.iter().rev().next().unwrap() != "]" {
            return Err(Cow::from("No closing bracket for memory index"));
        }

        lexed = &lexed[1..lexed.len() - 1];

        let expression = parse(lexed)?;

        Ok(Some(Token::Index {
            expression,
            size,
        }))
    }

    fn parse_label_from_lexed<'a>(
        &mut self,
        mut lexed: &'a [&'a str],
        line_number: usize,
    ) -> AssemblerResult<&'a [&'a str]> {
        while let Some(label_name) = lexed.get(0).filter(|s| s.ends_with(':')) {
            debug_println!("Parsing label \"{}\" at offset {}", label_name, self.offset);

            let label_name = &label_name[..label_name.len() - 1];
            lexed = &lexed[1..];

            match try_parse_identifier(label_name) {
                Ok(identifier) => {
                    if self.symbol_table.add_label(Label::new(
                        identifier.into(),
                        self.offset,
                        line_number,
                    )) == false
                    {
                        // Safe to unwrap because 'identifier' is garunteed to exist in the symbol table if this code is executed
                        let original_line = self.symbol_table.get(identifier).unwrap().get_line();
                        return Err(Cow::from(format!(
                            "Variable \"{}\" already declared on line {}",
                            identifier, original_line
                        )));
                    }
                }
                Err(_) => {
                    return Err(Cow::from(format!(
                        "\"{}\" is not a valid identifier for a label",
                        label_name
                    )))
                }
            }
        }

        Ok(lexed)
    }

    // Parses a constant declaration, and stores it in the symbol table. Returns Ok(Some) if it found a declaraion, Ok(None) if there wasn't a declaration, and Err if there was an error
    fn try_parse_constant_declaration(
        &mut self,
        lexed: &[&str],
        line_number: usize,
    ) -> AssemblerResult<Option<()>> {
        if lexed[0] != "const" {
            return Ok(None);
        }

        if lexed.len() < 5 {
            return Err(Cow::from("Invalid constant declaration"));
        }

        let size = match Size::from_str(lexed[1]) {
            Ok(size) => size,
            Err(_) => {
                return Err(Cow::from(format!(
                    "Expected type but found \"{}\" on constant declaration",
                    lexed[1]
                )))
            }
        };

        let identifier = lexed[2];

        if lexed[3] != "=" {
            return Err(Cow::from(format!(
                "Expected assignment operator but found \"{}\"",
                lexed[3]
            )));
        }

        let value = match self.try_eval(&lexed[4..]) {
            Ok((value, _)) => value,
            Err(e) => return Err(e),
        };

        if try_parse_identifier(identifier).is_err() {
            return Err(Cow::from(format!(
                "\"{}\" is an invalid identifier for constant declaration",
                identifier
            )));
        }

        if self.symbol_table.add_constant(Constant::new(
            identifier.into(),
            value,
            line_number,
            size,
        )) == false
        {
            let original_line = self.symbol_table.get(identifier).unwrap().get_line();
            return Err(Cow::from(format!(
                "Variable \"{}\" already declared on line {}",
                identifier, original_line
            )));
        }

        Ok(Some(()))
    }
}
