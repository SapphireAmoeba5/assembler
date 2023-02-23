use super::{expressions::parse, try_parse::*};
use crate::{debug_println, error_println};

use super::{expressions::RPNToken, Assembler, AssemblerResult, Instruction, Register, Size};
use path_absolutize::Absolutize;
use std::{
    borrow::Cow,
    path::{Path, PathBuf},
    str::FromStr,
};

#[derive(Debug)]
pub enum Token {
    Instruction {
        instruction: Instruction,
        operands: Vec<Token>,
        line_number: usize,
    },

    Index {
        expression: Vec<RPNToken>,
        size: Size,
        line_number: usize,
    },

    ConstantInteger {
        expression: Vec<RPNToken>,
        line_number: usize,
    },

    ConstantString {
        value: Vec<u8>,
        line_number: usize,
    },

    MemoryReserve {
        value: Box<Token>,
        size: Box<Token>,
        line_number: usize,
    },

    Directive {
        directive: String,
        arguments: Vec<String>,
        line_number: usize,
    },

    Register {
        register: Register,
        line_number: usize,
    },

    ConstantDeclaration {
        name: String,
        expression: Vec<RPNToken>,
        size: Size,
        public: bool,
        line_number: usize,
    },

    LabelDeclaration {
        name: String,
        public: bool,
        line_number: usize,
    },

    FileInclude {
        file_path: PathBuf,
        public: bool,
        line_number: usize,
    },
}

#[derive(Debug)]
pub struct Tokenizer {
    pub file_path: PathBuf,
    pub tokens: Vec<Token>,
}

impl Tokenizer {
    pub fn tokenize(file_path: PathBuf) -> Result<Self, ()> {
        let source = match std::fs::read_to_string(&file_path) {
            Ok(f) => f,
            Err(_) => {
                println!("Error opening input file: \"{:?}\"", file_path);
                return Err(());
            }
        };
        Self::tokenize_source(
            if !file_path.is_absolute() {
                file_path.absolutize().unwrap().into()
            } else {
                file_path
            },
            source,
        )
    }
}

impl Tokenizer {
    fn tokenize_source(file_path: PathBuf, source: String) -> Result<Self, ()> {
        let mut error = false;
        let mut tokens: Vec<Token> = Vec::with_capacity(100);

        for (line_idx, line) in source.lines().enumerate() {
            let line_number = line_idx + 1;
            let lexed_line = Self::lex_line(line);

            if lexed_line.is_empty() {
                continue;
            }

            match Self::parse_lexed_line(&lexed_line, &file_path, line_number) {
                Ok(token) => tokens.push(token),
                Err(e) => {
                    error_println!("{e} {:?}:{}", file_path, line_number);
                    error = true
                }
            }

            println!("{:?}", tokens.last());
        }

        if !error {
            Ok(Self { file_path, tokens })
        } else {
            Err(())
        }
    }

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

    fn parse_lexed_line(
        lexed: &[&str],
        file_path: &Path,
        line_number: usize,
    ) -> AssemblerResult<Token> {
        if let Some((name, public)) = Self::try_parse_label(lexed)? {
            return Ok(Token::LabelDeclaration {
                name,
                public,
                line_number,
            });
        }

        if let Some((file_path, public)) = Self::try_parse_include(lexed, file_path)? {
            return Ok(Token::FileInclude {
                file_path,
                public,
                line_number,
            });
        }

        if let Some((name, size, expression, public)) = Self::try_parse_constant_declaration(lexed)?
        {
            return Ok(Token::ConstantDeclaration {
                name,
                expression,
                size,
                public,
                line_number,
            });
        }

        if let Some((directive, arguments)) = Self::try_parse_directive(lexed)? {
            return Ok(Token::Directive {
                directive,
                arguments,
                line_number,
            });
        }

        if let Some((instruction, operands)) =
            Self::try_parse_instruction(lexed, file_path, line_number)?
        {
            return Ok(Token::Instruction {
                instruction,
                operands,
                line_number,
            });
        }

        if let Some((value, size)) = Self::try_parse_memory_reserve(lexed, file_path, line_number)?
        {
            return Ok(Token::MemoryReserve {
                value: Box::from(value),
                size: Box::from(size),
                line_number,
            });
        }

        if let Some((expression, size)) = Self::try_parse_index(lexed)? {
            return Ok(Token::Index {
                expression,
                size,
                line_number,
            });
        }

        if lexed.len() == 1 {
            if let Some(string) = try_parse_string(lexed[0])? {
                return Ok(Token::ConstantString {
                    value: string.to_vec(),
                    line_number,
                });
            }

            if let Ok(register) = Register::from_str(lexed[0]) {
                return Ok(Token::Register {
                    register,
                    line_number,
                });
            }
        }

        let expression = parse(lexed)?;

        return Ok(Token::ConstantInteger {
            expression,
            line_number,
        });
    }

    fn try_parse_label<'a>(mut lexed: &'a [&'a str]) -> AssemblerResult<Option<(String, bool)>> {
        let public: bool;

        if lexed[0] == "public" {
            public = true;
            lexed = &lexed[1..];
        } else {
            public = false;
        }

        if lexed.len() == 1 && lexed[0].ends_with(':') {
            if let Ok(identifier) = try_parse_identifier(&lexed[0][..lexed[0].len() - 1]) {
                Ok(Some((String::from(identifier), public)))
            } else {
                Err(Cow::from(format!(
                    "\"{}\" is not a valid identifier",
                    &lexed[0][..lexed[0].len() - 1]
                )))
            }
        } else {
            Ok(None)
        }
    }

    fn try_parse_include(
        mut lexed: &[&str],
        path_to_base: &Path,
    ) -> AssemblerResult<Option<(PathBuf, bool)>> {
        let public: bool;
        if lexed[0] == "public" {
            public = true;
            lexed = &lexed[1..];
        } else {
            public = false;
        }

        if !lexed.is_empty() && lexed[0] == "include" {
            if lexed.len() != 2 {
                return Err(Cow::from("Invalid include statement"));
            }

            let mut path_to_include = lexed[1];

            // Allow the file path to be enclosed by quotes
            if path_to_include.starts_with('"') || path_to_include.ends_with('"') {
                if !path_to_include.starts_with('"') || !path_to_include.starts_with('"') {
                    return Err(Cow::from("Missing quotation marks"));
                }

                path_to_include = &path_to_include[1..path_to_include.len() - 1];
            }

            Ok(Some((
                path_to_base
                    .parent()
                    .unwrap()
                    .join(path_to_include)
                    .absolutize()
                    .unwrap()
                    .into(),
                public,
            )))
        } else {
            Ok(None)
        }
    }

    fn try_parse_constant_declaration(
        mut lexed: &[&str],
    ) -> AssemblerResult<Option<(String, Size, Vec<RPNToken>, bool)>> {
        let public: bool;

        if lexed[0] == "public" {
            public = true;
            lexed = &lexed[1..];
        } else {
            public = false;
        }

        if lexed.is_empty() || lexed[0] != "const" {
            return Ok(None);
        }

        if lexed.len() >= 5 {
            let size = match Size::from_str(lexed[1]) {
                Ok(size) => size,
                Err(_) => return Err(Cow::from("Invalid type for constant variable")),
            };

            let identifier = match try_parse_identifier(lexed[2]) {
                Ok(identifier) => identifier,
                Err(_) => return Err(Cow::from("Invalid identifier for constant")),
            };

            if lexed[3] != "=" {
                return Err(Cow::from(format!(
                    "Expected assignment operator but found \"{}\"",
                    lexed[3]
                )));
            }

            let rpn = match parse(&lexed[4..]) {
                Ok(rpn) => rpn,
                Err(e) => return Err(Cow::from(format!("Error parsing expression: {}", e))),
            };

            Ok(Some((identifier.into(), size, rpn, public)))
        } else {
            Err(Cow::from("Invalid constant declaration"))
        }
    }

    fn try_parse_directive(lexed: &[&str]) -> AssemblerResult<Option<(String, Vec<String>)>> {
        if lexed[0].starts_with('.') {
            let directive = lexed[0].to_string();
            let arguments: Vec<String> = lexed[1..].iter().map(|s| (*s).into()).collect();

            Ok(Some((directive, arguments)))
        } else {
            Ok(None)
        }
    }

    fn try_parse_instruction(
        lexed: &[&str],
        file_path: &Path,
        line_number: usize,
    ) -> AssemblerResult<Option<(Instruction, Vec<Token>)>> {
        let instruction = match Instruction::from_str(lexed[0]) {
            Ok(instr) => instr,
            Err(_) => return Ok(None),
        };

        let mut operands: Vec<Token> = Vec::with_capacity(lexed.len());

        for operand in lexed[1..].split(|operand| *operand == ",") {
            if operand.is_empty() {
                continue;
            }

            let token = Self::parse_lexed_line(operand, file_path, line_number)?;
            operands.push(token);
        }

        Ok(Some((instruction, operands)))
    }

    fn try_parse_memory_reserve(
        lexed: &[&str],
        file_path: &Path,
        line_number: usize,
    ) -> AssemblerResult<Option<(Token, Token)>> {
        if lexed[0] != "{" {
            return Ok(None);
        }

        if lexed.iter().rev().next().unwrap() != &"}" {
            return Err(Cow::from("No closing brace for memory reserve"));
        }

        let mut arguments: Vec<Token> = Vec::with_capacity(lexed.len());
        for argument in lexed[1..lexed.len() - 1].split(|argument| argument == &",") {
            let token = Self::parse_lexed_line(argument, file_path, line_number)?;
            arguments.push(token);
        }

        if arguments.len() != 2 {
            return Err(Cow::from(
                "Memory reserve must take in 2 parameters; value and number of elements",
            ));
        }

        let size = arguments.pop().unwrap();
        let value = arguments.pop().unwrap();

        Ok(Some((value, size)))
    }

    fn try_parse_index(mut lexed: &[&str]) -> AssemblerResult<Option<(Vec<RPNToken>, Size)>> {
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

        let expression = parse(&lexed[1..lexed.len() - 1])?;

        Ok(Some((expression, size)))
    }
}
