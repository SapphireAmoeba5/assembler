mod expressions;
mod first_pass;
mod instruction;
mod register;
mod size;
mod symbol_table;
mod token;
pub mod tokenizer;
mod try_parse;

use crate::debug_println;
use instruction::Instruction;
use register::Register;
use size::Size;
use std::borrow::Cow;
use std::cell::RefCell;
use std::str::FromStr;
use token::Token;

use symbol_table::{Constant, Label, Symbol, SymbolTable};

use self::{
    expressions::RPNToken,
    first_pass::{FirstPass, TokenEntry},
};

pub struct Assembler {
    input_file: String,
    output_file: String,

    assembled_data: RefCell<Vec<u8>>,
    first_pass: FirstPass,
    entry_point: RefCell<Option<String>>,
    current_line: usize,
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
            assembled_data: RefCell::new(Vec::with_capacity(100 * 1024)), /* 100 kilobytes of data. */
            first_pass: FirstPass::new(input_file),
            entry_point: RefCell::new(None),
            current_line: 0,
        };

        errors.extend(this.first_pass.begin());

        // Skip the assembling process if there are any errors in the first pass
        if !errors.is_empty() {
            return errors;
        }

        errors.extend(this.assemble_impl());

        errors
    }

    fn get_identifier(&self, identifier: &str) -> Option<&Symbol> {
        let value = self.first_pass.symbol_table.get(identifier);
        match value {
            Some(Symbol::SymbolConstant(constant)) if constant.line <= self.current_line => value,
            Some(Symbol::SymbolLabel(label)) => value,
            _ => None,
        }
    }
}

impl Assembler {
    fn assemble_impl(&mut self) -> AssemblerReturn {
        let mut errors: AssemblerReturn = Vec::with_capacity(10);

        for token in self.first_pass.tokens.iter() {
            self.current_line = token.line_number;
            if let Err(e) = self.assemble_token(token) {
                errors.push((e, Some((self.input_file.clone(), token.line_number))));
            }
        }

        let mut buffer: Vec<u8> = Vec::with_capacity(self.assembled_data.borrow().len() + 8);

        if let Some(entry_point) = &*self.entry_point.borrow() {
            let entry_point_address = match self.first_pass.symbol_table.get_label(&entry_point) {
                Some(label) => label.address,
                None => {
                    errors.push((Cow::from("Entry point is not a valid label"), None));
                    return errors;
                }
            };

            buffer.extend((entry_point_address + 8).to_le_bytes());
        } else {
            buffer.extend(8u64.to_le_bytes());
        }

        buffer.extend(&*self.assembled_data.borrow());

        if errors.is_empty() {
            println!(
                "Writing {} bytes to file \"{}\"",
                buffer.len(),
                self.output_file
            );

            if let Err(e) = std::fs::write(&self.output_file, buffer) {
                errors.push((Cow::from(format!("{:?}", e)), None));
            }
        }

        errors
    }

    fn assemble_token(&self, token_entry: &TokenEntry) -> AssemblerResult<()> {
        match &token_entry.token {
            Token::ConstantInteger { value, size } => self.assemble_constant_integer(*value, *size),
            Token::ConstantString { value } => self.assemble_constant_string(value),
            Token::MemoryReserve { value, size } => self.assemble_memory_reserve(value, size),
            Token::Directive {
                directive,
                arguments,
            } => self.parse_directive(directive, arguments)?,
            Token::Instruction {
                instruction,
                operands,
            } => self.assemble_instruction(
                *instruction,
                &operands,
                token_entry.offset,
                token_entry.width,
            )?,
            _ => panic!(
                "Found token that could not turned into bytes! {:#?}",
                token_entry
            ),
        }

        Ok(())
    }
}

impl Assembler {
    fn assemble_constant_integer(&self, value: u64, size: Size) {
        self.assembled_data
            .borrow_mut()
            .extend(&value.to_le_bytes()[0..size as usize]);
    }

    fn assemble_constant_string(&self, string: &[u8]) {
        self.assembled_data.borrow_mut().extend(string);
    }

    fn assemble_memory_reserve(&self, value: &Token, size: &Token) {
        let (value, width) = match value {
            Token::ConstantInteger { value, size } => (*value, *size),
            Token::Identifier { identifier } => {
                let constant = self
                    .first_pass
                    .symbol_table
                    .get_constant(&identifier)
                    .expect(&format!("\"{}\" is not a constant", identifier));
                (constant.value, constant.size)
            }
            _ => panic!("Memory reserve value is not a constant integer or identifier"), /* Memory reserve validity should be checked during the first pass, this code should not be executed under normal circumstances */
        };

        let number_elements = match size {
            Token::ConstantInteger { value, .. } => *value,
            Token::Identifier { identifier } => {
                self.first_pass
                    .symbol_table
                    .get_constant(identifier)
                    .expect("\"{}\", identifier is not a constant")
                    .value
            }
            _ => panic!("Memory reserve size is not a constant integer or identifier"), /* See above comment */
        } as usize;

        for _ in 0..number_elements {
            self.assembled_data
                .borrow_mut()
                .extend(&value.to_le_bytes()[..width as usize]);
            //buffer.extend(&value.to_le_bytes()[..width as usize]);
        }
    }

    fn assemble_instruction(
        &self,
        instruction: Instruction,
        operands: &[Token],
        instruction_width: u64,
        instruction_offset: u64,
    ) -> AssemblerResult<()> {
        debug_println!("Assembling instruction: {:?}", instruction);

        match instruction {
            // Two operand instructions (left operand always register, right operand is register or immediate value)
            Instruction::Mov
            | Instruction::Add
            | Instruction::Sub
            | Instruction::Mul
            | Instruction::Div
            | Instruction::Or
            | Instruction::Xor
            | Instruction::And
            | Instruction::Cmp => self.assemble_two_operand_instruction(instruction, operands)?,

            // One operand instructions (operand is register)
            Instruction::Not | Instruction::Neg | Instruction::Push | Instruction::Pop => {
                self.assemble_single_operand_instruction(instruction, operands)?
            }

            // Zero operand instructions
            Instruction::Hlt
            | Instruction::Nop
            | Instruction::Ret
            | Instruction::Pushf
            | Instruction::Popf
            | Instruction::Reti
            | Instruction::Cli
            | Instruction::Sti => self.assembled_data.borrow_mut().push(instruction as u8),

            Instruction::Str | Instruction::Ldr | Instruction::Lea => self
                .assemble_memory_instruction(
                    instruction,
                    operands,
                    instruction_width,
                    instruction_offset,
                )?,

            Instruction::Jmp
            | Instruction::Jz
            | Instruction::Jnz
            | Instruction::Jnz
            | Instruction::Jo
            | Instruction::Jno
            | Instruction::Js
            | Instruction::Jns
            | Instruction::Jc
            | Instruction::Jnc
            | Instruction::Jbe
            | Instruction::Ja
            | Instruction::Ja
            | Instruction::Jl
            | Instruction::Jge
            | Instruction::Jle
            | Instruction::Jg
            | Instruction::Call
            | Instruction::Lidt => self.assemble_branch_instruction(
                instruction,
                operands,
                instruction_offset,
                instruction_width,
            )?,

            Instruction::Int => self.assemble_interrupt_instruction(instruction, operands)?,

            Instruction::In | Instruction::Out => {
                self.assemble_port_instruction(instruction, operands)?
            }
        }

        Ok(())
    }

    /// Assemble any instruction that takes in two operands where the left operand is always a register, and the right operand can either be a register, or an immediate value.
    fn assemble_two_operand_instruction(
        &self,
        instruction: Instruction,
        operands: &[Token],
    ) -> AssemblerResult<()> {
        if operands.len() != 2 {
            return Err(Cow::from(format!(
                "Instruction expects 2 operands but found {}",
                operands.len()
            )));
        }

        let mut assembled_data = self.assembled_data.borrow_mut();
        assembled_data.push(instruction as u8);

        let left_operand = &operands[0];
        let right_operand = &operands[1];

        let destination_register = match left_operand {
            Token::Register { register } => register,
            _ => return Err(Cow::from("Invalid left operand")),
        };

        let destination_size = destination_register.get_size();
        let size_id = match destination_size {
            Size::One => 0,
            Size::Two => 1,
            Size::Four => 2,
            Size::Eight => 3,
        };

        let destination_id = destination_register.get_register_id();

        match right_operand {
            Token::ConstantInteger { value, .. } => {
                let metadata = (size_id << 6) | (destination_id << 3);
                assembled_data.push(metadata);
                assembled_data.extend(&value.to_le_bytes()[..destination_size as usize]);
            }
            Token::Register { register: source } => {
                if destination_size != source.get_size() {
                    return Err(Cow::from(
                        "Cannot have two different register operands sized",
                    ));
                }

                let metadata = (size_id << 6) | (destination_id << 3) | source.get_register_id();
                assembled_data.push(metadata);
            }
            Token::Identifier { identifier } => {
                let constant = match self.first_pass.symbol_table.get(&identifier) {
                    Some(Symbol::SymbolConstant(constant)) => constant,
                    Some(Symbol::SymbolLabel(label)) => {
                        return Err(Cow::from(format!(
                            "\"{}\" is a label. Cannot move value of label into register",
                            identifier
                        )))
                    }
                    None => return Err(Cow::from(format!("\"{}\" does not exist", identifier))),
                };

                if destination_size != constant.size {
                    return Err(Cow::from("Cannot move mismatched sized values"));
                }

                let metadata = (size_id << 6) | (destination_id << 3) | 0;
                assembled_data.push(metadata);
                assembled_data.extend(constant.value.to_le_bytes());
            }
            _ => panic!(),
        }

        Ok(())
    }

    /// Assembles an instruction that takes only one operand, and that being a register
    fn assemble_single_operand_instruction(
        &self,
        instruction: Instruction,
        operands: &[Token],
    ) -> AssemblerResult<()> {
        if operands.len() != 1 {
            return Err(Cow::from(format!(
                "Instruction expects 1 operand, but found {}",
                operands.len()
            )));
        }

        let mut assembled_data = self.assembled_data.borrow_mut();
        assembled_data.push(instruction as u8);

        let register = match &operands[0] {
            Token::Register { register } => register,
            _ => {
                return Err(Cow::from(
                    "Instruction expects a single register as an operand",
                ))
            }
        };

        let size_id = match register.get_size() {
            Size::One => 0,
            Size::Two => 1,
            Size::Four => 2,
            Size::Eight => 3,
        };

        let metadata = (size_id << 6) | register.get_register_id();
        assembled_data.push(metadata);

        Ok(())
    }

    /// Assembles Str, Ldr, and Lea instructions.
    fn assemble_memory_instruction(
        &self,
        instruction: Instruction,
        operands: &[Token],
        instruction_offset: u64,
        instruction_width: u64,
    ) -> AssemblerResult<()> {
        if operands.len() != 2 {
            return Err(Cow::from(format!(
                "Instruction expects 2 operands, but found {}",
                operands.len()
            )));
        }

        self.assembled_data.borrow_mut().push(instruction as u8);

        let left_register = match &operands[0] {
            Token::Register { register } => register,
            _ => return Err(Cow::from("Left operand must be a register")),
        };

        let (expression, size) = match &operands[1] {
            Token::Index { expression, size } => (expression, *size),
            _ => return Err(Cow::from("Memory index expected for second operand")),
        };

        if left_register.get_size() != size {
            return Err(Cow::from("Mismatched operand sizes"));
        }

        let size_id = match left_register.get_size() {
            Size::One => 0,
            Size::Two => 1,
            Size::Four => 2,
            Size::Eight => 3,
        };

        // The metadata containing the size of the memory access & register, and the ID of the register that the data will be stored/sourced from
        let metadata = size_id << 6 | left_register.get_register_id();
        self.assembled_data.borrow_mut().push(metadata);

        self.assemble_index(expression, size, instruction_offset, instruction_width)?;

        Ok(())
    }

    fn assemble_branch_instruction(
        &self,
        instruction: Instruction,
        operands: &[Token],
        instruction_offset: u64,
        instruction_width: u64,
    ) -> AssemblerResult<()> {
        if operands.len() != 1 {
            return Err(Cow::from(format!(
                "Instruction expects 1 operand but found {}",
                operands.len()
            )));
        }

        self.assembled_data.borrow_mut().push(instruction as u8);

        let (expression, size) = match &operands[0] {
            Token::Index { expression, size } => (expression, *size),
            _ => return Err(Cow::from("Memory index expected for second operand")),
        };

        self.assemble_index(expression, size, instruction_offset, instruction_width)?;

        Ok(())
    }

    fn assemble_interrupt_instruction(
        &self,
        instruction: Instruction,
        operands: &[Token],
    ) -> AssemblerResult<()> {
        if operands.len() != 1 {
            return Err(Cow::from(format!(
                "Instruction expects 1 operand but found {}",
                operands.len()
            )));
        }

        self.assembled_data.borrow_mut().push(instruction as u8);

        let constant = match &operands[0] {
            Token::ConstantInteger { value, .. } => *value as u8,
            Token::Identifier { identifier } => {
                match self.first_pass.symbol_table.get(identifier) {
                    Some(Symbol::SymbolConstant(constant)) => constant.value as u8,
                    Some(Symbol::SymbolLabel(_)) => {
                        return Err(Cow::from(format!(
                            "Identifier \"{}\" is a label. Cannot use labels in this position",
                            identifier
                        )))
                    }
                    None => {
                        return Err(Cow::from(format!(
                            "Identifier \"{}\" does not exists",
                            identifier
                        )))
                    }
                }
            }
            _ => {
                return Err(Cow::from(
                    "Expected an integer literal, or constant variable",
                ))
            }
        };

        self.assembled_data.borrow_mut().push(constant);

        Ok(())
    }

    fn assemble_port_instruction(
        &self,
        instruction: Instruction,
        operands: &[Token],
    ) -> AssemblerResult<()> {
        self.assembled_data.borrow_mut().push(instruction as u8);

        if (operands.len() == 2) {
            let register = match operands[0] {
                Token::Register { register } => register,
                _ => {
                    return Err(Cow::from(
                        "Instruction expects left operand to be a register",
                    ))
                }
            };

            let port = match operands[1] {
                Token::ConstantInteger { value, size } => value as u16,
                _ => {
                    return Err(Cow::from(
                        "Instruction expects right operand to be a constant integer",
                    ))
                }
            };

            let reg_id = register.get_register_id();

            self.assembled_data.borrow_mut().push(reg_id);
            self.assembled_data
                .borrow_mut()
                .extend_from_slice(&port.to_le_bytes());

            Ok(())
        } else {
            Err(Cow::from("Instruction requires two operands"))
        }
    }

    fn assemble_index(
        &self,
        expression: &Vec<RPNToken>,
        _size: Size,
        instruction_offset: u64,
        instruction_width: u64,
    ) -> AssemblerResult<()> {
        let mut stack: Vec<(u64, bool)> = Vec::with_capacity(expression.len());

        let mut base_register: Option<Register> = None;
        let mut index_register: Option<Register> = None;

        for token in expression {
            match token {
                RPNToken::Constant(value, _) => stack.push((*value, false)),

                RPNToken::Identifier(identifier) => {
                    match Register::from_str(identifier) {
                        Ok(register) => {
                            if (base_register.is_none()) {
                                base_register = Some(register);
                            } else if (index_register.is_none()) {
                                index_register = Some(register);
                            } else {
                                return Err(Cow::from(
                                    "Cannot use more than two registers in an expression",
                                ));
                            }

                            stack.push((0, true));
                            continue;
                        }
                        Err(_) => {}
                    }

                    match self.get_identifier(identifier) {
                        Some(symbol) => match symbol {
                            Symbol::SymbolConstant(constant) => stack.push((constant.value, false)),
                            Symbol::SymbolLabel(label) => {
                                let byte_offset = label
                                    .address
                                    .wrapping_sub(instruction_offset + instruction_width);

                                if let Some(base_reg) = base_register {
                                    if base_reg != Register::IP(Size::Eight) {
                                        if let Some(index_reg) = index_register {
                                            if index_reg != Register::IP(Size::Eight) {
                                                return Err(Cow::from("Cannot use label in memory index at the same time as two other registers"));
                                            }
                                        } else {
                                            index_register = Some(Register::IP(Size::Eight));
                                        }
                                    }
                                } else {
                                    base_register = Some(Register::IP(Size::Eight));
                                }

                                stack.push((byte_offset, false))
                            }
                        },
                        None => {
                            return Err(Cow::from(format!(
                                "Undeclared identifier used \"{}\"",
                                identifier
                            )))
                        }
                    }
                }

                RPNToken::Add => {
                    let (right_value, right_depends_on_register) = stack.pop().unwrap();
                    let (left_value, left_depends_on_register) = stack.pop().unwrap();

                    let result = left_value.wrapping_add(right_value);

                    stack.push((
                        result,
                        right_depends_on_register || left_depends_on_register,
                    ));
                }

                RPNToken::Sub => {
                    let (right_value, right_depends_on_register) = stack.pop().unwrap();
                    let (left_value, left_depends_on_register) = stack.pop().unwrap();

                    if right_depends_on_register == false {
                        let result = left_value.wrapping_sub(right_value);
                        stack.push((result, left_depends_on_register));
                    } else {
                        return Err(Cow::from(
                            "Cannot subtract a value that depends on the value of a register",
                        ));
                    }
                }

                RPNToken::Mul => {
                    let (right_value, right_depends_on_register) = stack.pop().unwrap();
                    let (left_value, left_depends_on_register) = stack.pop().unwrap();

                    if !right_depends_on_register && !left_depends_on_register {
                        let result = left_value.wrapping_mul(right_value);
                        stack.push((result, false));
                    } else {
                        return Err(Cow::from(
                            "Cannot multiply values that depend on the value of registers",
                        ));
                    }
                }

                RPNToken::Div => {
                    let (right_value, right_depends_on_register) = stack.pop().unwrap();
                    let (left_value, left_depends_on_register) = stack.pop().unwrap();

                    if (!right_depends_on_register && !left_depends_on_register) {
                        if right_value == 0 {
                            return Err(Cow::from("Cannot divide by zero"));
                        }
                        let result = left_value.wrapping_div(right_value);
                        stack.push((result, false));
                    } else {
                        return Err(Cow::from(
                            "Cannot divide values that depend on the value of registers",
                        ));
                    }
                }

                RPNToken::Negate => {
                    let (value, does_value_depend_on_register) = stack.pop().unwrap();

                    if (!does_value_depend_on_register) {
                        let result = value.wrapping_neg();
                        stack.push((result, false));
                    } else {
                        return Err(Cow::from("Cannot use negative values when they depend on the value of a register"));
                    }
                }
            }
        }

        if (stack.len() == 1) {
            let (const_offset, _) = stack.pop().unwrap();

            let metadata = index_register.map(|s| s.get_register_id()).unwrap_or(0) << 3
                | base_register.map(|s| s.get_register_id()).unwrap_or(0);

            self.assembled_data.borrow_mut().push(metadata);
            self.assembled_data
                .borrow_mut()
                .extend(const_offset.to_le_bytes());

            Ok(())
        } else {
            Err(Cow::from("Invalid expression!"))
        }
    }
}

impl Assembler {
    fn parse_directive(&self, directive: &str, arguments: &[String]) -> AssemblerResult<()> {
        match directive {
            "entry" => self.parse_entry_directive(arguments),
            _ => Err(Cow::from(format!(
                "\"{}\" is an unknown directive",
                directive
            ))),
        }
    }

    fn parse_entry_directive(&self, arguments: &[String]) -> AssemblerResult<()> {
        if arguments.len() != 1 {
            return Err(Cow::from(format!(
                "Directive expects only one argument but found {}",
                arguments.len()
            )));
        }

        match self.first_pass.symbol_table.get(&arguments[0]) {
            Some(Symbol::SymbolLabel(label)) => {
                if self.entry_point.borrow().is_some() {
                    return Err(Cow::from("Redefined entry point"));
                }

                let _ = self.entry_point.borrow_mut().insert(label.name.clone());
                Ok(())
            }
            Some(Symbol::SymbolConstant(constant)) => Err(Cow::from(format!(
                "Identifier \"{}\" is a constant. Only labels can be defined as entry points",
                arguments[0]
            ))),
            None => Err(Cow::from(format!(
                "No label with the name \"{}\" exists",
                arguments[0]
            ))),
        }
    }
}
