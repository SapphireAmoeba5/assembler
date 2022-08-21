use crate::assembler::symbol_table::Symbol;
use crate::assembler::try_parse::trunucate_to_size;

use super::AssemblerResult;
use super::FirstPass;
use super::Size;
use super::{try_parse_identifier, try_parse_number};
use crate::assembler::expressions::{parse, RPNToken};

use crate::debug_println;
use std::borrow::Cow;
use std::cmp;
use std::collections::VecDeque;

impl FirstPass {
    pub fn try_eval(&self, expression: &[&str]) -> AssemblerResult<(u64, Size)> {
        let rpn = parse(expression)?;

        let mut stack: Vec<u64> = Vec::with_capacity(rpn.len());

        let mut maximum_size = Size::One;

        for operation in rpn.iter() {
            match operation {
                RPNToken::Identifier(identifier) => match self.symbol_table.get(identifier) {
                    Some(Symbol::SymbolConstant(constant)) => {
                        stack.push(constant.value);
                        maximum_size = cmp::max(maximum_size, constant.size);
                    }
                    Some(Symbol::SymbolLabel(label)) => return Err(Cow::from(format!("\"{}\" is a label. Labels can not be used here", identifier))),
                    None => return Err(Cow::from(format!("\"{}\" undeclared identifier. If \"{0}\" is a label, labels can only be used when indexing memory", identifier))),
                }

                RPNToken::Constant(value, size) => {
                    stack.push(*value);
                    maximum_size = cmp::max(maximum_size, *size);
                }

                RPNToken::Add => {
                    let right = stack.pop().unwrap();
                    let left = stack.pop().unwrap();

                    let result = left.wrapping_add(right);
                    stack.push(result);
                }

                RPNToken::Sub => {
                    let right = stack.pop().unwrap();
                    let left = stack.pop().unwrap();

                    let result = left.wrapping_sub(right);
                    stack.push(result);
                }

                RPNToken::Mul => {
                    let right = stack.pop().unwrap();
                    let left = stack.pop().unwrap();

                    let result = left.wrapping_mul(right);
                    stack.push(result);
                }

                RPNToken::Div => {
                    let right = stack.pop().unwrap();
                    let left = stack.pop().unwrap();

                    if right == 0 {
                        return Err(Cow::from("Division by zero error"));
                    }

                    let result = left.wrapping_div(right);
                    stack.push(result);
                }

                RPNToken::Negate => {
                    let value = stack.pop().unwrap();
                    let result = value.wrapping_neg();
                    stack.push(result);
                }
            }
        }

        debug_assert!(
            stack.len() == 1,
            "Stack length is not equal to {}, not 1",
            stack.len()
        );

        Ok((trunucate_to_size(stack[0], maximum_size), maximum_size))
    }
}
