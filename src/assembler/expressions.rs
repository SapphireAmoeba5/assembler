use super::size::Size;
use super::try_parse::{try_parse_identifier, try_parse_number};
use super::AssemblerResult;
use crate::debug_println;
use std::collections::VecDeque;

use std::borrow::Cow;

#[derive(Debug, Clone)]
pub enum RPNToken {
    Constant(u64, Size),
    Identifier(String),

    Add,
    Sub,
    Mul,
    Div,
    Negate,
}

#[derive(Debug, Clone)]
enum ExpressionToken {
    Constant(u64, Size),
    Identifier(String),

    Add,
    Sub,
    Mul,
    Div,
    Negate,

    OpeningBracket,
    ClosingBracket,
}

/// Parses an already lexed expression into a series of tokens easily used by an RPN solver.
/// This function expects the expression to be lexed such that all integer literals, identifiers, and '+', '-', '*', '/', '(', and ')' symbols are seperated.
pub fn parse(expression: &[&str]) -> AssemblerResult<Vec<RPNToken>> {
    let tokenized = tokenize_expression(expression)?;

    if let Err(e) = verify_expression(&tokenized) {
        return Err(e);
    }

    infix_to_rpn(tokenized)
}

impl ExpressionToken {
    fn is_operator(&self) -> bool {
        match self {
            Self::Add | Self::Sub | Self::Mul | Self::Div | Self::Negate => true,
            _ => false,
        }
    }

    fn is_math_operator(&self) -> bool {
        match self {
            Self::Add | Self::Sub | Self::Mul | Self::Div => true,
            _ => false,
        }
    }

    fn is_sub(&self) -> bool {
        match self {
            Self::Sub => true,
            _ => false,
        }
    }

    fn is_value(&self) -> bool {
        match self {
            Self::Constant(..) | Self::Identifier(..) => true,
            _ => false,
        }
    }

    fn is_negate(&self) -> bool {
        match self {
            Self::Negate => true,
            _ => false,
        }
    }

    fn is_opening_bracket(&self) -> bool {
        match self {
            Self::OpeningBracket => true,
            _ => false,
        }
    }

    fn is_closing_bracket(&self) -> bool {
        match self {
            Self::ClosingBracket => true,
            _ => false,
        }
    }

    fn is_bracket(&self) -> bool {
        self.is_opening_bracket() || self.is_closing_bracket()
    }

    /// Converts self into an RPNToken. Panics if it is impossible to convert
    fn to_rpn_token(self) -> RPNToken {
        match self {
            ExpressionToken::Constant(value, width) => RPNToken::Constant(value, width),
            ExpressionToken::Identifier(identifier) => RPNToken::Identifier(identifier),
            ExpressionToken::Add => RPNToken::Add,
            ExpressionToken::Sub => RPNToken::Sub,
            ExpressionToken::Mul => RPNToken::Mul,
            ExpressionToken::Div => RPNToken::Div,
            ExpressionToken::Negate => RPNToken::Negate,

            ExpressionToken::OpeningBracket | ExpressionToken::ClosingBracket => {
                panic!("Cannot convert {:?}", self)
            }
        }
    }
}

/// Tokenize an expression.
fn tokenize_expression(expression: &[&str]) -> AssemblerResult<Vec<ExpressionToken>> {
    debug_println!("Tokenizing expression: \"{:?}\"", expression);

    let mut output: Vec<ExpressionToken> = Vec::with_capacity(expression.len());

    let mut was_previous_operator = true;

    for word in expression {
        let token = word_to_expression_token(word)?;

        if was_previous_operator && token.is_sub() {
            // Replace a subtraction operator with a negate one. This is to allow equations like "5 + -3" to work properly
            output.push(ExpressionToken::Negate);
        } else {
            output.push(token);
        }

        was_previous_operator =
            output.last().unwrap().is_bracket() || output.last().unwrap().is_operator();
    }
    Ok(output)
}

// Convert an infix expression into an RPN/postfix expression
fn infix_to_rpn(expression: Vec<ExpressionToken>) -> AssemblerResult<Vec<RPNToken>> {
    let mut output_queue: Vec<RPNToken> = Vec::with_capacity(expression.len());
    let mut operator_stack: Vec<ExpressionToken> = Vec::with_capacity(expression.len());

    for token in expression {
        match token {
            ExpressionToken::Constant(_, _) | ExpressionToken::Identifier(_) => {
                let rpn_token = token.to_rpn_token();
                output_queue.push(rpn_token);
            }
            ExpressionToken::Add
            | ExpressionToken::Sub
            | ExpressionToken::Mul
            | ExpressionToken::Div
            | ExpressionToken::Negate => {
                if let Some(popped) = operator_stack.last() {
                    // So far, whether or not an operator is left or right associative doesn't matter because the only right associative operator (negate) has a higher precedence than anything else
                    if popped.is_operator()
                        && operator_precedence(popped) >= operator_precedence(&token)
                    {
                        let operator = operator_stack.pop().unwrap().to_rpn_token();
                        output_queue.push(operator);
                    }
                }
                operator_stack.push(token);
            }
            ExpressionToken::OpeningBracket => operator_stack.push(ExpressionToken::OpeningBracket),
            ExpressionToken::ClosingBracket => {
                let mut found_opening_bracket = false;
                while let Some(token) = operator_stack.pop() {
                    match token {
                        ExpressionToken::OpeningBracket => {
                            found_opening_bracket = true;
                            break;
                        }
                        _ => {}
                    }
                    output_queue.push(token.to_rpn_token());
                }

                if !found_opening_bracket {
                    return Err(Cow::from("Mismatched brackets"));
                }
            }
        }
    }

    // We must pop all remaining elements off the operator stack and push them to the back of the output queue
    for token in operator_stack.into_iter().rev() {
        output_queue.push(token.to_rpn_token());
    }

    Ok(output_queue)
}

fn word_to_expression_token(word: &str) -> AssemblerResult<ExpressionToken> {
    match word {
        "+" => return Ok(ExpressionToken::Add),
        "-" => return Ok(ExpressionToken::Sub),
        "*" => return Ok(ExpressionToken::Mul),
        "/" => return Ok(ExpressionToken::Div),
        "(" => return Ok(ExpressionToken::OpeningBracket),
        ")" => return Ok(ExpressionToken::ClosingBracket),
        _ => {}
    }

    match try_parse_number(word) {
        Ok(Some((value, size))) => return Ok(ExpressionToken::Constant(value, size)),
        Err(_) => return Err(Cow::from(format!("Invalid integer literal \"{}\"", word))),
        Ok(None) => {}
    }

    match try_parse_identifier(word) {
        Ok(identifier) => Ok(ExpressionToken::Identifier(identifier.into())),
        Err(_) => Err(Cow::from(format!("\"{}\" is an invalid token", word))),
    }
}

fn operator_precedence(operator: &ExpressionToken) -> usize {
    match operator {
        ExpressionToken::Add | ExpressionToken::Sub => 0,
        ExpressionToken::Mul | ExpressionToken::Div => 1,
        ExpressionToken::Negate => 2,
        _ => panic!(
            "Internal error: Expected an operator but found \"{:?}\"",
            operator
        ),
    }
}

/// Returns Err if the given expression is not a valid infix expression
fn verify_expression(expression: &Vec<ExpressionToken>) -> AssemblerResult<()> {
    let mut previous_token: Option<&ExpressionToken> = None;

    for token in expression.iter() {
        // Negates are placed during tokenization so they are always valid.
        if token.is_negate() {
            continue;
        }

        if let Some(previous_token) = previous_token {
            if (previous_token.is_math_operator() || previous_token.is_opening_bracket())
                && !token.is_value()
                && !token.is_opening_bracket()
            {
                return Err(Cow::from("Invalid expression"));
            } else if (previous_token.is_value() || previous_token.is_closing_bracket())
                && !token.is_operator()
                && !token.is_closing_bracket()
            {
                return Err(Cow::from("Invalid expression"));
            }
        } else {
            match token {
                ExpressionToken::Negate
                | ExpressionToken::Constant(..)
                | ExpressionToken::Identifier(..)
                | ExpressionToken::OpeningBracket => {}
                _ => return Err(Cow::from("Invalid expression")),
            }
        }

        previous_token = Some(token);
    }

    if let Some(previous_token) = previous_token {
        if !previous_token.is_closing_bracket() && !previous_token.is_value() {
            return Err(Cow::from("Invalid expression"));
        }
    } else {
        // This branch shouldn't be executed. It is just here in case though.
        return Err(Cow::from("Invalid expression"));
    }

    Ok(())
}
