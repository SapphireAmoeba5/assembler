use super::register::Register;
use super::Instruction;
use super::Size;

#[derive(Debug)]
pub enum Token {
    Instruction {
        instruction: Instruction,
        operands: Vec<Token>,
    },

    Index {
        base: Option<Register>,
        index: Option<Register>,
        offset: usize,
        scalar: Size, /* The scalar can only be 1, 2, 4 or 8 */
        size: Size,
    },

    // Constant integer, and strings are usually encoded directly into the executable. This behaviour may change depending on where they are used.
    // Character constants are converted into 1 byte integers
    ConstantInteger {
        value: u64,
        size: Size,
    },

    ConstantString {
        value: String,
    },

    Identifier {
        identifier: String,
    },

    MemoryReserve {
        value: Box<Token>,
        size: Box<Token>, /* Size is a token to allow you to use identifiers instead of just integers */
    },

    Directive {
        directive: String,
        arguments: Vec<String>,
    },

    // Register is only a valid token if used as a operand inside of an instruction
    Register {
        register: Register,
    },
}
