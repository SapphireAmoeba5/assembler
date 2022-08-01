use super::register::Register;
use super::Instruction;
use super::Size;

#[derive(Debug)]
pub enum Token {
    Instruction {
        instruction: Instruction,
        arguments: Vec<Token>,
        line_number: usize,
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
        line_number: usize,
    },

    ConstantString {
        value: String,
        line_number: usize,
    },

    Identifier {
        identifier: String,
        line_number: usize,
    },

    MemoryReserve {
        value: Box<Token>,
        size: Box<Token>, /* Size is a token to allow you to use identifiers instead of just integers */
        line_number: usize,
    }, // Memory reserving allows you to insert data directly into the executable

    Directive {
        directive: String,
        arguments: Vec<String>,
        line_number: usize,
    },

    Register {
        register: Register,
        line_number: usize,
    },
}
