pub enum TokenType {
    Instruction(String),
    ConstantInteger(u64),
    ConstantFloat(f64),
    ConstantString(String),
    Identifier(String),
    Label(String),
}

pub struct Token {
    pub token_type: TokenType,
    pub line_number: usize,
}
