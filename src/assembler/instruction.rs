use std::str::FromStr;

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
pub enum Instruction {
    Mov = 0x01,
    Add = 0x03,
    Sub = 0x13,
    Mul = 0x23,
    Div = 0x33,
    Or = 0x04,
    Xor = 0x14,
    And = 0x24,
    Cmp = 0x11,
    Not = 0x34,
    Neg = 0x44,

    Push = 0x21,
    Pop = 0x31,
    Pushf = 0x71,
    Popf = 0x81,

    Hlt = 0x00,
    Nop = 0x90,

    Str = 0x41,
    Ldr = 0x51,
    Lea = 0x61,

    Jmp = 0x05,
    Jz = 0x15,
    Jnz = 0x25,
    Jo = 0x35,
    Jno = 0x45,
    Js = 0x55,
    Jns = 0x65,
    Jc = 0x75,
    Jnc = 0x85,
    Jbe = 0x95,
    Ja = 0xa5,
    Jl = 0xb5,
    Jge = 0xc5,
    Jle = 0xd5,
    Jg = 0xe5,

    Call = 0x06,
    Ret = 0x16,

    Lidt = 0x08,
    Reti = 0x28,
    Int = 0x18,
    Cli = 0x38,
    Sti = 0x48,

    In = 0x10,
    Out = 0x20,
}

impl FromStr for Instruction {
    type Err = ();

    fn from_str(string: &str) -> Result<Self, Self::Err> {
        match string {
            "mov" => Ok(Self::Mov),
            "add" => Ok(Self::Add),
            "sub" => Ok(Self::Sub),
            "mul" => Ok(Self::Mul),
            "div" => Ok(Self::Div),
            "or" => Ok(Self::Or),
            "xor" => Ok(Self::Xor),
            "and" => Ok(Self::And),
            "cmp" => Ok(Self::Cmp),
            "not" => Ok(Self::Not),
            "neg" => Ok(Self::Neg),
            "push" => Ok(Self::Push),
            "pop" => Ok(Self::Pop),
            "pushf" => Ok(Self::Pushf),
            "popf" => Ok(Self::Popf),

            "hlt" => Ok(Self::Hlt),
            "nop" => Ok(Self::Nop),

            "str" => Ok(Self::Str),
            "ldr" => Ok(Self::Ldr),
            "lea" => Ok(Self::Lea),

            "jmp" => Ok(Self::Jmp),
            "jz" | "je" => Ok(Self::Jz),
            "jnz" | "jne" => Ok(Self::Jnz),
            "jo" => Ok(Self::Jo),
            "jno" => Ok(Self::Jno),
            "js" => Ok(Self::Js),
            "jns" => Ok(Self::Jns),
            "jc" => Ok(Self::Jc),
            "jnc" => Ok(Self::Jnc),
            "jbe" => Ok(Self::Jbe),
            "ja" => Ok(Self::Ja),
            "jl" => Ok(Self::Jl),
            "jge" => Ok(Self::Jge),
            "jle" => Ok(Self::Jle),
            "jg" => Ok(Self::Jg),

            "call" => Ok(Self::Call),
            "ret" => Ok(Self::Ret),

            "lidt" => Ok(Self::Lidt),
            "reti" => Ok(Self::Reti),
            "int" => Ok(Self::Int),
            "cli" => Ok(Self::Cli),
            "sti" => Ok(Self::Sti),

            "in" => Ok(Self::In),
            "out" => Ok(Self::Out),

            _ => Err(()),
        }
    }
}
