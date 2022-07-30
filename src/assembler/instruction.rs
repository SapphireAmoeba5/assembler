use std::str::FromStr;

#[derive(Debug)]
pub enum Instruction {
    Mov,
    Add,
    Sub,
    Mul,
    Div,
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

            _ => Err(()),
        }
    }
}
