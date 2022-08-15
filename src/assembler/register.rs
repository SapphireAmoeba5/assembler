use super::Size;
use std::str::FromStr;

#[derive(Debug, Clone, Copy)]
pub enum Register {
    // Each register enum contains a single usize which contains the size of the register. that usize should always be either 1, 2, 4, or 8
    X0(Size),
    X1(Size),
    X2(Size),
    X3(Size),
    X4(Size),
    SP(Size),
    IP(Size),
}

impl Register {
    pub fn get_size(&self) -> Size {
        match self {
            Self::X0(size) => *size,
            Self::X1(size) => *size,
            Self::X2(size) => *size,
            Self::X3(size) => *size,
            Self::X4(size) => *size,
            Self::IP(size) => *size,
            Self::SP(size) => *size,
        }
    }

    pub fn get_register_id(&self) -> u8 {
        match self {
            Self::X0(_) => 1,
            Self::X1(_) => 2,
            Self::X2(_) => 3,
            Self::X3(_) => 4,
            Self::X4(_) => 5,
            Self::SP(_) => 6,
            Self::IP(_) => 7,
        }
    }
}

impl FromStr for Register {
    type Err = ();

    fn from_str(string: &str) -> Result<Self, Self::Err> {
        match string {
            "x0" => Ok(Self::X0(Size::Eight)),
            "x1" => Ok(Self::X1(Size::Eight)),
            "x2" => Ok(Self::X2(Size::Eight)),
            "x3" => Ok(Self::X3(Size::Eight)),
            "x4" => Ok(Self::X4(Size::Eight)),
            "ip" => Ok(Self::IP(Size::Eight)),
            "sp" => Ok(Self::SP(Size::Eight)),

            "d0" => Ok(Self::X0(Size::Four)),
            "d1" => Ok(Self::X1(Size::Four)),
            "d2" => Ok(Self::X2(Size::Four)),
            "d3" => Ok(Self::X3(Size::Four)),
            "d4" => Ok(Self::X4(Size::Four)),
            "dip" => Ok(Self::IP(Size::Four)),
            "dsp" => Ok(Self::SP(Size::Four)),

            "w0" => Ok(Self::X0(Size::Two)),
            "w1" => Ok(Self::X1(Size::Two)),
            "w2" => Ok(Self::X2(Size::Two)),
            "w3" => Ok(Self::X3(Size::Two)),
            "w4" => Ok(Self::X4(Size::Two)),
            "wip" => Ok(Self::IP(Size::Two)),
            "wsp" => Ok(Self::SP(Size::Two)),

            "b0" => Ok(Self::X0(Size::One)),
            "b1" => Ok(Self::X1(Size::One)),
            "b2" => Ok(Self::X2(Size::One)),
            "b3" => Ok(Self::X3(Size::One)),
            "b4" => Ok(Self::X4(Size::One)),
            "bip" => Ok(Self::IP(Size::One)),
            "bsp" => Ok(Self::SP(Size::One)),

            _ => Err(()),
        }
    }
}
