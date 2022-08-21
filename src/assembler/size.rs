use std::convert::TryFrom;
use std::str::FromStr;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Size {
    One = 1,
    Two = 2,
    Four = 4,
    Eight = 8,
}

impl TryFrom<u64> for Size {
    type Error = ();

    fn try_from(value: u64) -> Result<Self, Self::Error> {
        match value {
            1 => Ok(Self::One),
            2 => Ok(Self::Two),
            4 => Ok(Self::Four),
            8 => Ok(Self::Eight),
            _ => Err(()),
        }
    }
}

impl FromStr for Size {
    type Err = ();
    fn from_str(value: &str) -> Result<Self, Self::Err> {
        match value {
            "u8" | "i8" => Ok(Self::One),
            "u16" | "i16" => Ok(Self::Two),
            "u32" | "i32" => Ok(Self::Four),
            "u64" | "i64" => Ok(Self::Eight),
            _ => Err(()),
        }
    }
}
