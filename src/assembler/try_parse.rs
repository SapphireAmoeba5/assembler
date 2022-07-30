use super::AssemblerResult;
use super::Size;
use crate::debug_println;
use std::borrow::Cow;

/// Takes in a string surrounded by quotation marks. If the string does not end or start with double quotes it returns Ok(None), if the string begins or ends with a quotation, but not both it returns Err. On success this function will return the given string with the quotation marks removed
pub fn try_parse_string(string: &str) -> AssemblerResult<Option<&str>> {
    // If the string contains neither the starting or ending quotation mark, then this is just not a string.
    if !string.starts_with('"') && !string.ends_with('"') {
        return Ok(None);
    }

    if !string.starts_with('"') {
        return Err(Cow::from("Missing beginning quotation mark"));
    } else if !string.ends_with('"') {
        return Err(Cow::from("Missing ending quotation mark"));
    }

    Ok(Some(&string[1..string.len() - 1]))
}

pub fn try_parse_identifier(identifier: &str) -> AssemblerResult<&str> {
    if identifier.starts_with(|ch: char| ch.is_numeric()) {
        return Err(Cow::from(format!(
            "Identifier \"{}\" cannot begin with a numeric integer",
            identifier
        )));
    }

    if !identifier.chars().all(valid_identifier_character) {
        return Err(Cow::from("Invalid identifier character"));
    }

    return Ok(identifier);
}

pub fn try_parse_number(number: &str) -> AssemblerResult<Option<(u64, Size)>> {
    if !number.starts_with(|ch: char| ch.is_numeric() || ch == '-') {
        return Ok(None);
    }

    let mut number = number;
    let mut size = Size::Eight;

    let mut sign = 1;
    if number.starts_with('-') {
        sign = -1;
        number = &number[1..];
    }

    if number.ends_with("u8") || number.ends_with("i8") {
        size = Size::One;
        number = &number[0..number.len() - 2];
    } else if number.ends_with("u16") || number.ends_with("i16") {
        size = Size::Two;
        number = &number[0..number.len() - 3];
    } else if number.ends_with("u32") || number.ends_with("i32") {
        size = Size::Four;
        number = &number[0..number.len() - 3];
    } else if number.ends_with("u64") || number.ends_with("i64") {
        size = Size::Eight;
        number = &number[0..number.len() - 3];
    }

    match try_parse_hex(number) {
        Ok(Some(val)) => {
            return Ok(Some((
                if sign == 1 { val } else { val.wrapping_neg() },
                size,
            )))
        }
        Err(e) => return Err(e),
        _ => {}
    };

    match try_parse_binary(number) {
        Ok(Some(val)) => {
            return Ok(Some((
                if sign == 1 { val } else { val.wrapping_neg() },
                size,
            )))
        }
        Err(e) => return Err(e),
        _ => {}
    }

    match try_parse_decimal(number) {
        Ok(val) => Ok(Some((
            if sign == 1 { val } else { val.wrapping_neg() },
            size,
        ))),
        Err(e) => Err(e),
    }
}

fn try_parse_hex(hex: &str) -> AssemblerResult<Option<u64>> {
    if !hex.starts_with("0x") {
        return Ok(None);
    }

    let hex = &hex[2..];

    if hex.is_empty() {
        return Err(Cow::from("Invalid hexadecimal value"));
    }

    match u64::from_str_radix(hex, 16) {
        Ok(val) => Ok(Some(val)),
        Err(e) => Err(Cow::from(format!("{}", e))),
    }
}

fn try_parse_binary(bin: &str) -> AssemblerResult<Option<u64>> {
    if !bin.starts_with("0b") {
        return Ok(None);
    }

    let bin = &bin[2..];

    if bin.is_empty() {
        return Err(Cow::from("Empty binary value"));
    }

    match u64::from_str_radix(bin, 2) {
        Ok(val) => Ok(Some(val)),
        Err(e) => Err(Cow::from(format!("{}", e))),
    }
}

fn try_parse_decimal(bin: &str) -> AssemblerResult<u64> {
    match bin.parse::<u64>() {
        Ok(val) => Ok(val),
        Err(e) => Err(Cow::from(format!("{}", e))),
    }
}

fn valid_identifier_character(ch: char) -> bool {
    ch.is_alphabetic() || ch.is_numeric() || ch == '_' || ch == '$' || ch == '-'
}
