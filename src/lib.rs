use std::borrow::Borrow;
use std::collections::HashMap;
use std::fmt::*;
use std::hash::Hash;

use unicode_ident::{is_xid_continue, is_xid_start};

// type Result<T = (), E = ()> = std::result::Result<T, E>;

macro_rules! ensure {
    ($condition:expr) => {
        if !$condition {
            return None;
        }
    };
}

#[derive(Default)]
pub struct Formattable<'a> {
    binary: Option<&'a dyn Binary>,
    debug: Option<&'a dyn Debug>,
    display: Option<&'a dyn Display>,
    lower_exp: Option<&'a dyn LowerExp>,
    lower_hex: Option<&'a dyn LowerHex>,
    octal: Option<&'a dyn Octal>,
    pointer: Option<&'a dyn Pointer>,
    upper_exp: Option<&'a dyn UpperExp>,
    upper_hex: Option<&'a dyn UpperHex>,
}

macro_rules! formattable_fn {
    ($name:ident, $builder:ident<$($traits:ident),*> $($fields:ident),+) => {
        pub fn $name<T: $($traits+)*>(value: &'a T) -> Self {
            Self {
                $($fields: Some(value),)*
                ..Default::default()
            }
        }
        pub fn $builder<T: $($traits+)*>(mut self, value: &'a T) -> Self {
            $(self.$fields = Some(value);)*
            self
        }
    };
    ($name:ident, $builder:ident<$($traits:ident),*>) => {
        formattable_fn!($name, $builder<$($traits),*> $name);
    }
}
macro_rules! formattable {
    [$($name:ident, $builder:ident<$($traits:ident),*> $($fields:ident),*;)*] => {
        impl<'a> Formattable<'a> {
            $(formattable_fn!($name, $builder<$($traits),*> $($fields),*);)*
        }
    };
}

formattable![
    debug_display, and_debug_display<Debug, Display> debug, display;
    debug, and_debug<Debug>;
    display, and_display<Display>;
    number, and_number<Binary, Debug, Display, LowerExp, LowerHex, Octal, UpperExp, UpperHex>
        binary, debug, display, lower_exp, lower_hex, octal, upper_exp, upper_hex;
    binary, and_binary<Binary>;
    lower_exp, and_lower_exp<LowerExp>;
    lower_hex, and_lower_hex<LowerHex>;
    octal, and_octal<Octal>;
    pointer, and_pointer<Pointer>;
    upper_exp, and_upper_exp<UpperExp>;
    upper_hex, and_upper_hex<UpperHex>;
];

pub fn format<K: Borrow<str> + Eq + Hash>(
    mut format: &str,
    context: HashMap<K, Formattable>,
) -> Option<String> {
    let mut out = String::with_capacity(format.len());
    while !format.is_empty() {
        if format.starts_with("{{") || format.starts_with("}}") {
            out.push_str(&format[..1]);
            format = &format[2..];
            continue;
        }
        if format.starts_with('{') {
            format = format[1..].trim_start();
            ensure!(format.starts_with('_') || format.starts_with(is_xid_start));
            let variable_len = format[1..].find(|c| !is_xid_continue(c))?;
            let variable = &format[..1 + variable_len];
            let variable = context.get(variable)?;
            format = &format[1 + variable_len..];
            if format.starts_with(':') {
                format = &format[1..];
                let width_len = format.find(|c: char| !c.is_ascii_digit()).unwrap();
                let mut width = 0;
                if width_len != 0 {
                    width = format[..width_len].parse().ok().unwrap();
                    format = &format[width_len..];
                };
                dbg!(format);
                match format.as_bytes()[0] {
                    b'#' => {
                        format = &format[1..];
                        if format.starts_with('}') {
                            write!(out, "{:#}", variable.display?).ok()?;
                        } else {
                            match format.as_bytes()[0] {
                                b'?' => write!(out, "{:#width$?}", variable.debug?).ok()?,
                                b'b' => write!(out, "{:#width$b}", variable.binary?).ok()?,
                                b'e' => write!(out, "{:#width$e}", variable.lower_exp?).ok()?,
                                b'x' if format.as_bytes()[1] == b'?' => {
                                    format = &format[1..];
                                    write!(out, "{:#width$x?}", variable.debug?).ok()?
                                }
                                b'x' => write!(out, "{:#width$x}", variable.lower_hex?).ok()?,
                                b'o' => write!(out, "{:#width$o}", variable.octal?).ok()?,
                                b'p' => write!(out, "{:#width$p}", variable.pointer?).ok()?,
                                b'E' => write!(out, "{:#width$E}", variable.upper_exp?).ok()?,
                                b'X' if format.as_bytes()[1] == b'?' => {
                                    format = &format[1..];
                                    write!(out, "{:#width$X?}", variable.debug?).ok()?
                                }
                                b'X' => write!(out, "{:#width$X}", variable.upper_hex?).ok()?,
                                _ => return None,
                            }
                            format = &format[1..];
                        }
                    }
                    b'x' if format.as_bytes()[1] == b'?' => {
                        format = &format[2..];
                        write!(out, "{:width$x?}", variable.debug?).ok()?;
                    }
                    b'X' if format.as_bytes()[1] == b'?' => {
                        format = &format[2..];
                        write!(out, "{:width$X?}", variable.debug?).ok()?;
                    }
                    b'}' => {
                        write!(out, "{:width$}", variable.display?).ok()?;
                    }
                    b => {
                        match b {
                            b'?' => write!(out, "{:width$?}", variable.debug?).ok()?,
                            b'b' => write!(out, "{:width$b}", variable.binary?).ok()?,
                            b'e' => write!(out, "{:width$e}", variable.lower_exp?).ok()?,
                            b'x' => write!(out, "{:width$x}", variable.lower_hex?).ok()?,
                            b'o' => write!(out, "{:width$o}", variable.octal?).ok()?,
                            b'p' => write!(out, "{:width$p}", variable.pointer?).ok()?,
                            b'E' => write!(out, "{:width$E}", variable.upper_exp?).ok()?,
                            b'X' => write!(out, "{:width$X}", variable.upper_hex?).ok()?,
                            _ => return None,
                        }
                        format = &format[1..];
                    }
                }
            } else {
                write!(out, "{}", variable.display?).ok()?
            }
            ensure!(format.starts_with('}'));
            format = &format[1..];
            continue;
        }
        out.push_str(&format[..1]);
        format = &format[1..];
    }
    Some(out)
}

#[test]
fn test_format() {
    assert_eq!(
        format("{{hello}}", HashMap::<String, Formattable>::new()).unwrap(),
        "{hello}"
    );
    assert_eq!(
        format(
            "{hi:10} {hi:?} {int:#x?} {int:b} {int:#X} {int:4o}",
            [
                ("hi", Formattable::debug_display(&"hello")),
                ("int", Formattable::number(&123u8))
            ]
            .into_iter()
            .collect()
        )
        .unwrap(),
        r#"hello      "hello" 0x7b 1111011 0x7B  173"#
    );
}
