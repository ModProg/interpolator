use std::borrow::Borrow;
use std::collections::HashMap;
use std::fmt::{
    Alignment, Binary, Debug, Display, LowerExp, LowerHex, Octal, Pointer, UpperExp, UpperHex,
    Write,
};
use std::hash::Hash;

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

impl<'a, T: Display> From<&'a T> for Formattable<'a> {
    fn from(value: &'a T) -> Self {
        Self {
            display: Some(value),
            ..Default::default()
        }
    }
}

fn alignment(c: u8) -> Option<Alignment> {
    match c {
        b'<' => Some(Alignment::Left),
        b'^' => Some(Alignment::Center),
        b'>' => Some(Alignment::Right),
        _ => None,
    }
}

#[derive(Debug, Default)]
enum Sign {
    Plus,
    Minus,
    #[default]
    No,
}

#[derive(Debug, Default)]
#[allow(non_camel_case_types)]
enum Trait {
    #[default]
    Display,
    Question,
    xQuestion,
    XQuestion,
    x,
    X,
    b,
    o,
    e,
    E,
    p,
}

impl Trait {
    fn from_str(format: &mut &str) -> Option<Self> {
        match format.as_bytes()[0] {
            b'?' => Some(Trait::Question),
            b'x' if format.as_bytes()[1] == b'?' => {
                step(1, format);
                Some(Trait::xQuestion)
            }
            b'X' if format.as_bytes()[1] == b'?' => {
                step(1, format);
                Some(Trait::XQuestion)
            }
            b'o' => Some(Trait::o),
            b'x' => Some(Trait::x),
            b'X' => Some(Trait::X),
            b'p' => Some(Trait::p),
            b'b' => Some(Trait::b),
            b'e' => Some(Trait::e),
            b'E' => Some(Trait::E),
            _ => None,
        }
        .map(|m| {
            step(1, format);
            m
        })
        .or_else(|| format.starts_with('}').then_some(Trait::Display))
    }
}

#[derive(Default, Debug)]
struct FormatArgument<'a> {
    ident: &'a str,
    alignment: Option<(Option<char>, Alignment)>,
    sign: Sign,
    hash: bool,
    zero: bool,
    width: Option<usize>,
    precision: Option<usize>,
    trait_: Trait,
}

fn step(len: usize, format: &mut &str) {
    *format = &format[len..];
}

impl<'a> FormatArgument<'a> {
    fn from_str(format: &mut &'a str) -> Option<Self> {
        let mut it = Self::default();
        step(
            format.find(|c: char| !c.is_whitespace()).unwrap_or(0),
            format,
        );
        let variable_len = format.find(|c| c == ':' || c == '}')?;
        it.ident = &format[..variable_len];
        step(variable_len, format);
        if format.starts_with(':') {
            step(1, format);
            if format.is_empty() {
                return Some(it);
            }
            let fill = format.chars().next()?;
            if format[fill.len_utf8()..].is_empty() {
                return Some(it);
            }
            if let Some(alignment) = alignment(format[fill.len_utf8()..].as_bytes()[0]) {
                it.alignment = Some((Some(fill), alignment));
                step(2, format)
            } else if fill.is_ascii() {
                if let Some(alignment) = alignment(fill as u8) {
                    it.alignment = Some((None, alignment));
                    step(1, format);
                }
            }
            if format[fill.len_utf8()..].is_empty() {
                return Some(it);
            }
            match format.as_bytes()[0] {
                b'+' => {
                    step(1, format);
                    it.sign = Sign::Plus
                }
                b'-' => {
                    step(1, format);
                    it.sign = Sign::Minus
                }
                _ => {}
            }
            if format[fill.len_utf8()..].is_empty() {
                return Some(it);
            }
            if b'#' == format.as_bytes()[0] {
                step(1, format);
                it.hash = true;
            }
            if format.starts_with('0') {
                it.zero = true;
                step(1, format);
            }
            if format.starts_with(|c: char| c.is_ascii_digit()) {
                let width = format.find(|c: char| !c.is_ascii_digit())?;
                it.width = Some(format[..width].parse().ok()?);
                step(width, format);
            }
            if format.starts_with('.') {
                step(1, format);
                let precision = format.find(|c: char| !c.is_ascii_digit())?;
                it.precision = Some(format[..precision].parse().ok()?);
                step(precision, format);
            }
            it.trait_ = Trait::from_str(format)?
        }
        Some(it)
    }
}

macro_rules! w {
    ($out:ident, $fmt:literal, $variable:ident) => {
        write!($out, $fmt, $variable.display?).ok()?
    };
    ($out:ident, $fmt:literal, $variable:ident. $field:ident) => {
        write!($out, $fmt, $variable.$field?).ok()?
    };
}

pub fn format<K: Borrow<str> + Eq + Hash>(
    mut format: &str,
    context: HashMap<K, Formattable>,
) -> Option<String> {
    let format = &mut format;
    use Alignment::*;
    use Sign::*;
    use Trait::*;
    let mut out = String::with_capacity(format.len());
    while !format.is_empty() {
        if format.starts_with("{{") || format.starts_with("}}") {
            out.push_str(&format[..1]);
            step(2, format);
            continue;
        }
        if format.starts_with('{') {
            step(1, format);
            let FormatArgument {
                ident,
                alignment,
                sign,
                hash,
                zero,
                width,
                precision,
                trait_,
            } = FormatArgument::from_str(format)?;
            let wi = width.unwrap_or(0);
            let pr = precision.unwrap_or(usize::MAX);
            let v = context.get(ident)?;
            #[rustfmt::skip]
            match (alignment, sign, hash, zero, trait_) { 
                (None, No, false, false, Display) => w!(out, "{:wi$.pr$}", v.display),
                (None, No, false, false, Question) => w!(out, "{:wi$.pr$?}", v.debug),
                (None, No, false, false, xQuestion) => w!(out, "{:wi$.pr$x?}", v.debug),
                (None, No, false, false, XQuestion) => w!(out, "{:wi$.pr$X?}", v.debug),
                (None, No, false, false, x) => w!(out, "{:wi$.pr$x}", v.lower_hex),
                (None, No, false, false, X) => w!(out, "{:wi$.pr$X}", v.upper_hex),
                (None, No, false, false, b) => w!(out, "{:wi$.pr$b}", v.binary),
                (None, No, false, false, o) => w!(out, "{:wi$.pr$o}", v.octal),
                (None, No, false, false, e) => w!(out, "{:wi$.pr$e}", v.lower_exp),
                (None, No, false, false, E) => w!(out, "{:wi$.pr$E}", v.upper_exp),
                (None, No, false, false, p) => w!(out, "{:wi$.pr$p}", v.pointer),
                (None, No, false, true, Display) => w!(out, "{:0wi$.pr$}", v.display),
                (None, No, false, true, Question) => w!(out, "{:0?}", v.debug),
                (None, No, false, true, xQuestion) => w!(out, "{:0x?}", v.debug),
                (None, No, false, true, XQuestion) => w!(out, "{:0X?}", v.debug),
                (None, No, false, true, x) => w!(out, "{:0wi$.pr$x}", v.lower_hex),
                (None, No, false, true, X) => w!(out, "{:0wi$.pr$X}", v.upper_hex),
                (None, No, false, true, b) => w!(out, "{:0wi$.pr$b}", v.binary),
                (None, No, false, true, o) => w!(out, "{:0wi$.pr$o}", v.octal),
                (None, No, false, true, e) => w!(out, "{:0wi$.pr$e}", v.lower_exp),
                (None, No, false, true, E) => w!(out, "{:0wi$.pr$E}", v.upper_exp),
                (None, No, false, true, p) => w!(out, "{:0wi$.pr$p}", v.pointer),
                (None, Plus, false, false, Display) => w!(out, "{:+wi$.pr$}", v.display),
                (None, Plus, false, false, Question) => w!(out, "{:+wi$.pr$?}", v.debug),
                (None, Plus, false, false, xQuestion) => w!(out, "{:+wi$.pr$x?}", v.debug),
                (None, Plus, false, false, XQuestion) => w!(out, "{:+wi$.pr$X?}", v.debug),
                (None, Plus, false, false, x) => w!(out, "{:+wi$.pr$x}", v.lower_hex),
                (None, Plus, false, false, X) => w!(out, "{:+wi$.pr$X}", v.upper_hex),
                (None, Plus, false, false, b) => w!(out, "{:+wi$.pr$b}", v.binary),
                (None, Plus, false, false, o) => w!(out, "{:+wi$.pr$o}", v.octal),
                (None, Plus, false, false, e) => w!(out, "{:+wi$.pr$e}", v.lower_exp),
                (None, Plus, false, false, E) => w!(out, "{:+wi$.pr$E}", v.upper_exp),
                (None, Plus, false, false, p) => w!(out, "{:+wi$.pr$p}", v.pointer),
                (None, Plus, false, true, Display) => w!(out, "{:+0wi$.pr$}", v.display),
                (None, Plus, false, true, Question) => w!(out, "{:+0wi$.pr$?}", v.debug),
                (None, Plus, false, true, xQuestion) => w!(out, "{:+0wi$.pr$x?}", v.debug),
                (None, Plus, false, true, XQuestion) => w!(out, "{:+0wi$.pr$X?}", v.debug),
                (None, Plus, false, true, x) => w!(out, "{:+0wi$.pr$x}", v.lower_hex),
                (None, Plus, false, true, X) => w!(out, "{:+0wi$.pr$X}", v.upper_hex),
                (None, Plus, false, true, b) => w!(out, "{:+0wi$.pr$b}", v.binary),
                (None, Plus, false, true, o) => w!(out, "{:+0wi$.pr$o}", v.octal),
                (None, Plus, false, true, e) => w!(out, "{:+0wi$.pr$e}", v.lower_exp),
                (None, Plus, false, true, E) => w!(out, "{:+0wi$.pr$E}", v.upper_exp),
                (None, Plus, false, true, p) => w!(out, "{:+0wi$.pr$p}", v.pointer),
                (None, Minus, false, false, Display) => w!(out, "{:-wi$.pr$}", v.display),
                (None, Minus, false, false, Question) => w!(out, "{:-wi$.pr$?}", v.debug),
                (None, Minus, false, false, xQuestion) => w!(out, "{:-wi$.pr$x?}", v.debug),
                (None, Minus, false, false, XQuestion) => w!(out, "{:-wi$.pr$X?}", v.debug),
                (None, Minus, false, false, x) => w!(out, "{:-wi$.pr$x}", v.lower_hex),
                (None, Minus, false, false, X) => w!(out, "{:-wi$.pr$X}", v.upper_hex),
                (None, Minus, false, false, b) => w!(out, "{:-wi$.pr$b}", v.binary),
                (None, Minus, false, false, o) => w!(out, "{:-wi$.pr$o}", v.octal),
                (None, Minus, false, false, e) => w!(out, "{:-wi$.pr$e}", v.lower_exp),
                (None, Minus, false, false, E) => w!(out, "{:-wi$.pr$E}", v.upper_exp),
                (None, Minus, false, false, p) => w!(out, "{:-wi$.pr$p}", v.pointer),
                (None, Minus, false, true, Display) => w!(out, "{:-0wi$.pr$}", v.display),
                (None, Minus, false, true, Question) => w!(out, "{:-0wi$.pr$?}", v.debug),
                (None, Minus, false, true, xQuestion) => w!(out, "{:-0wi$.pr$x?}", v.debug),
                (None, Minus, false, true, XQuestion) => w!(out, "{:-0wi$.pr$X?}", v.debug),
                (None, Minus, false, true, x) => w!(out, "{:-0wi$.pr$x}", v.lower_hex),
                (None, Minus, false, true, X) => w!(out, "{:-0wi$.pr$X}", v.upper_hex),
                (None, Minus, false, true, b) => w!(out, "{:-0wi$.pr$b}", v.binary),
                (None, Minus, false, true, o) => w!(out, "{:-0wi$.pr$o}", v.octal),
                (None, Minus, false, true, e) => w!(out, "{:-0wi$.pr$e}", v.lower_exp),
                (None, Minus, false, true, E) => w!(out, "{:-0wi$.pr$E}", v.upper_exp),
                (None, Minus, false, true, p) => w!(out, "{:-0wi$.pr$p}", v.pointer),
                (Some((None, Left)), No, false, false, Display) => w!(out, "{:<wi$.pr$}", v.display),
                (Some((None, Left)), No, false, false, Question) => w!(out, "{:<wi$.pr$?}", v.debug),
                (Some((None, Left)), No, false, false, xQuestion) => w!(out, "{:<wi$.pr$x?}", v.debug),
                (Some((None, Left)), No, false, false, XQuestion) => w!(out, "{:<wi$.pr$X?}", v.debug),
                (Some((None, Left)), No, false, false, x) => w!(out, "{:<wi$.pr$x}", v.lower_hex),
                (Some((None, Left)), No, false, false, X) => w!(out, "{:<wi$.pr$X}", v.upper_hex),
                (Some((None, Left)), No, false, false, b) => w!(out, "{:<wi$.pr$b}", v.binary),
                (Some((None, Left)), No, false, false, o) => w!(out, "{:<wi$.pr$o}", v.octal),
                (Some((None, Left)), No, false, false, e) => w!(out, "{:<wi$.pr$e}", v.lower_exp),
                (Some((None, Left)), No, false, false, E) => w!(out, "{:<wi$.pr$E}", v.upper_exp),
                (Some((None, Left)), No, false, false, p) => w!(out, "{:<wi$.pr$p}", v.pointer),
                (Some((None, Left)), No, false, true, Display) => w!(out, "{:<0wi$.pr$}", v.display),
                (Some((None, Left)), No, false, true, Question) => w!(out, "{:<0wi$.pr$?}", v.debug),
                (Some((None, Left)), No, false, true, xQuestion) => w!(out, "{:<0wi$.pr$x?}", v.debug),
                (Some((None, Left)), No, false, true, XQuestion) => w!(out, "{:<0wi$.pr$X?}", v.debug),
                (Some((None, Left)), No, false, true, x) => w!(out, "{:<0wi$.pr$x}", v.lower_hex),
                (Some((None, Left)), No, false, true, X) => w!(out, "{:<0wi$.pr$X}", v.upper_hex),
                (Some((None, Left)), No, false, true, b) => w!(out, "{:<0wi$.pr$b}", v.binary),
                (Some((None, Left)), No, false, true, o) => w!(out, "{:<0wi$.pr$o}", v.octal),
                (Some((None, Left)), No, false, true, e) => w!(out, "{:<0wi$.pr$e}", v.lower_exp),
                (Some((None, Left)), No, false, true, E) => w!(out, "{:<0wi$.pr$E}", v.upper_exp),
                (Some((None, Left)), No, false, true, p) => w!(out, "{:<0wi$.pr$p}", v.pointer),
                (Some((None, Left)), Plus, false, false, Display) => w!(out, "{:<+wi$.pr$}", v.display),
                (Some((None, Left)), Plus, false, false, Question) => w!(out, "{:<+wi$.pr$?}", v.debug),
                (Some((None, Left)), Plus, false, false, xQuestion) => w!(out, "{:<+wi$.pr$x?}", v.debug),
                (Some((None, Left)), Plus, false, false, XQuestion) => w!(out, "{:<+wi$.pr$X?}", v.debug),
                (Some((None, Left)), Plus, false, false, x) => w!(out, "{:<+wi$.pr$x}", v.lower_hex),
                (Some((None, Left)), Plus, false, false, X) => w!(out, "{:<+wi$.pr$X}", v.upper_hex),
                (Some((None, Left)), Plus, false, false, b) => w!(out, "{:<+wi$.pr$b}", v.binary),
                (Some((None, Left)), Plus, false, false, o) => w!(out, "{:<+wi$.pr$o}", v.octal),
                (Some((None, Left)), Plus, false, false, e) => w!(out, "{:<+wi$.pr$e}", v.lower_exp),
                (Some((None, Left)), Plus, false, false, E) => w!(out, "{:<+wi$.pr$E}", v.upper_exp),
                (Some((None, Left)), Plus, false, false, p) => w!(out, "{:<+wi$.pr$p}", v.pointer),
                (Some((None, Left)), Plus, false, true, Display) => w!(out, "{:<+0wi$.pr$}", v.display),
                (Some((None, Left)), Plus, false, true, Question) => w!(out, "{:<+0wi$.pr$?}", v.debug),
                (Some((None, Left)), Plus, false, true, xQuestion) => w!(out, "{:<+0wi$.pr$x?}", v.debug),
                (Some((None, Left)), Plus, false, true, XQuestion) => w!(out, "{:<+0wi$.pr$X?}", v.debug),
                (Some((None, Left)), Plus, false, true, x) => w!(out, "{:<+0wi$.pr$x}", v.lower_hex),
                (Some((None, Left)), Plus, false, true, X) => w!(out, "{:<+0wi$.pr$X}", v.upper_hex),
                (Some((None, Left)), Plus, false, true, b) => w!(out, "{:<+0wi$.pr$b}", v.binary),
                (Some((None, Left)), Plus, false, true, o) => w!(out, "{:<+0wi$.pr$o}", v.octal),
                (Some((None, Left)), Plus, false, true, e) => w!(out, "{:<+0wi$.pr$e}", v.lower_exp),
                (Some((None, Left)), Plus, false, true, E) => w!(out, "{:<+0wi$.pr$E}", v.upper_exp),
                (Some((None, Left)), Plus, false, true, p) => w!(out, "{:<+0wi$.pr$p}", v.pointer),
                (Some((None, Left)), Minus, false, false, Display) => w!(out, "{:<-wi$.pr$}", v.display),
                (Some((None, Left)), Minus, false, false, Question) => w!(out, "{:<-wi$.pr$?}", v.debug),
                (Some((None, Left)), Minus, false, false, xQuestion) => w!(out, "{:<-wi$.pr$x?}", v.debug),
                (Some((None, Left)), Minus, false, false, XQuestion) => w!(out, "{:<-wi$.pr$X?}", v.debug),
                (Some((None, Left)), Minus, false, false, x) => w!(out, "{:<-wi$.pr$x}", v.lower_hex),
                (Some((None, Left)), Minus, false, false, X) => w!(out, "{:<-wi$.pr$X}", v.upper_hex),
                (Some((None, Left)), Minus, false, false, b) => w!(out, "{:<-wi$.pr$b}", v.binary),
                (Some((None, Left)), Minus, false, false, o) => w!(out, "{:<-wi$.pr$o}", v.octal),
                (Some((None, Left)), Minus, false, false, e) => w!(out, "{:<-wi$.pr$e}", v.lower_exp),
                (Some((None, Left)), Minus, false, false, E) => w!(out, "{:<-wi$.pr$E}", v.upper_exp),
                (Some((None, Left)), Minus, false, false, p) => w!(out, "{:<-wi$.pr$p}", v.pointer),
                (Some((None, Left)), Minus, false, true, Display) => w!(out, "{:<-0wi$.pr$}", v.display),
                (Some((None, Left)), Minus, false, true, Question) => w!(out, "{:<-0wi$.pr$?}", v.debug),
                (Some((None, Left)), Minus, false, true, xQuestion) => w!(out, "{:<-0wi$.pr$x?}", v.debug),
                (Some((None, Left)), Minus, false, true, XQuestion) => w!(out, "{:<-0wi$.pr$X?}", v.debug),
                (Some((None, Left)), Minus, false, true, x) => w!(out, "{:<-0wi$.pr$x}", v.lower_hex),
                (Some((None, Left)), Minus, false, true, X) => w!(out, "{:<-0wi$.pr$X}", v.upper_hex),
                (Some((None, Left)), Minus, false, true, b) => w!(out, "{:<-0wi$.pr$b}", v.binary),
                (Some((None, Left)), Minus, false, true, o) => w!(out, "{:<-0wi$.pr$o}", v.octal),
                (Some((None, Left)), Minus, false, true, e) => w!(out, "{:<-0wi$.pr$e}", v.lower_exp),
                (Some((None, Left)), Minus, false, true, E) => w!(out, "{:<-0wi$.pr$E}", v.upper_exp),
                (Some((None, Left)), Minus, false, true, p) => w!(out, "{:<-0wi$.pr$p}", v.pointer),
                (Some((None, Center)), No, false, false, Display) => w!(out, "{:^wi$.pr$}", v.display),
                (Some((None, Center)), No, false, false, Question) => w!(out, "{:^wi$.pr$?}", v.debug),
                (Some((None, Center)), No, false, false, xQuestion) => w!(out, "{:^wi$.pr$x?}", v.debug),
                (Some((None, Center)), No, false, false, XQuestion) => w!(out, "{:^wi$.pr$X?}", v.debug),
                (Some((None, Center)), No, false, false, x) => w!(out, "{:^wi$.pr$x}", v.lower_hex),
                (Some((None, Center)), No, false, false, X) => w!(out, "{:^wi$.pr$X}", v.upper_hex),
                (Some((None, Center)), No, false, false, b) => w!(out, "{:^wi$.pr$b}", v.binary),
                (Some((None, Center)), No, false, false, o) => w!(out, "{:^wi$.pr$o}", v.octal),
                (Some((None, Center)), No, false, false, e) => w!(out, "{:^wi$.pr$e}", v.lower_exp),
                (Some((None, Center)), No, false, false, E) => w!(out, "{:^wi$.pr$E}", v.upper_exp),
                (Some((None, Center)), No, false, false, p) => w!(out, "{:^wi$.pr$p}", v.pointer),
                (Some((None, Center)), No, false, true, Display) => w!(out, "{:^0wi$.pr$}", v.display),
                (Some((None, Center)), No, false, true, Question) => w!(out, "{:^0wi$.pr$?}", v.debug),
                (Some((None, Center)), No, false, true, xQuestion) => w!(out, "{:^0wi$.pr$x?}", v.debug),
                (Some((None, Center)), No, false, true, XQuestion) => w!(out, "{:^0wi$.pr$X?}", v.debug),
                (Some((None, Center)), No, false, true, x) => w!(out, "{:^0wi$.pr$x}", v.lower_hex),
                (Some((None, Center)), No, false, true, X) => w!(out, "{:^0wi$.pr$X}", v.upper_hex),
                (Some((None, Center)), No, false, true, b) => w!(out, "{:^0wi$.pr$b}", v.binary),
                (Some((None, Center)), No, false, true, o) => w!(out, "{:^0wi$.pr$o}", v.octal),
                (Some((None, Center)), No, false, true, e) => w!(out, "{:^0wi$.pr$e}", v.lower_exp),
                (Some((None, Center)), No, false, true, E) => w!(out, "{:^0wi$.pr$E}", v.upper_exp),
                (Some((None, Center)), No, false, true, p) => w!(out, "{:^0wi$.pr$p}", v.pointer),
                (Some((None, Center)), Plus, false, false, Display) => w!(out, "{:^+wi$.pr$}", v.display),
                (Some((None, Center)), Plus, false, false, Question) => w!(out, "{:^+wi$.pr$?}", v.debug),
                (Some((None, Center)), Plus, false, false, xQuestion) => w!(out, "{:^+wi$.pr$x?}", v.debug),
                (Some((None, Center)), Plus, false, false, XQuestion) => w!(out, "{:^+wi$.pr$X?}", v.debug),
                (Some((None, Center)), Plus, false, false, x) => w!(out, "{:^+wi$.pr$x}", v.lower_hex),
                (Some((None, Center)), Plus, false, false, X) => w!(out, "{:^+wi$.pr$X}", v.upper_hex),
                (Some((None, Center)), Plus, false, false, b) => w!(out, "{:^+wi$.pr$b}", v.binary),
                (Some((None, Center)), Plus, false, false, o) => w!(out, "{:^+wi$.pr$o}", v.octal),
                (Some((None, Center)), Plus, false, false, e) => w!(out, "{:^+wi$.pr$e}", v.lower_exp),
                (Some((None, Center)), Plus, false, false, E) => w!(out, "{:^+wi$.pr$E}", v.upper_exp),
                (Some((None, Center)), Plus, false, false, p) => w!(out, "{:^+wi$.pr$p}", v.pointer),
                (Some((None, Center)), Plus, false, true, Display) => w!(out, "{:^+0wi$.pr$}", v.display),
                (Some((None, Center)), Plus, false, true, Question) => w!(out, "{:^+0wi$.pr$?}", v.debug),
                (Some((None, Center)), Plus, false, true, xQuestion) => w!(out, "{:^+0wi$.pr$x?}", v.debug),
                (Some((None, Center)), Plus, false, true, XQuestion) => w!(out, "{:^+0wi$.pr$X?}", v.debug),
                (Some((None, Center)), Plus, false, true, x) => w!(out, "{:^+0wi$.pr$x}", v.lower_hex),
                (Some((None, Center)), Plus, false, true, X) => w!(out, "{:^+0wi$.pr$X}", v.upper_hex),
                (Some((None, Center)), Plus, false, true, b) => w!(out, "{:^+0wi$.pr$b}", v.binary),
                (Some((None, Center)), Plus, false, true, o) => w!(out, "{:^+0wi$.pr$o}", v.octal),
                (Some((None, Center)), Plus, false, true, e) => w!(out, "{:^+0wi$.pr$e}", v.lower_exp),
                (Some((None, Center)), Plus, false, true, E) => w!(out, "{:^+0wi$.pr$E}", v.upper_exp),
                (Some((None, Center)), Plus, false, true, p) => w!(out, "{:^+0wi$.pr$p}", v.pointer),
                (Some((None, Center)), Minus, false, false, Display) => w!(out, "{:^-wi$.pr$}", v.display),
                (Some((None, Center)), Minus, false, false, Question) => w!(out, "{:^-wi$.pr$?}", v.debug),
                (Some((None, Center)), Minus, false, false, xQuestion) => w!(out, "{:^-wi$.pr$x?}", v.debug),
                (Some((None, Center)), Minus, false, false, XQuestion) => w!(out, "{:^-wi$.pr$X?}", v.debug),
                (Some((None, Center)), Minus, false, false, x) => w!(out, "{:^-wi$.pr$x}", v.lower_hex),
                (Some((None, Center)), Minus, false, false, X) => w!(out, "{:^-wi$.pr$X}", v.upper_hex),
                (Some((None, Center)), Minus, false, false, b) => w!(out, "{:^-wi$.pr$b}", v.binary),
                (Some((None, Center)), Minus, false, false, o) => w!(out, "{:^-wi$.pr$o}", v.octal),
                (Some((None, Center)), Minus, false, false, e) => w!(out, "{:^-wi$.pr$e}", v.lower_exp),
                (Some((None, Center)), Minus, false, false, E) => w!(out, "{:^-wi$.pr$E}", v.upper_exp),
                (Some((None, Center)), Minus, false, false, p) => w!(out, "{:^-wi$.pr$p}", v.pointer),
                (Some((None, Center)), Minus, false, true, Display) => w!(out, "{:^-0wi$.pr$}", v.display),
                (Some((None, Center)), Minus, false, true, Question) => w!(out, "{:^-0wi$.pr$?}", v.debug),
                (Some((None, Center)), Minus, false, true, xQuestion) => w!(out, "{:^-0wi$.pr$x?}", v.debug),
                (Some((None, Center)), Minus, false, true, XQuestion) => w!(out, "{:^-0wi$.pr$X?}", v.debug),
                (Some((None, Center)), Minus, false, true, x) => w!(out, "{:^-0wi$.pr$x}", v.lower_hex),
                (Some((None, Center)), Minus, false, true, X) => w!(out, "{:^-0wi$.pr$X}", v.upper_hex),
                (Some((None, Center)), Minus, false, true, b) => w!(out, "{:^-0wi$.pr$b}", v.binary),
                (Some((None, Center)), Minus, false, true, o) => w!(out, "{:^-0wi$.pr$o}", v.octal),
                (Some((None, Center)), Minus, false, true, e) => w!(out, "{:^-0wi$.pr$e}", v.lower_exp),
                (Some((None, Center)), Minus, false, true, E) => w!(out, "{:^-0wi$.pr$E}", v.upper_exp),
                (Some((None, Center)), Minus, false, true, p) => w!(out, "{:^-0wi$.pr$p}", v.pointer),
                (Some((None, Right)), No, false, false, Display) => w!(out, "{:>wi$.pr$}", v.display),
                (Some((None, Right)), No, false, false, Question) => w!(out, "{:>wi$.pr$?}", v.debug),
                (Some((None, Right)), No, false, false, xQuestion) => w!(out, "{:>wi$.pr$x?}", v.debug),
                (Some((None, Right)), No, false, false, XQuestion) => w!(out, "{:>wi$.pr$X?}", v.debug),
                (Some((None, Right)), No, false, false, x) => w!(out, "{:>wi$.pr$x}", v.lower_hex),
                (Some((None, Right)), No, false, false, X) => w!(out, "{:>wi$.pr$X}", v.upper_hex),
                (Some((None, Right)), No, false, false, b) => w!(out, "{:>wi$.pr$b}", v.binary),
                (Some((None, Right)), No, false, false, o) => w!(out, "{:>wi$.pr$o}", v.octal),
                (Some((None, Right)), No, false, false, e) => w!(out, "{:>wi$.pr$e}", v.lower_exp),
                (Some((None, Right)), No, false, false, E) => w!(out, "{:>wi$.pr$E}", v.upper_exp),
                (Some((None, Right)), No, false, false, p) => w!(out, "{:>wi$.pr$p}", v.pointer),
                (Some((None, Right)), No, false, true, Display) => w!(out, "{:>0wi$.pr$}", v.display),
                (Some((None, Right)), No, false, true, Question) => w!(out, "{:>0wi$.pr$?}", v.debug),
                (Some((None, Right)), No, false, true, xQuestion) => w!(out, "{:>0wi$.pr$x?}", v.debug),
                (Some((None, Right)), No, false, true, XQuestion) => w!(out, "{:>0wi$.pr$X?}", v.debug),
                (Some((None, Right)), No, false, true, x) => w!(out, "{:>0wi$.pr$x}", v.lower_hex),
                (Some((None, Right)), No, false, true, X) => w!(out, "{:>0wi$.pr$X}", v.upper_hex),
                (Some((None, Right)), No, false, true, b) => w!(out, "{:>0wi$.pr$b}", v.binary),
                (Some((None, Right)), No, false, true, o) => w!(out, "{:>0wi$.pr$o}", v.octal),
                (Some((None, Right)), No, false, true, e) => w!(out, "{:>0wi$.pr$e}", v.lower_exp),
                (Some((None, Right)), No, false, true, E) => w!(out, "{:>0wi$.pr$E}", v.upper_exp),
                (Some((None, Right)), No, false, true, p) => w!(out, "{:>0wi$.pr$p}", v.pointer),
                (Some((None, Right)), Plus, false, false, Display) => w!(out, "{:>+wi$.pr$}", v.display),
                (Some((None, Right)), Plus, false, false, Question) => w!(out, "{:>+wi$.pr$?}", v.debug),
                (Some((None, Right)), Plus, false, false, xQuestion) => w!(out, "{:>+wi$.pr$x?}", v.debug),
                (Some((None, Right)), Plus, false, false, XQuestion) => w!(out, "{:>+wi$.pr$X?}", v.debug),
                (Some((None, Right)), Plus, false, false, x) => w!(out, "{:>+wi$.pr$x}", v.lower_hex),
                (Some((None, Right)), Plus, false, false, X) => w!(out, "{:>+wi$.pr$X}", v.upper_hex),
                (Some((None, Right)), Plus, false, false, b) => w!(out, "{:>+wi$.pr$b}", v.binary),
                (Some((None, Right)), Plus, false, false, o) => w!(out, "{:>+wi$.pr$o}", v.octal),
                (Some((None, Right)), Plus, false, false, e) => w!(out, "{:>+wi$.pr$e}", v.lower_exp),
                (Some((None, Right)), Plus, false, false, E) => w!(out, "{:>+wi$.pr$E}", v.upper_exp),
                (Some((None, Right)), Plus, false, false, p) => w!(out, "{:>+wi$.pr$p}", v.pointer),
                (Some((None, Right)), Plus, false, true, Display) => w!(out, "{:>+0wi$.pr$}", v.display),
                (Some((None, Right)), Plus, false, true, Question) => w!(out, "{:>+0wi$.pr$?}", v.debug),
                (Some((None, Right)), Plus, false, true, xQuestion) => w!(out, "{:>+0wi$.pr$x?}", v.debug),
                (Some((None, Right)), Plus, false, true, XQuestion) => w!(out, "{:>+0wi$.pr$X?}", v.debug),
                (Some((None, Right)), Plus, false, true, x) => w!(out, "{:>+0wi$.pr$x}", v.lower_hex),
                (Some((None, Right)), Plus, false, true, X) => w!(out, "{:>+0wi$.pr$X}", v.upper_hex),
                (Some((None, Right)), Plus, false, true, b) => w!(out, "{:>+0wi$.pr$b}", v.binary),
                (Some((None, Right)), Plus, false, true, o) => w!(out, "{:>+0wi$.pr$o}", v.octal),
                (Some((None, Right)), Plus, false, true, e) => w!(out, "{:>+0wi$.pr$e}", v.lower_exp),
                (Some((None, Right)), Plus, false, true, E) => w!(out, "{:>+0wi$.pr$E}", v.upper_exp),
                (Some((None, Right)), Plus, false, true, p) => w!(out, "{:>+0wi$.pr$p}", v.pointer),
                (Some((None, Right)), Minus, false, false, Display) => w!(out, "{:>-wi$.pr$}", v.display),
                (Some((None, Right)), Minus, false, false, Question) => w!(out, "{:>-wi$.pr$?}", v.debug),
                (Some((None, Right)), Minus, false, false, xQuestion) => w!(out, "{:>-wi$.pr$x?}", v.debug),
                (Some((None, Right)), Minus, false, false, XQuestion) => w!(out, "{:>-wi$.pr$X?}", v.debug),
                (Some((None, Right)), Minus, false, false, x) => w!(out, "{:>-wi$.pr$x}", v.lower_hex),
                (Some((None, Right)), Minus, false, false, X) => w!(out, "{:>-wi$.pr$X}", v.upper_hex),
                (Some((None, Right)), Minus, false, false, b) => w!(out, "{:>-wi$.pr$b}", v.binary),
                (Some((None, Right)), Minus, false, false, o) => w!(out, "{:>-wi$.pr$o}", v.octal),
                (Some((None, Right)), Minus, false, false, e) => w!(out, "{:>-wi$.pr$e}", v.lower_exp),
                (Some((None, Right)), Minus, false, false, E) => w!(out, "{:>-wi$.pr$E}", v.upper_exp),
                (Some((None, Right)), Minus, false, false, p) => w!(out, "{:>-wi$.pr$p}", v.pointer),
                (Some((None, Right)), Minus, false, true, Display) => w!(out, "{:>-0wi$.pr$}", v.display),
                (Some((None, Right)), Minus, false, true, Question) => w!(out, "{:>-0wi$.pr$?}", v.debug),
                (Some((None, Right)), Minus, false, true, xQuestion) => w!(out, "{:>-0wi$.pr$x?}", v.debug),
                (Some((None, Right)), Minus, false, true, XQuestion) => w!(out, "{:>-0wi$.pr$X?}", v.debug),
                (Some((None, Right)), Minus, false, true, x) => w!(out, "{:>-0wi$.pr$x}", v.lower_hex),
                (Some((None, Right)), Minus, false, true, X) => w!(out, "{:>-0wi$.pr$X}", v.upper_hex),
                (Some((None, Right)), Minus, false, true, b) => w!(out, "{:>-0wi$.pr$b}", v.binary),
                (Some((None, Right)), Minus, false, true, o) => w!(out, "{:>-0wi$.pr$o}", v.octal),
                (Some((None, Right)), Minus, false, true, e) => w!(out, "{:>-0wi$.pr$e}", v.lower_exp),
                (Some((None, Right)), Minus, false, true, E) => w!(out, "{:>-0wi$.pr$E}", v.upper_exp),
                (Some((None, Right)), Minus, false, true, p) => w!(out, "{:>-0wi$.pr$p}", v.pointer),
                (None, No, true, false, Display) => w!(out, "{:#wi$.pr$}", v.display),
                (None, No, true, false, Question) => w!(out, "{:#wi$.pr$?}", v.debug),
                (None, No, true, false, xQuestion) => w!(out, "{:#wi$.pr$x?}", v.debug),
                (None, No, true, false, XQuestion) => w!(out, "{:#wi$.pr$X?}", v.debug),
                (None, No, true, false, x) => w!(out, "{:#wi$.pr$x}", v.lower_hex),
                (None, No, true, false, X) => w!(out, "{:#wi$.pr$X}", v.upper_hex),
                (None, No, true, false, b) => w!(out, "{:#wi$.pr$b}", v.binary),
                (None, No, true, false, o) => w!(out, "{:#wi$.pr$o}", v.octal),
                (None, No, true, false, e) => w!(out, "{:#wi$.pr$e}", v.lower_exp),
                (None, No, true, false, E) => w!(out, "{:#wi$.pr$E}", v.upper_exp),
                (None, No, true, false, p) => w!(out, "{:#wi$.pr$p}", v.pointer),
                (None, No, true, true, Display) => w!(out, "{:#0wi$.pr$}", v.display),
                (None, No, true, true, Question) => w!(out, "{:#0wi$.pr$?}", v.debug),
                (None, No, true, true, xQuestion) => w!(out, "{:#0wi$.pr$x?}", v.debug),
                (None, No, true, true, XQuestion) => w!(out, "{:#0wi$.pr$X?}", v.debug),
                (None, No, true, true, x) => w!(out, "{:#0wi$.pr$x}", v.lower_hex),
                (None, No, true, true, X) => w!(out, "{:#0wi$.pr$X}", v.upper_hex),
                (None, No, true, true, b) => w!(out, "{:#0wi$.pr$b}", v.binary),
                (None, No, true, true, o) => w!(out, "{:#0wi$.pr$o}", v.octal),
                (None, No, true, true, e) => w!(out, "{:#0wi$.pr$e}", v.lower_exp),
                (None, No, true, true, E) => w!(out, "{:#0wi$.pr$E}", v.upper_exp),
                (None, No, true, true, p) => w!(out, "{:#0wi$.pr$p}", v.pointer),
                (None, Plus, true, false, Display) => w!(out, "{:+#wi$.pr$}", v.display),
                (None, Plus, true, false, Question) => w!(out, "{:+#wi$.pr$?}", v.debug),
                (None, Plus, true, false, xQuestion) => w!(out, "{:+#wi$.pr$x?}", v.debug),
                (None, Plus, true, false, XQuestion) => w!(out, "{:+#wi$.pr$X?}", v.debug),
                (None, Plus, true, false, x) => w!(out, "{:+#wi$.pr$x}", v.lower_hex),
                (None, Plus, true, false, X) => w!(out, "{:+#wi$.pr$X}", v.upper_hex),
                (None, Plus, true, false, b) => w!(out, "{:+#wi$.pr$b}", v.binary),
                (None, Plus, true, false, o) => w!(out, "{:+#wi$.pr$o}", v.octal),
                (None, Plus, true, false, e) => w!(out, "{:+#wi$.pr$e}", v.lower_exp),
                (None, Plus, true, false, E) => w!(out, "{:+#wi$.pr$E}", v.upper_exp),
                (None, Plus, true, false, p) => w!(out, "{:+#wi$.pr$p}", v.pointer),
                (None, Plus, true, true, Display) => w!(out, "{:+#0wi$.pr$}", v.display),
                (None, Plus, true, true, Question) => w!(out, "{:+#0wi$.pr$?}", v.debug),
                (None, Plus, true, true, xQuestion) => w!(out, "{:+#0wi$.pr$x?}", v.debug),
                (None, Plus, true, true, XQuestion) => w!(out, "{:+#0wi$.pr$X?}", v.debug),
                (None, Plus, true, true, x) => w!(out, "{:+#0wi$.pr$x}", v.lower_hex),
                (None, Plus, true, true, X) => w!(out, "{:+#0wi$.pr$X}", v.upper_hex),
                (None, Plus, true, true, b) => w!(out, "{:+#0wi$.pr$b}", v.binary),
                (None, Plus, true, true, o) => w!(out, "{:+#0wi$.pr$o}", v.octal),
                (None, Plus, true, true, e) => w!(out, "{:+#0wi$.pr$e}", v.lower_exp),
                (None, Plus, true, true, E) => w!(out, "{:+#0wi$.pr$E}", v.upper_exp),
                (None, Plus, true, true, p) => w!(out, "{:+#0wi$.pr$p}", v.pointer),
                (None, Minus, true, false, Display) => w!(out, "{:-#wi$.pr$}", v.display),
                (None, Minus, true, false, Question) => w!(out, "{:-#wi$.pr$?}", v.debug),
                (None, Minus, true, false, xQuestion) => w!(out, "{:-#wi$.pr$x?}", v.debug),
                (None, Minus, true, false, XQuestion) => w!(out, "{:-#wi$.pr$X?}", v.debug),
                (None, Minus, true, false, x) => w!(out, "{:-#wi$.pr$x}", v.lower_hex),
                (None, Minus, true, false, X) => w!(out, "{:-#wi$.pr$X}", v.upper_hex),
                (None, Minus, true, false, b) => w!(out, "{:-#wi$.pr$b}", v.binary),
                (None, Minus, true, false, o) => w!(out, "{:-#wi$.pr$o}", v.octal),
                (None, Minus, true, false, e) => w!(out, "{:-#wi$.pr$e}", v.lower_exp),
                (None, Minus, true, false, E) => w!(out, "{:-#wi$.pr$E}", v.upper_exp),
                (None, Minus, true, false, p) => w!(out, "{:-#wi$.pr$p}", v.pointer),
                (None, Minus, true, true, Display) => w!(out, "{:-#0wi$.pr$}", v.display),
                (None, Minus, true, true, Question) => w!(out, "{:-#0wi$.pr$?}", v.debug),
                (None, Minus, true, true, xQuestion) => w!(out, "{:-#0wi$.pr$x?}", v.debug),
                (None, Minus, true, true, XQuestion) => w!(out, "{:-#0wi$.pr$X?}", v.debug),
                (None, Minus, true, true, x) => w!(out, "{:-#0wi$.pr$x}", v.lower_hex),
                (None, Minus, true, true, X) => w!(out, "{:-#0wi$.pr$X}", v.upper_hex),
                (None, Minus, true, true, b) => w!(out, "{:-#0wi$.pr$b}", v.binary),
                (None, Minus, true, true, o) => w!(out, "{:-#0wi$.pr$o}", v.octal),
                (None, Minus, true, true, e) => w!(out, "{:-#0wi$.pr$e}", v.lower_exp),
                (None, Minus, true, true, E) => w!(out, "{:-#0wi$.pr$E}", v.upper_exp),
                (None, Minus, true, true, p) => w!(out, "{:-#0wi$.pr$p}", v.pointer),
                (Some((None, Left)), No, true, false, Display) => w!(out, "{:<#wi$.pr$}", v.display),
                (Some((None, Left)), No, true, false, Question) => w!(out, "{:<#wi$.pr$?}", v.debug),
                (Some((None, Left)), No, true, false, xQuestion) => w!(out, "{:<#wi$.pr$x?}", v.debug),
                (Some((None, Left)), No, true, false, XQuestion) => w!(out, "{:<#wi$.pr$X?}", v.debug),
                (Some((None, Left)), No, true, false, x) => w!(out, "{:<#wi$.pr$x}", v.lower_hex),
                (Some((None, Left)), No, true, false, X) => w!(out, "{:<#wi$.pr$X}", v.upper_hex),
                (Some((None, Left)), No, true, false, b) => w!(out, "{:<#wi$.pr$b}", v.binary),
                (Some((None, Left)), No, true, false, o) => w!(out, "{:<#wi$.pr$o}", v.octal),
                (Some((None, Left)), No, true, false, e) => w!(out, "{:<#wi$.pr$e}", v.lower_exp),
                (Some((None, Left)), No, true, false, E) => w!(out, "{:<#wi$.pr$E}", v.upper_exp),
                (Some((None, Left)), No, true, false, p) => w!(out, "{:<#wi$.pr$p}", v.pointer),
                (Some((None, Left)), No, true, true, Display) => w!(out, "{:<#0wi$.pr$}", v.display),
                (Some((None, Left)), No, true, true, Question) => w!(out, "{:<#0wi$.pr$?}", v.debug),
                (Some((None, Left)), No, true, true, xQuestion) => w!(out, "{:<#0wi$.pr$x?}", v.debug),
                (Some((None, Left)), No, true, true, XQuestion) => w!(out, "{:<#0wi$.pr$X?}", v.debug),
                (Some((None, Left)), No, true, true, x) => w!(out, "{:<#0wi$.pr$x}", v.lower_hex),
                (Some((None, Left)), No, true, true, X) => w!(out, "{:<#0wi$.pr$X}", v.upper_hex),
                (Some((None, Left)), No, true, true, b) => w!(out, "{:<#0wi$.pr$b}", v.binary),
                (Some((None, Left)), No, true, true, o) => w!(out, "{:<#0wi$.pr$o}", v.octal),
                (Some((None, Left)), No, true, true, e) => w!(out, "{:<#0wi$.pr$e}", v.lower_exp),
                (Some((None, Left)), No, true, true, E) => w!(out, "{:<#0wi$.pr$E}", v.upper_exp),
                (Some((None, Left)), No, true, true, p) => w!(out, "{:<#0wi$.pr$p}", v.pointer),
                (Some((None, Left)), Plus, true, false, Display) => w!(out, "{:<+#wi$.pr$}", v.display),
                (Some((None, Left)), Plus, true, false, Question) => w!(out, "{:<+#wi$.pr$?}", v.debug),
                (Some((None, Left)), Plus, true, false, xQuestion) => w!(out, "{:<+#wi$.pr$x?}", v.debug),
                (Some((None, Left)), Plus, true, false, XQuestion) => w!(out, "{:<+#wi$.pr$X?}", v.debug),
                (Some((None, Left)), Plus, true, false, x) => w!(out, "{:<+#wi$.pr$x}", v.lower_hex),
                (Some((None, Left)), Plus, true, false, X) => w!(out, "{:<+#wi$.pr$X}", v.upper_hex),
                (Some((None, Left)), Plus, true, false, b) => w!(out, "{:<+#wi$.pr$b}", v.binary),
                (Some((None, Left)), Plus, true, false, o) => w!(out, "{:<+#wi$.pr$o}", v.octal),
                (Some((None, Left)), Plus, true, false, e) => w!(out, "{:<+#wi$.pr$e}", v.lower_exp),
                (Some((None, Left)), Plus, true, false, E) => w!(out, "{:<+#wi$.pr$E}", v.upper_exp),
                (Some((None, Left)), Plus, true, false, p) => w!(out, "{:<+#wi$.pr$p}", v.pointer),
                (Some((None, Left)), Plus, true, true, Display) => w!(out, "{:<+#0wi$.pr$}", v.display),
                (Some((None, Left)), Plus, true, true, Question) => w!(out, "{:<+#0wi$.pr$?}", v.debug),
                (Some((None, Left)), Plus, true, true, xQuestion) => w!(out, "{:<+#0wi$.pr$x?}", v.debug),
                (Some((None, Left)), Plus, true, true, XQuestion) => w!(out, "{:<+#0wi$.pr$X?}", v.debug),
                (Some((None, Left)), Plus, true, true, x) => w!(out, "{:<+#0wi$.pr$x}", v.lower_hex),
                (Some((None, Left)), Plus, true, true, X) => w!(out, "{:<+#0wi$.pr$X}", v.upper_hex),
                (Some((None, Left)), Plus, true, true, b) => w!(out, "{:<+#0wi$.pr$b}", v.binary),
                (Some((None, Left)), Plus, true, true, o) => w!(out, "{:<+#0wi$.pr$o}", v.octal),
                (Some((None, Left)), Plus, true, true, e) => w!(out, "{:<+#0wi$.pr$e}", v.lower_exp),
                (Some((None, Left)), Plus, true, true, E) => w!(out, "{:<+#0wi$.pr$E}", v.upper_exp),
                (Some((None, Left)), Plus, true, true, p) => w!(out, "{:<+#0wi$.pr$p}", v.pointer),
                (Some((None, Left)), Minus, true, false, Display) => w!(out, "{:<-#wi$.pr$}", v.display),
                (Some((None, Left)), Minus, true, false, Question) => w!(out, "{:<-#wi$.pr$?}", v.debug),
                (Some((None, Left)), Minus, true, false, xQuestion) => w!(out, "{:<-#wi$.pr$x?}", v.debug),
                (Some((None, Left)), Minus, true, false, XQuestion) => w!(out, "{:<-#wi$.pr$X?}", v.debug),
                (Some((None, Left)), Minus, true, false, x) => w!(out, "{:<-#wi$.pr$x}", v.lower_hex),
                (Some((None, Left)), Minus, true, false, X) => w!(out, "{:<-#wi$.pr$X}", v.upper_hex),
                (Some((None, Left)), Minus, true, false, b) => w!(out, "{:<-#wi$.pr$b}", v.binary),
                (Some((None, Left)), Minus, true, false, o) => w!(out, "{:<-#wi$.pr$o}", v.octal),
                (Some((None, Left)), Minus, true, false, e) => w!(out, "{:<-#wi$.pr$e}", v.lower_exp),
                (Some((None, Left)), Minus, true, false, E) => w!(out, "{:<-#wi$.pr$E}", v.upper_exp),
                (Some((None, Left)), Minus, true, false, p) => w!(out, "{:<-#wi$.pr$p}", v.pointer),
                (Some((None, Left)), Minus, true, true, Display) => w!(out, "{:<-#0wi$.pr$}", v.display),
                (Some((None, Left)), Minus, true, true, Question) => w!(out, "{:<-#0wi$.pr$?}", v.debug),
                (Some((None, Left)), Minus, true, true, xQuestion) => w!(out, "{:<-#0wi$.pr$x?}", v.debug),
                (Some((None, Left)), Minus, true, true, XQuestion) => w!(out, "{:<-#0wi$.pr$X?}", v.debug),
                (Some((None, Left)), Minus, true, true, x) => w!(out, "{:<-#0wi$.pr$x}", v.lower_hex),
                (Some((None, Left)), Minus, true, true, X) => w!(out, "{:<-#0wi$.pr$X}", v.upper_hex),
                (Some((None, Left)), Minus, true, true, b) => w!(out, "{:<-#0wi$.pr$b}", v.binary),
                (Some((None, Left)), Minus, true, true, o) => w!(out, "{:<-#0wi$.pr$o}", v.octal),
                (Some((None, Left)), Minus, true, true, e) => w!(out, "{:<-#0wi$.pr$e}", v.lower_exp),
                (Some((None, Left)), Minus, true, true, E) => w!(out, "{:<-#0wi$.pr$E}", v.upper_exp),
                (Some((None, Left)), Minus, true, true, p) => w!(out, "{:<-#0wi$.pr$p}", v.pointer),
                (Some((None, Center)), No, true, false, Display) => w!(out, "{:^#wi$.pr$}", v.display),
                (Some((None, Center)), No, true, false, Question) => w!(out, "{:^#wi$.pr$?}", v.debug),
                (Some((None, Center)), No, true, false, xQuestion) => w!(out, "{:^#wi$.pr$x?}", v.debug),
                (Some((None, Center)), No, true, false, XQuestion) => w!(out, "{:^#wi$.pr$X?}", v.debug),
                (Some((None, Center)), No, true, false, x) => w!(out, "{:^#wi$.pr$x}", v.lower_hex),
                (Some((None, Center)), No, true, false, X) => w!(out, "{:^#wi$.pr$X}", v.upper_hex),
                (Some((None, Center)), No, true, false, b) => w!(out, "{:^#wi$.pr$b}", v.binary),
                (Some((None, Center)), No, true, false, o) => w!(out, "{:^#wi$.pr$o}", v.octal),
                (Some((None, Center)), No, true, false, e) => w!(out, "{:^#wi$.pr$e}", v.lower_exp),
                (Some((None, Center)), No, true, false, E) => w!(out, "{:^#wi$.pr$E}", v.upper_exp),
                (Some((None, Center)), No, true, false, p) => w!(out, "{:^#wi$.pr$p}", v.pointer),
                (Some((None, Center)), No, true, true, Display) => w!(out, "{:^#0wi$.pr$}", v.display),
                (Some((None, Center)), No, true, true, Question) => w!(out, "{:^#0wi$.pr$?}", v.debug),
                (Some((None, Center)), No, true, true, xQuestion) => w!(out, "{:^#0wi$.pr$x?}", v.debug),
                (Some((None, Center)), No, true, true, XQuestion) => w!(out, "{:^#0wi$.pr$X?}", v.debug),
                (Some((None, Center)), No, true, true, x) => w!(out, "{:^#0wi$.pr$x}", v.lower_hex),
                (Some((None, Center)), No, true, true, X) => w!(out, "{:^#0wi$.pr$X}", v.upper_hex),
                (Some((None, Center)), No, true, true, b) => w!(out, "{:^#0wi$.pr$b}", v.binary),
                (Some((None, Center)), No, true, true, o) => w!(out, "{:^#0wi$.pr$o}", v.octal),
                (Some((None, Center)), No, true, true, e) => w!(out, "{:^#0wi$.pr$e}", v.lower_exp),
                (Some((None, Center)), No, true, true, E) => w!(out, "{:^#0wi$.pr$E}", v.upper_exp),
                (Some((None, Center)), No, true, true, p) => w!(out, "{:^#0wi$.pr$p}", v.pointer),
                (Some((None, Center)), Plus, true, false, Display) => w!(out, "{:^+#wi$.pr$}", v.display),
                (Some((None, Center)), Plus, true, false, Question) => w!(out, "{:^+#wi$.pr$?}", v.debug),
                (Some((None, Center)), Plus, true, false, xQuestion) => w!(out, "{:^+#wi$.pr$x?}", v.debug),
                (Some((None, Center)), Plus, true, false, XQuestion) => w!(out, "{:^+#wi$.pr$X?}", v.debug),
                (Some((None, Center)), Plus, true, false, x) => w!(out, "{:^+#wi$.pr$x}", v.lower_hex),
                (Some((None, Center)), Plus, true, false, X) => w!(out, "{:^+#wi$.pr$X}", v.upper_hex),
                (Some((None, Center)), Plus, true, false, b) => w!(out, "{:^+#wi$.pr$b}", v.binary),
                (Some((None, Center)), Plus, true, false, o) => w!(out, "{:^+#wi$.pr$o}", v.octal),
                (Some((None, Center)), Plus, true, false, e) => w!(out, "{:^+#wi$.pr$e}", v.lower_exp),
                (Some((None, Center)), Plus, true, false, E) => w!(out, "{:^+#wi$.pr$E}", v.upper_exp),
                (Some((None, Center)), Plus, true, false, p) => w!(out, "{:^+#wi$.pr$p}", v.pointer),
                (Some((None, Center)), Plus, true, true, Display) => w!(out, "{:^+#0wi$.pr$}", v.display),
                (Some((None, Center)), Plus, true, true, Question) => w!(out, "{:^+#0wi$.pr$?}", v.debug),
                (Some((None, Center)), Plus, true, true, xQuestion) => w!(out, "{:^+#0wi$.pr$x?}", v.debug),
                (Some((None, Center)), Plus, true, true, XQuestion) => w!(out, "{:^+#0wi$.pr$X?}", v.debug),
                (Some((None, Center)), Plus, true, true, x) => w!(out, "{:^+#0wi$.pr$x}", v.lower_hex),
                (Some((None, Center)), Plus, true, true, X) => w!(out, "{:^+#0wi$.pr$X}", v.upper_hex),
                (Some((None, Center)), Plus, true, true, b) => w!(out, "{:^+#0wi$.pr$b}", v.binary),
                (Some((None, Center)), Plus, true, true, o) => w!(out, "{:^+#0wi$.pr$o}", v.octal),
                (Some((None, Center)), Plus, true, true, e) => w!(out, "{:^+#0wi$.pr$e}", v.lower_exp),
                (Some((None, Center)), Plus, true, true, E) => w!(out, "{:^+#0wi$.pr$E}", v.upper_exp),
                (Some((None, Center)), Plus, true, true, p) => w!(out, "{:^+#0wi$.pr$p}", v.pointer),
                (Some((None, Center)), Minus, true, false, Display) => w!(out, "{:^-#wi$.pr$}", v.display),
                (Some((None, Center)), Minus, true, false, Question) => w!(out, "{:^-#wi$.pr$?}", v.debug),
                (Some((None, Center)), Minus, true, false, xQuestion) => w!(out, "{:^-#wi$.pr$x?}", v.debug),
                (Some((None, Center)), Minus, true, false, XQuestion) => w!(out, "{:^-#wi$.pr$X?}", v.debug),
                (Some((None, Center)), Minus, true, false, x) => w!(out, "{:^-#wi$.pr$x}", v.lower_hex),
                (Some((None, Center)), Minus, true, false, X) => w!(out, "{:^-#wi$.pr$X}", v.upper_hex),
                (Some((None, Center)), Minus, true, false, b) => w!(out, "{:^-#wi$.pr$b}", v.binary),
                (Some((None, Center)), Minus, true, false, o) => w!(out, "{:^-#wi$.pr$o}", v.octal),
                (Some((None, Center)), Minus, true, false, e) => w!(out, "{:^-#wi$.pr$e}", v.lower_exp),
                (Some((None, Center)), Minus, true, false, E) => w!(out, "{:^-#wi$.pr$E}", v.upper_exp),
                (Some((None, Center)), Minus, true, false, p) => w!(out, "{:^-#wi$.pr$p}", v.pointer),
                (Some((None, Center)), Minus, true, true, Display) => w!(out, "{:^-#0wi$.pr$}", v.display),
                (Some((None, Center)), Minus, true, true, Question) => w!(out, "{:^-#0wi$.pr$?}", v.debug),
                (Some((None, Center)), Minus, true, true, xQuestion) => w!(out, "{:^-#0wi$.pr$x?}", v.debug),
                (Some((None, Center)), Minus, true, true, XQuestion) => w!(out, "{:^-#0wi$.pr$X?}", v.debug),
                (Some((None, Center)), Minus, true, true, x) => w!(out, "{:^-#0wi$.pr$x}", v.lower_hex),
                (Some((None, Center)), Minus, true, true, X) => w!(out, "{:^-#0wi$.pr$X}", v.upper_hex),
                (Some((None, Center)), Minus, true, true, b) => w!(out, "{:^-#0wi$.pr$b}", v.binary),
                (Some((None, Center)), Minus, true, true, o) => w!(out, "{:^-#0wi$.pr$o}", v.octal),
                (Some((None, Center)), Minus, true, true, e) => w!(out, "{:^-#0wi$.pr$e}", v.lower_exp),
                (Some((None, Center)), Minus, true, true, E) => w!(out, "{:^-#0wi$.pr$E}", v.upper_exp),
                (Some((None, Center)), Minus, true, true, p) => w!(out, "{:^-#0wi$.pr$p}", v.pointer),
                (Some((None, Right)), No, true, false, Display) => w!(out, "{:>#wi$.pr$}", v.display),
                (Some((None, Right)), No, true, false, Question) => w!(out, "{:>#wi$.pr$?}", v.debug),
                (Some((None, Right)), No, true, false, xQuestion) => w!(out, "{:>#wi$.pr$x?}", v.debug),
                (Some((None, Right)), No, true, false, XQuestion) => w!(out, "{:>#wi$.pr$X?}", v.debug),
                (Some((None, Right)), No, true, false, x) => w!(out, "{:>#wi$.pr$x}", v.lower_hex),
                (Some((None, Right)), No, true, false, X) => w!(out, "{:>#wi$.pr$X}", v.upper_hex),
                (Some((None, Right)), No, true, false, b) => w!(out, "{:>#wi$.pr$b}", v.binary),
                (Some((None, Right)), No, true, false, o) => w!(out, "{:>#wi$.pr$o}", v.octal),
                (Some((None, Right)), No, true, false, e) => w!(out, "{:>#wi$.pr$e}", v.lower_exp),
                (Some((None, Right)), No, true, false, E) => w!(out, "{:>#wi$.pr$E}", v.upper_exp),
                (Some((None, Right)), No, true, false, p) => w!(out, "{:>#wi$.pr$p}", v.pointer),
                (Some((None, Right)), No, true, true, Display) => w!(out, "{:>#0wi$.pr$}", v.display),
                (Some((None, Right)), No, true, true, Question) => w!(out, "{:>#0wi$.pr$?}", v.debug),
                (Some((None, Right)), No, true, true, xQuestion) => w!(out, "{:>#0wi$.pr$x?}", v.debug),
                (Some((None, Right)), No, true, true, XQuestion) => w!(out, "{:>#0wi$.pr$X?}", v.debug),
                (Some((None, Right)), No, true, true, x) => w!(out, "{:>#0wi$.pr$x}", v.lower_hex),
                (Some((None, Right)), No, true, true, X) => w!(out, "{:>#0wi$.pr$X}", v.upper_hex),
                (Some((None, Right)), No, true, true, b) => w!(out, "{:>#0wi$.pr$b}", v.binary),
                (Some((None, Right)), No, true, true, o) => w!(out, "{:>#0wi$.pr$o}", v.octal),
                (Some((None, Right)), No, true, true, e) => w!(out, "{:>#0wi$.pr$e}", v.lower_exp),
                (Some((None, Right)), No, true, true, E) => w!(out, "{:>#0wi$.pr$E}", v.upper_exp),
                (Some((None, Right)), No, true, true, p) => w!(out, "{:>#0wi$.pr$p}", v.pointer),
                (Some((None, Right)), Plus, true, false, Display) => w!(out, "{:>+#wi$.pr$}", v.display),
                (Some((None, Right)), Plus, true, false, Question) => w!(out, "{:>+#wi$.pr$?}", v.debug),
                (Some((None, Right)), Plus, true, false, xQuestion) => w!(out, "{:>+#wi$.pr$x?}", v.debug),
                (Some((None, Right)), Plus, true, false, XQuestion) => w!(out, "{:>+#wi$.pr$X?}", v.debug),
                (Some((None, Right)), Plus, true, false, x) => w!(out, "{:>+#wi$.pr$x}", v.lower_hex),
                (Some((None, Right)), Plus, true, false, X) => w!(out, "{:>+#wi$.pr$X}", v.upper_hex),
                (Some((None, Right)), Plus, true, false, b) => w!(out, "{:>+#wi$.pr$b}", v.binary),
                (Some((None, Right)), Plus, true, false, o) => w!(out, "{:>+#wi$.pr$o}", v.octal),
                (Some((None, Right)), Plus, true, false, e) => w!(out, "{:>+#wi$.pr$e}", v.lower_exp),
                (Some((None, Right)), Plus, true, false, E) => w!(out, "{:>+#wi$.pr$E}", v.upper_exp),
                (Some((None, Right)), Plus, true, false, p) => w!(out, "{:>+#wi$.pr$p}", v.pointer),
                (Some((None, Right)), Plus, true, true, Display) => w!(out, "{:>+#0wi$.pr$}", v.display),
                (Some((None, Right)), Plus, true, true, Question) => w!(out, "{:>+#0wi$.pr$?}", v.debug),
                (Some((None, Right)), Plus, true, true, xQuestion) => w!(out, "{:>+#0wi$.pr$x?}", v.debug),
                (Some((None, Right)), Plus, true, true, XQuestion) => w!(out, "{:>+#0wi$.pr$X?}", v.debug),
                (Some((None, Right)), Plus, true, true, x) => w!(out, "{:>+#0wi$.pr$x}", v.lower_hex),
                (Some((None, Right)), Plus, true, true, X) => w!(out, "{:>+#0wi$.pr$X}", v.upper_hex),
                (Some((None, Right)), Plus, true, true, b) => w!(out, "{:>+#0wi$.pr$b}", v.binary),
                (Some((None, Right)), Plus, true, true, o) => w!(out, "{:>+#0wi$.pr$o}", v.octal),
                (Some((None, Right)), Plus, true, true, e) => w!(out, "{:>+#0wi$.pr$e}", v.lower_exp),
                (Some((None, Right)), Plus, true, true, E) => w!(out, "{:>+#0wi$.pr$E}", v.upper_exp),
                (Some((None, Right)), Plus, true, true, p) => w!(out, "{:>+#0wi$.pr$p}", v.pointer),
                (Some((None, Right)), Minus, true, false, Display) => w!(out, "{:>-#wi$.pr$}", v.display),
                (Some((None, Right)), Minus, true, false, Question) => w!(out, "{:>-#wi$.pr$?}", v.debug),
                (Some((None, Right)), Minus, true, false, xQuestion) => w!(out, "{:>-#wi$.pr$x?}", v.debug),
                (Some((None, Right)), Minus, true, false, XQuestion) => w!(out, "{:>-#wi$.pr$X?}", v.debug),
                (Some((None, Right)), Minus, true, false, x) => w!(out, "{:>-#wi$.pr$x}", v.lower_hex),
                (Some((None, Right)), Minus, true, false, X) => w!(out, "{:>-#wi$.pr$X}", v.upper_hex),
                (Some((None, Right)), Minus, true, false, b) => w!(out, "{:>-#wi$.pr$b}", v.binary),
                (Some((None, Right)), Minus, true, false, o) => w!(out, "{:>-#wi$.pr$o}", v.octal),
                (Some((None, Right)), Minus, true, false, e) => w!(out, "{:>-#wi$.pr$e}", v.lower_exp),
                (Some((None, Right)), Minus, true, false, E) => w!(out, "{:>-#wi$.pr$E}", v.upper_exp),
                (Some((None, Right)), Minus, true, false, p) => w!(out, "{:>-#wi$.pr$p}", v.pointer),
                (Some((None, Right)), Minus, true, true, Display) => w!(out, "{:>-#0wi$.pr$}", v.display),
                (Some((None, Right)), Minus, true, true, Question) => w!(out, "{:>-#0wi$.pr$?}", v.debug),
                (Some((None, Right)), Minus, true, true, xQuestion) => w!(out, "{:>-#0wi$.pr$x?}", v.debug),
                (Some((None, Right)), Minus, true, true, XQuestion) => w!(out, "{:>-#0wi$.pr$X?}", v.debug),
                (Some((None, Right)), Minus, true, true, x) => w!(out, "{:>-#0wi$.pr$x}", v.lower_hex),
                (Some((None, Right)), Minus, true, true, X) => w!(out, "{:>-#0wi$.pr$X}", v.upper_hex),
                (Some((None, Right)), Minus, true, true, b) => w!(out, "{:>-#0wi$.pr$b}", v.binary),
                (Some((None, Right)), Minus, true, true, o) => w!(out, "{:>-#0wi$.pr$o}", v.octal),
                (Some((None, Right)), Minus, true, true, e) => w!(out, "{:>-#0wi$.pr$e}", v.lower_exp),
                (Some((None, Right)), Minus, true, true, E) => w!(out, "{:>-#0wi$.pr$E}", v.upper_exp),
                (Some((None, Right)), Minus, true, true, p) => w!(out, "{:>-#0wi$.pr$p}", v.pointer),
                (Some((Some(_), _)), ..) => todo!(),
            };
            ensure!(format.starts_with('}'));
            step(1, format);
            continue;
        }
        let next = format
            .chars()
            .next()
            .expect("should contain a char if not empty");
        out.push(next);
        step(next.len_utf8(), format);
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
            "{hi:10} {hi:?} {int:#x?} {int:b} {int:#X} {int:4o} {display} {display:5} {display:05}",
            // "{int:#x?}",
            [
                ("hi", Formattable::debug_display(&"hello")),
                ("int", Formattable::number(&123u8)),
                ("display", (&10).into())
            ]
            .into_iter()
            .collect()
        )
        .unwrap(),
        r#"hello      "hello" 0x7b 1111011 0x7B  173 10    10 00010"#
    );
}
