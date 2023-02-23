use std::borrow::Borrow;
use std::collections::HashMap;
use std::fmt::{
    Binary, Debug, Display, LowerExp, LowerHex, Octal, Pointer, UpperExp, UpperHex, Write,
};
use std::hash::Hash;

mod hard_coded;
use hard_coded::format_value;

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
    pointer: Option<PointerWrapper<'a>>,
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
    integer, and_integer<Binary, Debug, Display, LowerExp, LowerHex, Octal, UpperExp, UpperHex>
        binary, debug, display, lower_exp, lower_hex, octal, upper_exp, upper_hex;
    float, and_float<Debug, Display, LowerExp, UpperExp>
        debug, display, lower_exp, upper_exp;
    binary, and_binary<Binary>;
    lower_exp, and_lower_exp<LowerExp>;
    lower_hex, and_lower_hex<LowerHex>;
    octal, and_octal<Octal>;
    // pointer, and_pointer<Pointer>;
    upper_exp, and_upper_exp<UpperExp>;
    upper_hex, and_upper_hex<UpperHex>;
];

impl<'a> Formattable<'a> {
    pub fn pointer<T: Pointer>(value: &'a T) -> Self {
        Self::default().and_pointer(value)
    }

    pub fn and_pointer<T: Pointer>(mut self, value: &'a T) -> Self {
        self.pointer = Some(PointerWrapper(value));
        self
    }
}

#[derive(Clone, Copy)]
struct PointerWrapper<'a>(&'a dyn Pointer);

impl Pointer for PointerWrapper<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Pointer::fmt(self.0, f)
    }
}

impl<'a, T: Display + Debug> From<&'a T> for Formattable<'a> {
    fn from(value: &'a T) -> Self {
        Self::debug_display(value)
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

#[derive(Debug, Default, PartialEq, Eq, Clone, Copy)]
enum Alignment {
    Left,
    Center,
    Right,
    #[default]
    None,
}

#[derive(Debug, Default)]
enum Sign {
    Plus,
    Minus,
    #[default]
    None,
}

#[derive(Debug, Default, Clone, Copy)]
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
    fill: Option<char>,
    alignment: Alignment,
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
                it.fill = Some(fill);
                it.alignment = alignment;
                step(1 + fill.len_utf8(), format)
            } else if fill.is_ascii() {
                if let Some(alignment) = alignment(fill as u8) {
                    it.alignment = alignment;
                    step(1, format);
                }
            }
            if format.is_empty() {
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
            if format.is_empty() {
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

pub fn format<K: Borrow<str> + Eq + Hash>(
    mut format: &str,
    context: HashMap<K, Formattable>,
) -> Option<String> {
    let format = &mut format;
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
                fill,
                alignment,
                sign,
                hash,
                zero,
                width,
                precision,
                trait_,
            } = FormatArgument::from_str(format)?;
            let value = context.get(ident)?;
            if fill.is_some() {
                unimplemented!("fill is not supported");
                // let mut tmp = String::new();
                // format_value(
                //     &mut tmp, value, 0, precision, alignment, sign, hash,
                // zero, trait_, )?;
                // if tmp.len() < width {
                //     let fill = &fill.to_string();
                //     let width = width - tmp.len();
                //     if alignment == Alignment::Right {
                //         out.push_str(&fill.repeat(width))
                //     }
                //     if alignment == Alignment::Center {
                //         out.push_str(&fill.repeat(width / 2))
                //     }
                //     out.push_str(&tmp);
                //     if alignment == Alignment::Center {
                //         out.push_str(&fill.repeat(width - width / 2))
                //     }
                //     if alignment == Alignment::Left {
                //         out.push_str(&fill.repeat(width))
                //     }
                // } else {
                //     out.push_str(&tmp);
                // }
            } else {
                format_value(
                    &mut out, value, width, precision, alignment, sign, hash, zero, trait_,
                )?;
            }
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
    let p = &42;
    assert_eq!(
        format!("{:#p}", p),
        format!("{:#p}", Formattable::pointer(&p).pointer.unwrap())
    );
    assert_eq!(
        format(
            "{ident:#p}",
            [("ident", Formattable::pointer(&p))].into_iter().collect(),
        )
        .unwrap(),
        format!("{ident:#p}", ident = p)
    );
    // assert_eq!(
    //     format(
    //         "{ident:a>+09.15}",
    //         [("ident", Formattable::from(&"hi"))].into_iter().collect(),
    //     )
    //     .unwrap(),
    //     format!("{ident:a>+09.15}", ident = "hi")
    // );
    assert_eq!(
        format("{{hello}}", HashMap::<String, Formattable>::new()).unwrap(),
        "{hello}"
    );
    assert_eq!(
        format(
            "{hi:10} {hi:?} {int:#x?} {int:b} {int:#X} {int:4o} {display} {display:5} {display:05}",
            [
                ("hi", Formattable::debug_display(&"hello")),
                ("int", Formattable::integer(&123u8)),
                ("display", (&10).into())
            ]
            .into_iter()
            .collect()
        )
        .unwrap(),
        r#"hello      "hello" 0x7b 1111011 0x7B  173 10    10 00010"#
    );
}
