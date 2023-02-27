//! Runtime implementation of [`format!`].
//!
//! See [`format`](format()) and [`write`](write()) for details.
//!
//! # Features
//! By default only [`Display`] is supported, the rest of the
//! [formatting traits](https://doc.rust-lang.org/std/fmt/index.html#formatting-traits)
//! can be enabled through the following features.
//!
//! - `debug` enables `?`, `x?` and `X?` trait specifiers
//! - `number` enables `x`, `X`, `b`, `o`, `e` and `E` trait specifiers
//! - `pointer` enables `p` trait specifiers
#![warn(clippy::pedantic, missing_docs)]
#![allow(
    clippy::return_self_not_must_use,
    clippy::wildcard_imports,
    clippy::implicit_hasher
)]
#![cfg_attr(docsrs, feature(doc_auto_cfg))]
use std::borrow::Borrow;
use std::collections::HashMap;
use std::error::Error as StdError;
#[cfg(feature = "pointer")]
use std::fmt::Pointer;
#[cfg(feature = "number")]
use std::fmt::{Binary, LowerExp, LowerHex, Octal, UpperExp, UpperHex};
use std::fmt::{Debug, Display, Error as FmtError, Write};
use std::hash::Hash;
use std::num::ParseIntError;

mod hard_coded;
use hard_coded::format_value;

#[derive(Debug)]
/// Error returned by [`format()`].
pub enum Error {
    /// Value was formatted with unimplemented trait.
    /// - `.0` the trait
    /// - `.1` the byte index of the format argument
    MissingTraitImpl(Trait, usize),
    /// Error occurred while calling `::fmt`
    /// - `.0` the error
    /// - `.1` the byte index of the format argument
    FmtError(FmtError, usize),
    /// Error occurred  while parsing format string
    ParseError(FormatArgumentError),
    /// Tried to format value that was not in context
    /// - `.0` the ident of the value
    /// - `.1` the byte index of the format argument
    MissingValue(String, usize),
    /// Unsupported Option was used.
    /// - `.0` is the option
    /// - `.1` the feature that needs to be enabled to use it
    /// - `.2` the byte index of the format argument
    UnsupportedOption(&'static str, &'static str, usize),
}

impl From<FormatArgumentError> for Error {
    fn from(value: FormatArgumentError) -> Self {
        Self::ParseError(value)
    }
}

#[derive(Debug)]
#[non_exhaustive]
/// The trait used to format.
pub enum Trait {
    /// [`Binary`]
    #[cfg(feature = "number")]
    Binary,
    /// [`Debug`]
    #[cfg(feature = "debug")]
    Debug,
    /// [`Display`]
    Display,
    /// [`LowerExp`]
    #[cfg(feature = "number")]
    LowerExp,
    /// [`LowerHex`]
    #[cfg(feature = "number")]
    LowerHex,
    /// [`Octal`]
    #[cfg(feature = "number")]
    Octal,
    /// [`Pointer`]
    #[cfg(feature = "pointer")]
    Pointer,
    /// [`UpperExp`]
    #[cfg(feature = "number")]
    UpperExp,
    /// [`UpperHex`]
    #[cfg(feature = "number")]
    UpperHex,
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::MissingTraitImpl(t, idx) => write!(
                f,
                "trait `{t:?}` not implemented, used by format argument at {idx}"
            ),
            Error::FmtError(e, idx) => write!(f, "error while formatting at {idx}: {e}"),
            Error::ParseError(e) => write!(f, "error while parsing input: {e}"),
            Error::MissingValue(ident, idx) => {
                write!(f, "specified value not in context `{ident}` at {idx}")
            }
            Error::UnsupportedOption(option, feature, idx) => write!(
                f,
                "option `{option}` is not supported without feature `{feature}` at {idx}"
            ),
        }
    }
}

impl StdError for Error {}

type Result<T = (), E = Error> = std::result::Result<T, E>;

macro_rules! ensure {
    ($condition:expr, $error:expr) => {
        if !$condition {
            return Err($error);
        }
    };
}

#[derive(Default)]
/// Utility struct holding references to the trait implementation of a value to
/// enable runtime verification and execution of them
pub struct Formattable<'a> {
    #[cfg(any(feature = "debug", feature = "number"))]
    debug: Option<&'a dyn Debug>,
    display: Option<&'a dyn Display>,
    #[cfg(feature = "number")]
    binary: Option<&'a dyn Binary>,
    #[cfg(feature = "number")]
    lower_exp: Option<&'a dyn LowerExp>,
    #[cfg(feature = "number")]
    lower_hex: Option<&'a dyn LowerHex>,
    #[cfg(feature = "number")]
    octal: Option<&'a dyn Octal>,
    #[cfg(feature = "number")]
    upper_exp: Option<&'a dyn UpperExp>,
    #[cfg(feature = "number")]
    upper_hex: Option<&'a dyn UpperHex>,
    #[cfg(feature = "pointer")]
    pointer: Option<PointerWrapper<'a>>,
}

macro_rules! formattable_fn {
    (($($cfg:tt)*), ($doc:expr), $name:ident, $builder:ident<$($traits:ident),*> $($fields:ident),+) => {
        /// Creates a [`Formattable`] from a value implementing
        #[doc = $doc]
        $($cfg)* pub fn $name<T: $($traits+)*>(value: &'a T) -> Self {
            #[allow(clippy::needless_update)]
            Self {
                $($fields: Some(value),)*
                ..Default::default()
            }
        }
        /// Adds implementation for
        #[doc = $doc]
        $($cfg)* pub fn $builder<T: $($traits+)*>(mut self, value: &'a T) -> Self {
            $(self.$fields = Some(value);)*
            self
        }
    };
    (($($cfg:tt)*), (), $name:ident, $builder:ident, $getter:ident<$trait:ident>) => {
        formattable_fn!(($($cfg)*), (concat!("[`",stringify!($trait),"`]")), $name, $builder<$trait> $name);
        $($cfg)* fn $getter(&self) -> Result<&dyn $trait, Trait> {
            self.$name.ok_or(Trait::$trait)
        }
    };
}
macro_rules! formattable {
    [$($($cfg:literal, $($doc:literal,)?)? $name:ident, $builder:ident$(, $getter:ident)?<$($traits:ident),*> $($fields:ident),*;)*] => {
        impl<'a> Formattable<'a> {
            $(formattable_fn!(($(#[cfg(feature=$cfg)])?), ($($($doc)?)?), $name, $builder$(, $getter)?<$($traits),*> $($fields),*);)*
        }
    };
}

formattable![
    "debug", "[`Debug`] and [`Display`]", debug_display, and_debug_display<Debug, Display> debug, display;
    "debug", debug, and_debug, get_debug<Debug>;
    display, and_display, get_display<Display>;
    "number", "[`Debug`], [`Display`], [`Octal`], [`LowerHex`], [`UpperHex`], [`Binary`], [`LowerExp`] and [`UpperExp`]", integer, and_integer<Binary, Debug, Display, LowerExp, LowerHex, Octal, UpperExp, UpperHex>
        binary, debug, display, lower_exp, lower_hex, octal, upper_exp, upper_hex;
    "number", "[`Debug`], [`Display`], [`LowerExp`] and [`UpperExp`]", float, and_float<Debug, Display, LowerExp, UpperExp>
        debug, display, lower_exp, upper_exp;
    "number", binary, and_binary, get_binary<Binary>;
    "number", lower_exp, and_lower_exp, get_lower_exp<LowerExp>;
    "number", lower_hex, and_lower_hex, get_lower_hex<LowerHex>;
    "number", octal, and_octal, get_octal<Octal>;
    "number", upper_exp, and_upper_exp, get_upper_exp<UpperExp>;
    "number", upper_hex, and_upper_hex, get_upper_hex<UpperHex>;
];

#[cfg(feature = "pointer")]
impl<'a> Formattable<'a> {
    /// Creates a [`Formattable`] from a value implementing [`Pointer`].
    pub fn pointer<T: Pointer>(value: &'a T) -> Self {
        Self::default().and_pointer(value)
    }

    /// Adds implementation for [`Pointer`]
    pub fn and_pointer<T: Pointer>(mut self, value: &'a T) -> Self {
        self.pointer = Some(PointerWrapper(value));
        self
    }

    fn get_pointer(&self) -> Result<PointerWrapper, Trait> {
        self.pointer.ok_or(Trait::Pointer)
    }
}

#[cfg(feature = "pointer")]
#[derive(Clone, Copy)]
struct PointerWrapper<'a>(&'a dyn Pointer);

#[cfg(feature = "pointer")]
impl Pointer for PointerWrapper<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Pointer::fmt(self.0, f)
    }
}

#[cfg(all(feature = "display", feature = "debug"))]
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

#[derive(Debug, Default, PartialEq, Eq)]
enum Sign {
    Plus,
    Minus,
    #[default]
    None,
}

#[derive(Debug, Default, Clone, Copy)]
#[allow(non_camel_case_types)]
enum TraitSpec {
    #[default]
    Display,
    #[cfg(feature = "debug")]
    Question,
    #[cfg(feature = "debug")]
    LowerHexQuestion,
    #[cfg(feature = "debug")]
    UpperHexQuestion,
    #[cfg(feature = "number")]
    LowerHex,
    #[cfg(feature = "number")]
    UpperHex,
    #[cfg(feature = "number")]
    Binary,
    #[cfg(feature = "number")]
    Octal,
    #[cfg(feature = "number")]
    LowerExp,
    #[cfg(feature = "number")]
    UpperExp,
    #[cfg(feature = "pointer")]
    Pointer,
}

impl TraitSpec {
    fn from_str(format: &mut &str, idx: &mut usize) -> Result<Self> {
        if format.starts_with('}') {
            return Ok(Self::Display);
        }
        match format.as_bytes()[0] {
            #[cfg(feature = "debug")]
            b'?' => Ok(TraitSpec::Question),
            #[cfg(not(feature = "debug"))]
            b'?' => Err(Error::UnsupportedOption("?", "debug", *idx)),
            #[cfg(feature = "debug")]
            b'x' if format.as_bytes()[1] == b'?' => {
                step(1, format, idx);
                Ok(TraitSpec::LowerHexQuestion)
            }
            #[cfg(not(feature = "debug"))]
            b'x' if format.as_bytes()[1] == b'?' => {
                Err(Error::UnsupportedOption("x?", "debug", *idx))
            }
            #[cfg(feature = "debug")]
            b'X' if format.as_bytes()[1] == b'?' => {
                step(1, format, idx);
                Ok(TraitSpec::UpperHexQuestion)
            }
            #[cfg(not(feature = "debug"))]
            b'X' if format.as_bytes()[1] == b'?' => {
                Err(Error::UnsupportedOption("X?", "debug", *idx))
            }
            #[cfg(feature = "number")]
            b'o' => Ok(TraitSpec::Octal),
            #[cfg(not(feature = "number"))]
            b'o' => Err(Error::UnsupportedOption("o", "number", *idx)),
            #[cfg(feature = "number")]
            b'x' => Ok(TraitSpec::LowerHex),
            #[cfg(not(feature = "number"))]
            b'x' => Err(Error::UnsupportedOption("x", "number", *idx)),
            #[cfg(feature = "number")]
            b'X' => Ok(TraitSpec::UpperHex),
            #[cfg(not(feature = "number"))]
            b'X' => Err(Error::UnsupportedOption("X", "number", *idx)),
            #[cfg(feature = "number")]
            b'b' => Ok(TraitSpec::Binary),
            #[cfg(not(feature = "number"))]
            b'b' => Err(Error::UnsupportedOption("b", "number", *idx)),
            #[cfg(feature = "number")]
            b'e' => Ok(TraitSpec::LowerExp),
            #[cfg(not(feature = "number"))]
            b'e' => Err(Error::UnsupportedOption("e", "number", *idx)),
            #[cfg(feature = "number")]
            b'E' => Ok(TraitSpec::UpperExp),
            #[cfg(not(feature = "number"))]
            b'E' => Err(Error::UnsupportedOption("E", "number", *idx)),
            #[cfg(feature = "pointer")]
            b'p' => Ok(TraitSpec::Pointer),
            #[cfg(not(feature = "pointer"))]
            b'p' => Err(Error::UnsupportedOption("p", "pointer", *idx)),
            _ => Err(FormatArgumentError::ExpectedBrace(*idx).into()),
        }
        .map(|m| {
            step(1, format, idx);
            m
        })
    }
}

#[derive(Default, Debug)]
struct FormatArgument<'a> {
    ident: &'a str,
    alignment: Alignment,
    sign: Sign,
    hash: bool,
    zero: bool,
    width: Option<usize>,
    precision: Option<usize>,
    trait_: TraitSpec,
}

fn step(len: usize, format: &mut &str, idx: &mut usize) {
    *format = &format[len..];
    *idx += len;
}

#[derive(Debug)]
/// Error caused by invalid format string
pub enum FormatArgumentError {
    /// Format spec at byte index is nether closed with a `}`
    FormatSpecUnclosed(usize),
    /// Expected `}` at byte index
    ExpectedBrace(usize),
    /// Unable to parse specified width as usize
    InvalidWidth(ParseIntError, usize),
    /// Unable to parse specified precision as usize
    InvalidPrecision(ParseIntError, usize),
    /// Fill is not supported due to [rust-lang/rfcs#3394](https://github.com/rust-lang/rfcs/pull/3394)
    Fill(usize),
}

impl Display for FormatArgumentError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FormatArgumentError::FormatSpecUnclosed(idx) => {
                write!(f, "Format spec at {idx} is nether closed with a `}}`")
            }
            FormatArgumentError::ExpectedBrace(idx) => write!(f, "Expected `}}` at {idx}"),
            FormatArgumentError::InvalidWidth(e, idx) => {
                write!(f, "Unable to parse width at {idx} as usize: {e}")
            }
            FormatArgumentError::InvalidPrecision(e, idx) => {
                write!(f, "Unable to parse precision at {idx} as usize: {e}")
            }
            FormatArgumentError::Fill(idx) => write!(
                f,
                "Fill is not supported due to https://github.com/rust-lang/rfcs/pull/3394 at {idx}"
            ),
        }
    }
}

impl StdError for FormatArgumentError {}

impl<'a> FormatArgument<'a> {
    fn from_str(format: &mut &'a str, idx: &mut usize) -> Result<Self> {
        let start_index = *idx;
        let mut it = Self::default();
        step(
            format.find(|c: char| !c.is_whitespace()).unwrap_or(0),
            format,
            idx,
        );
        let variable_len = format
            .find(|c| c == ':' || c == '}')
            .ok_or(FormatArgumentError::FormatSpecUnclosed(start_index))?;
        it.ident = &format[..variable_len];
        step(variable_len, format, idx);
        if format.starts_with(':') {
            step(1, format, idx);
            if format.is_empty() {
                return Ok(it);
            }
            let fill = format
                .chars()
                .next()
                .ok_or(FormatArgumentError::ExpectedBrace(*idx))?;
            if format[fill.len_utf8()..].is_empty() {
                return Ok(it);
            }
            if alignment(format[fill.len_utf8()..].as_bytes()[0]).is_some() {
                return Err(FormatArgumentError::Fill(*idx).into());
            } else if fill.is_ascii() {
                if let Some(alignment) = alignment(fill as u8) {
                    it.alignment = alignment;
                    step(1, format, idx);
                }
            }
            if format.is_empty() {
                return Ok(it);
            }
            match format.as_bytes()[0] {
                b'+' => {
                    step(1, format, idx);
                    it.sign = Sign::Plus;
                }
                b'-' => {
                    step(1, format, idx);
                    it.sign = Sign::Minus;
                }
                _ => {}
            }
            if format.is_empty() {
                return Ok(it);
            }
            if b'#' == format.as_bytes()[0] {
                step(1, format, idx);
                it.hash = true;
            }
            if format.starts_with('0') {
                it.zero = true;
                step(1, format, idx);
            }
            if format.starts_with(|c: char| c.is_ascii_digit()) {
                let width = format
                    .find(|c: char| !c.is_ascii_digit())
                    .ok_or(FormatArgumentError::FormatSpecUnclosed(start_index))?;
                it.width = Some(
                    format[..width]
                        .parse()
                        .map_err(|e| FormatArgumentError::InvalidWidth(e, *idx))?,
                );
                step(width, format, idx);
            }
            if format.starts_with('.') {
                step(1, format, idx);
                let precision = format
                    .find(|c: char| !c.is_ascii_digit())
                    .ok_or(FormatArgumentError::FormatSpecUnclosed(start_index))?;
                it.precision = Some(
                    format[..precision]
                        .parse()
                        .map_err(|e| FormatArgumentError::InvalidPrecision(e, *idx))?,
                );
                step(precision, format, idx);
            }
            it.trait_ = TraitSpec::from_str(format, idx)?;
        }
        Ok(it)
    }
}

/// Runtime version of [`write!`].
///
/// Takes a mutable [`Write`] e.g `&mut String`, a format string and a context,
/// containing [`Formattable`] values.
///
/// ```
/// use template::{write, Formattable};
///
/// let mut buf = String::new();
/// write(
///     &mut buf,
///     "{value:+05}", // could be dynamic
///     &[("value", Formattable::display(&12))].into_iter().collect(),
/// )
/// .unwrap();
///
/// assert_eq!(buf, format!("{:+05}", 12));
/// ```
///
/// # Errors
///
/// It will return an error if the specified format string has invalid syntax,
/// the type doesn't implement the expected trait, or the formatting it self
/// failed.
///
/// For more details have a look at [`Error`] and [`FormatArgumentError`].
pub fn write<K: Borrow<str> + Eq + Hash>(
    out: &mut impl Write,
    mut format: &str,
    context: &HashMap<K, Formattable>,
) -> Result {
    let format = &mut format;
    let idx = &mut 0;
    while !format.is_empty() {
        if format.starts_with("{{") || format.starts_with("}}") {
            out.write_str(&format[..1])
                .map_err(|e| Error::FmtError(e, *idx))?;
            step(2, format, idx);
            continue;
        }
        if format.starts_with('{') {
            step(1, format, idx);
            let start = *idx;
            let FormatArgument {
                ident,
                alignment,
                sign,
                hash,
                zero,
                width,
                precision,
                trait_,
            } = FormatArgument::from_str(format, idx)?;
            let value = context
                .get(ident)
                .ok_or(Error::MissingValue(ident.to_string(), start))?;
            format_value(
                out, value, width, precision, alignment, sign, hash, zero, trait_, *idx,
            )?;
            ensure!(
                format.starts_with('}'),
                FormatArgumentError::ExpectedBrace(*idx).into()
            );
            step(1, format, idx);
            continue;
        }
        let next = format
            .chars()
            .next()
            .expect("should contain a char if not empty");
        out.write_char(next).map_err(|e| Error::FmtError(e, *idx))?;
        step(next.len_utf8(), format, idx);
    }
    Ok(())
}

/// Runtime version of [`format!`].
///
/// Takes a string and a context, containing [`Formattable`] values, returns a
/// string.
///
/// ```
/// use template::{format, Formattable};
///
/// let formatted = format(
///     "{value:+05}", // could be dynamic
///     &[("value", Formattable::display(&12))].into_iter().collect(),
/// )
/// .unwrap();
///
/// assert_eq!(formatted, format!("{:+05}", 12));
/// ```
///
/// # Errors
///
/// It will return an error if the specified format string has invalid syntax,
/// the type doesn't implement the expected trait, or the formatting it self
/// failed.
///
/// For more details have a look at [`Error`] and [`FormatArgumentError`].
pub fn format<K: Borrow<str> + Eq + Hash>(
    format: &str,
    context: &HashMap<K, Formattable>,
) -> Result<String> {
    let mut out = String::with_capacity(format.len());
    write(&mut out, format, context)?;
    Ok(out)
}
