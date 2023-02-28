//! Runtime implementation of [`format!`].
//!
//! # `std::fmt` compatible formatting
//!
//! All options but the fill character for alignment is supported
//! (due to [rust-lang/rfcs#3394](https://github.com/rust-lang/rfcs/pull/3394)).
//!
//! Though the non [`Display`] traits need to be enabled through
//! [features](#features).
//!
//! ```
//! use interpolator::{format, Formattable};
//!
//! let formatted = format(
//!     "{value:+05}", // could be dynamic
//!     &[("value", Formattable::display(&12))].into_iter().collect(),
//! )?;
//!
//! assert_eq!(formatted, format!("{:+05}", 12));
//! # return Ok::<(), interpolator::Error>(())
//! ```
//!
//! # `i` iter format
//!
//! The feature `iter` enables an additional format trait `i`, it allows to
//! format a list of values with a format string and an optional join
//! expression.
//!
//! The syntax is `{list:i(the format string, `{it}` is the array element)(the
//! optional join)}`, an empty join can also be omitted `{list:i({it})}`. Should
//! you need to use `)` inside your format string or join, you can add `#`
//! similar to rust's [raw string](https://doc.rust-lang.org/reference/tokens.html#raw-string-literals)
//!
//! A [`Formattable`] implementing iter is created using [`Formattable::iter`]:
//!
//! ```
//! // HashMap macro
//! use collection_literals::hash;
//! use interpolator::{format, Formattable};
//! // Needs to be a slice of references so because `Formattable::display` expects a
//! // reference
//! let items = [&"hello", &"hi", &"hey"].map(Formattable::display);
//! let items = Formattable::iter(&items);
//! let format_str = "Greetings: {items:i(`{it}`)(, )}";
//! assert_eq!(
//!     format(format_str, &hash!("items" => items))?,
//!     "Greetings: `hello`, `hi`, `hey`"
//! );
//! # return Ok::<(), interpolator::Error>(())
//! ```
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
    clippy::wildcard_imports,
    clippy::implicit_hasher,
    clippy::enum_glob_use,
    clippy::module_name_repetitions
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

#[macro_use]
mod error;
pub use error::*;
mod hard_coded;
use hard_coded::format_value;
mod formattable;
pub use formattable::*;
mod parser;
use parser::*;

type Result<T = (), E = Error> = std::result::Result<T, E>;

/// Runtime version of [`format!`].
///
/// Takes a string and a context, containing [`Formattable`] values, returns a
/// string.
///
/// ```
/// use interpolator::{format, Formattable};
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
/// the type doesn't implement the expected trait, or the formatting itself
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

/// Runtime version of [`write!`].
///
/// Takes a mutable [`Write`] e.g. `&mut String`, a format string and a context,
/// containing [`Formattable`] values.
///
/// ```
/// use interpolator::{write, Formattable};
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
/// the type doesn't implement the expected trait, or the formatting itself
/// failed.
///
/// For more details have a look at [`Error`] and [`FormatArgumentError`].
pub fn write<'a, K: Borrow<str> + Eq + Hash, F: Borrow<Formattable<'a>>>(
    out: &mut impl Write,
    mut format: &str,
    context: &'a HashMap<K, F>,
) -> Result {
    let format = &mut format;
    let idx = &mut 0;
    while !format.is_empty() {
        if format.starts_with("{{") || format.starts_with("}}") {
            out.write_str(&format[..1])
                .map_err(|e| Error::Fmt(e, *idx))?;
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
                out,
                value.borrow(),
                width,
                precision,
                alignment,
                sign,
                hash,
                zero,
                trait_,
                *idx,
            )?;
            ensure!(format.starts_with('}'), ParseError::Expected('}', *idx));
            step(1, format, idx);
            continue;
        }
        let next = format
            .chars()
            .next()
            .expect("should contain a char if not empty");
        out.write_char(next).map_err(|e| Error::Fmt(e, *idx))?;
        step(next.len_utf8(), format, idx);
    }
    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn iter() {
        // HashMap macro
        use collection_literals::hash;
        // Needs to be a slice of references so because `Formattable::display` expects a
        // reference
        let items = [&"hello", &"hi", &"hey"].map(Formattable::display);
        let items = Formattable::iter(&items);
        let format_str = "Greetings: {items:i(`{it}`)(,)}";
        assert_eq!(
            format(format_str, &hash!("items" => items)).unwrap(),
            "Greetings: `hello`, `hi`, `hey`"
        );

        assert_eq!(
            format(
                "{h:i((`{it}`),#() )#)}",
                &[(
                    "h",
                    Formattable::iter(&[&"hi", &"hello"].map(Formattable::display)),
                )]
                .into_iter()
                .collect(),
            )
            .unwrap(),
            "`hi`) `hello`"
        );
    }
}
