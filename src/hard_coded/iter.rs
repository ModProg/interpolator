use std::collections::HashMap;
use std::fmt::Write;

use crate::{write, Alignment, Error, Formattable, ParseError, Result, Sign};

fn write_iter(
    out: &mut impl Write,
    value: &[Formattable<'_>],
    format: &str,
    join: Option<&str>,
) -> Result {
    if value.is_empty() {
        return Ok(());
    }
    for value in &value[..value.len() - 1] {
        let mut context = HashMap::new();
        context.insert("it", value);
        write(out, format, &context)?;
        if let Some(join) = join {
            write!(out, "{join}").map_err(|e| Error::Fmt(e, 0))?;
        }
    }
    let mut context = HashMap::new();
    context.insert("it", value.last().expect("value is not empty"));
    write(out, format, &context)?;
    Ok(())
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn iter(
    out: &mut impl Write,
    value: &[Formattable<'_>],
    width: Option<usize>,
    precision: Option<usize>,
    alignment: crate::Alignment,
    sign: Sign,
    hash: bool,
    zero: bool,
    format: &str,
    join: Option<&str>,
) -> Result {
    match (precision, sign, hash, zero) {
        (None, Sign::None, false, false) => {
            if let Some(width) = width {
                let mut buf = String::new();
                write_iter(&mut buf, value, format, join)?;
                match alignment {
                    Alignment::Left | Alignment::None => write!(out, "{buf:<width$}"),
                    Alignment::Center => write!(out, "{buf:^width$}"),
                    Alignment::Right => write!(out, "{buf:>width$}"),
                }
                .map_err(|e| Error::Fmt(e, 0))
            } else {
                write_iter(out, value, format, join)
            }
        }
        _ => Err(ParseError::Iter(0).into()),
    }
}
