use crate::*;

fn write_iter(
    out: &mut impl Write,
    value: &[Formattable<'_>],
    range: Option<Range>,
    format: &str,
    join: Option<&str>,
) -> Result {
    if value.is_empty() {
        return Ok(());
    }
    let Range(lhs, inclusive, rhs) = range.unwrap_or(Range(None, false, None));
    let rhs = rhs.unwrap_or(isize::MAX);
    let rhs = (usize::try_from(rhs).unwrap_or(value.len().saturating_sub(rhs.unsigned_abs()))
        + usize::from(inclusive))
    .min(value.len());
    let lhs = lhs.unwrap_or(0);
    let lhs = usize::try_from(lhs)
        .unwrap_or(value.len().saturating_sub(lhs.unsigned_abs()))
        .min(rhs);

    if rhs > lhs {
        for value in &value[lhs..rhs - 1] {
            let mut context = HashMap::new();
            context.insert("it", value);
            write(out, format, &context)?;
            if let Some(join) = join {
                write!(out, "{join}").map_err(|e| Error::Fmt(e, 0))?;
            }
        }
        if let Some(value) = value[..rhs].last() {
            let mut context = HashMap::new();
            context.insert("it", value);
            write(out, format, &context)?;
        }
    }
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
    range: Option<Range>,
    format: &str,
    join: Option<&str>,
) -> Result {
    match (precision, sign, hash, zero) {
        (None, Sign::None, false, false) => {
            if let Some(width) = width {
                let mut buf = String::new();
                write_iter(&mut buf, value, range, format, join)?;
                match alignment {
                    Alignment::Left | Alignment::None => write!(out, "{buf:<width$}"),
                    Alignment::Center => write!(out, "{buf:^width$}"),
                    Alignment::Right => write!(out, "{buf:>width$}"),
                }
                .map_err(|e| Error::Fmt(e, 0))
            } else {
                write_iter(out, value, range, format, join)
            }
        }
        _ => Err(ParseError::Iter(0).into()),
    }
}
