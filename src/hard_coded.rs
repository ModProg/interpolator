#![allow(non_snake_case)]
use super::*;

macro_rules! format_value {
    ($name:ident($out:ident, $v:ident, $Trait:ident): $($tts:tt)*) => {
        #[allow(clippy::too_many_arguments)]
        pub(crate) fn $name(
            $out: &mut String,
            $v: impl std::fmt::$Trait,
            width: Option<usize>,
            precision: Option<usize>,
            alignment: crate::Alignment,
            sign: crate::Sign,
            hash: bool,
            zero: bool,
        ) -> crate::Result<(), std::fmt::Error> {
            use {crate::Alignment as A, crate::Sign as S};
            use std::fmt::Write;

            match (width, precision, alignment, sign, hash, zero) {
                $($tts)*
            }
        }
    };
}

macro_rules! call_format_value {
    (
        match
        $out:ident,
        $value:ident,
        $width:ident,
        $precision:ident,
        $alignment:ident,
        $sign:ident,
        $hash:ident,
        $zero:ident,
        $trait_:ident,
        $idx:ident {
            $($Trait:ident => $fn:ident($getter:ident $(, $feature:literal)?),)*
        }
    ) => {
        match $trait_ {
            $($(#[cfg(feature = $feature)])? crate::TraitSpec::$Trait => {
                let value = match $value.$getter() {
                    Ok(v) => v,
                    Err(e) => return Err(crate::Error::MissingTraitImpl(e, $idx))
                };
                $fn::$fn($out, value, $width, $precision, $alignment, $sign, $hash, $zero)
            },)*
        }
    };
}

#[cfg(feature = "number")]
mod E;
#[cfg(feature = "number")]
mod X;
#[cfg(feature = "debug")]
mod Xquestion;
#[cfg(feature = "number")]
mod b;
mod display;
#[cfg(feature = "number")]
mod e;
#[cfg(feature = "number")]
mod o;
#[cfg(feature = "pointer")]
mod p;
#[cfg(feature = "debug")]
mod question;
#[cfg(feature = "number")]
mod x;
#[cfg(feature = "debug")]
mod xquestion;

#[allow(clippy::too_many_arguments)]
pub(crate) fn format_value(
    out: &mut String,
    value: &Formattable,
    width: Option<usize>,
    precision: Option<usize>,
    alignment: Alignment,
    sign: Sign,
    hash: bool,
    zero: bool,
    trait_: TraitSpec,
    idx: usize,
) -> Result<()> {
    call_format_value! {
        match out, value, width, precision, alignment, sign, hash, zero, trait_, idx {
            Display => display(get_display),
            Question => question(get_debug, "debug"),
            xQuestion => xquestion(get_debug, "debug"),
            XQuestion => Xquestion(get_debug, "debug"),
            x => x(get_lower_hex, "number"),
            X => X(get_upper_hex, "number"),
            b => b(get_binary, "number"),
            o => o(get_octal, "number"),
            e => e(get_lower_exp, "number"),
            E => E(get_upper_exp, "number"),
            p => p(get_pointer, "pointer"),
        }
    }
    .map_err(|e| Error::FmtError(e, idx))
}
