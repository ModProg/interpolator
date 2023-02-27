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
mod upper_exp;
#[cfg(feature = "number")]
mod upper_hex;
#[cfg(feature = "debug")]
mod upper_hex_debug;
#[cfg(feature = "number")]
mod binary;
mod display;
#[cfg(feature = "number")]
mod lower_exp;
#[cfg(feature = "number")]
mod octal;
#[cfg(feature = "pointer")]
mod pointer;
#[cfg(feature = "debug")]
mod debug;
#[cfg(feature = "number")]
mod lower_hex;
#[cfg(feature = "debug")]
mod lower_hex_question;

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
            Question => debug(get_debug, "debug"),
            xQuestion => lower_hex_question(get_debug, "debug"),
            XQuestion => upper_hex_debug(get_debug, "debug"),
            x => lower_hex(get_lower_hex, "number"),
            X => upper_hex(get_upper_hex, "number"),
            b => binary(get_binary, "number"),
            o => octal(get_octal, "number"),
            e => lower_exp(get_lower_exp, "number"),
            E => upper_exp(get_upper_exp, "number"),
            p => pointer(get_pointer, "pointer"),
        }
    }
    .map_err(|e| Error::FmtError(e, idx))
}
