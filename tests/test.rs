use std::fmt::Display;

use derive_more::Display;
use proptest::option;
use proptest::prelude::*;
use proptest_derive::Arbitrary;

#[derive(Debug, Arbitrary, Display, Clone)]
#[allow(non_camel_case_types)]
enum Alignment {
    #[display(fmt = "<")]
    Left,
    #[display(fmt = "^")]
    Center,
    #[display(fmt = ">")]
    Right,
}

#[derive(Debug, Default, Arbitrary, Display, Clone)]
#[allow(non_camel_case_types)]
enum Trait {
    #[default]
    #[display(fmt = "")]
    Display,
    #[display(fmt = "?")]
    Question,
    #[display(fmt = "x?")]
    xQuestion,
    #[display(fmt = "X?")]
    XQuestion,
    x,
    X,
    b,
    o,
    e,
    E,
    p,
}

#[derive(Debug, Default, Arbitrary, Display)]
enum Sign {
    #[display(fmt = "+")]
    Plus,
    #[display(fmt = "-")]
    Minus,
    #[default]
    #[display(fmt = "")]
    No,
}

#[derive(Default, Debug, Arbitrary)]
struct FormatArgument {
    #[proptest(regex = "ident")]
    ident: String,
    #[proptest(strategy = "Just(None)")]
    alignment: Option<(Option<char>, Alignment)>,
    sign: Sign,
    hash: bool,
    zero: bool,
    #[proptest(strategy = "option::of(0usize..20)")]
    width: Option<usize>,
    #[proptest(strategy = "option::of(0usize..20)")]
    precision: Option<usize>,
    #[proptest(strategy = "Just(Trait::Display)")]
    trait_: Trait,
}

impl Display for FormatArgument {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self {
            ident,
            alignment,
            sign,
            hash,
            zero,
            width,
            precision,
            trait_,
        } = self;
        let alignment = alignment
            .as_ref()
            .map(|(fill, alignment)| format!("{}{alignment}", fill.unwrap_or_default()))
            .unwrap_or_default();
        let hash = if *hash { "#" } else { "" };
        let zero = if *zero { "0" } else { "" };
        let width = width.map(|w| w.to_string()).unwrap_or_default();
        let precision = precision.map(|w| format!(".{w}")).unwrap_or_default();
        write!(
            f,
            "{{{ident}:{alignment}{sign}{hash}{zero}{width}{precision}{trait_}}}"
        )
    }
}

proptest! {
    #[test]
    fn display(format_arg in any::<FormatArgument>()) {
        let t = trybuild2::TestCases::new();
        t.pass_inline("display", &format!{r#"
            use template::format;
            fn main() {{
                assert_eq!(
                    format("{format_arg}", [("ident", (&"test").into())].into_iter().collect()).unwrap(),
                    format!("{format_arg}", ident = "test")
                );
            }}
        "#});
    }
}
