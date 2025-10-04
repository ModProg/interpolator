#[cfg(feature = "number")]
use std::f64::consts::PI;
use std::fmt::Display;
use std::iter;

use derive_more::Display;
use proptest::option;
use proptest::prelude::*;
use proptest::sample::select;
use proptest::strategy::ValueTree;
use proptest::test_runner::TestRunner;
use proptest_derive::Arbitrary;
use quote::{quote, ToTokens};

#[derive(Debug, Arbitrary, Display, Clone)]
#[allow(non_camel_case_types)]
enum Alignment {
    #[display("<")]
    Left,
    #[display("^")]
    Center,
    #[display(">")]
    Right,
}

#[derive(Debug, Default, Arbitrary, Display, Clone)]
#[allow(non_camel_case_types)]
enum Trait {
    #[default]
    #[display("")]
    Display,
    #[display("?")]
    Question,
    #[display("x?")]
    xQuestion,
    #[display("X?")]
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
    #[display("+")]
    Plus,
    #[display("-")]
    Minus,
    #[default]
    #[display("")]
    None,
}

#[derive(Default, Debug, Arbitrary)]
#[proptest(params = "&'static [Trait]")]
struct FormatArgument {
    #[proptest(regex = "ident")]
    ident: String,
    // NOTE: fill is not supported for now
    // #[proptest(strategy = "option::of((any::<Option<char>>(), any::<Alignment>()))")]
    #[proptest(strategy = "option::of((Just(None), any::<Alignment>()))")]
    alignment: Option<(Option<char>, Alignment)>,
    sign: Sign,
    hash: bool,
    zero: bool,
    #[proptest(strategy = "option::of(0usize..20)")]
    width: Option<usize>,
    #[proptest(strategy = "option::of(0usize..20)")]
    precision: Option<usize>,
    #[proptest(strategy = "select(params)")]
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
            .map(|(fill, alignment)| {
                format!(
                    "{}{alignment}",
                    fill.map(|c| c.to_string()).unwrap_or_default()
                )
            })
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

fn test(
    value: impl ToTokens,
    converter: impl ToTokens,
    strategy: impl Strategy<Value = FormatArgument>,
) {
    let mut runner = TestRunner::deterministic();
    let format_args = iter::repeat_with(|| strategy.new_tree(&mut runner))
        .take(1000)
        .map(|s| s.unwrap().current().to_string());
    let t = trybuild2::TestCases::new();
    t.pass_inline(
        &converter.to_token_stream().to_string(),
        &quote! {
            use std::fmt::Write;
            use std::thread;

            use collection_literals::hash;
            use interpolator::{{format, Formattable, write}};

            fn main() {
                // This stack overflows without the thread on windows + nightly
                thread::spawn(move ||{
                    let value = &hash!("ident" => Formattable::#converter(&#value));
                    #(
                        assert_eq!(
                            format(#format_args, value).unwrap(),
                            format!(#format_args, ident = #value),
                            "{}", #format_args
                        );
                        let mut buf_rt = String::new();
                        write(&mut buf_rt, #format_args, value).unwrap();
                        let mut buf_ct = String::new();
                        write!(&mut buf_ct, #format_args, ident = #value).unwrap();
                        assert_eq!(buf_rt, buf_ct, "{}", #format_args);
                    )*
                }).join().unwrap();
            }
        }
        .to_string(),
    );
}

#[test]
fn string() {
    test(
        "test",
        #[cfg(feature = "debug")]
        quote!(debug_display),
        #[cfg(not(feature = "debug"))]
        quote!(display),
        FormatArgument::arbitrary_with(&[
            Trait::Display,
            #[cfg(feature = "debug")]
            Trait::Question,
            #[cfg(feature = "debug")]
            Trait::XQuestion,
            #[cfg(feature = "debug")]
            Trait::xQuestion,
        ]),
    );
}

#[test]
#[cfg(feature = "number")]
fn integer() {
    test(
        42,
        quote!(integer),
        FormatArgument::arbitrary_with(&[
            Trait::Display,
            #[cfg(feature = "debug")]
            Trait::Question,
            #[cfg(feature = "debug")]
            Trait::XQuestion,
            #[cfg(feature = "debug")]
            Trait::xQuestion,
            #[cfg(feature = "number")]
            Trait::x,
            #[cfg(feature = "number")]
            Trait::X,
            #[cfg(feature = "number")]
            Trait::b,
            #[cfg(feature = "number")]
            Trait::o,
            #[cfg(feature = "number")]
            Trait::e,
            #[cfg(feature = "number")]
            Trait::E,
        ]),
    );
}
#[test]
#[cfg(feature = "number")]
fn float() {
    test(
        PI,
        quote!(float),
        FormatArgument::arbitrary_with(&[
            Trait::Display,
            #[cfg(feature = "debug")]
            Trait::Question,
            #[cfg(feature = "debug")]
            Trait::XQuestion,
            #[cfg(feature = "debug")]
            Trait::xQuestion,
            #[cfg(feature = "number")]
            Trait::e,
            #[cfg(feature = "number")]
            Trait::E,
        ]),
    );
}

#[test]
#[cfg(feature = "pointer")]
fn pointer() {
    test(
        quote!(&42),
        quote!(pointer),
        FormatArgument::arbitrary_with(&[Trait::p]),
    );
}

proptest! {
    #[test]
    #[ignore = "long runtime"]
    fn string_pt(format_arg in FormatArgument::arbitrary_with(&[Trait::Display, Trait::Question, Trait::XQuestion, Trait::xQuestion])) {
        let t = trybuild2::TestCases::new();
        let format_arg = format_arg.to_string();
        let format_arg = format_arg.escape_default();
        t.pass_inline("display", &format!{r#"
            use interpolator::{{format, Formattable}};
            fn main() {{
                assert_eq!(
                    format("{format_arg}", [("ident", (&"test").into())].into_iter().collect()).unwrap(),
                    format!("{format_arg}", ident = "test"),
                    "{{}}", "{format_arg}"
                );
                assert_eq!(
                    format("{format_arg}", [("ident", Formattable::debug_display(&"test").into())].into_iter().collect()).unwrap(),
                    format!("{format_arg}", ident = "test"),
                    "{{}}", "{format_arg}"
                );
            }}
        "#});
    }

    #[test]
    #[ignore = "long runtime"]
    fn integer_pt(format_arg in FormatArgument::arbitrary_with(&[
            Trait::Display, Trait::Question, Trait::XQuestion, Trait::xQuestion,
            Trait::x, Trait::X, Trait::b, Trait::o, Trait::e, Trait::E
    ])) {
        let t = trybuild2::TestCases::new();
        let format_arg = format_arg.to_string();
        let format_arg = format_arg.escape_default();
        t.pass_inline("integer", &format!{r#"
            use interpolator::{{format, Formattable}};
            fn main() {{
                assert_eq!(
                    format("{format_arg}", [("ident", Formattable::integer(&10))].into_iter().collect()).unwrap(),
                    format!("{format_arg}", ident = 10),
                    "{{}}", "{format_arg}"
                );
            }}
        "#});
    }

    #[test]
    #[ignore = "long runtime"]
    fn float_pt(format_arg in FormatArgument::arbitrary_with(&[
            Trait::Display, Trait::Question, Trait::XQuestion, Trait::xQuestion, Trait::e, Trait::E
    ])) {
        let t = trybuild2::TestCases::new();
        let format_arg = format_arg.to_string();
        let format_arg = format_arg.escape_default();
        t.pass_inline("float", &format!{r#"
            use interpolator::{{format, Formattable}};
            fn main() {{
                assert_eq!(
                    format("{format_arg}", [("ident", Formattable::float(&3.14))].into_iter().collect()).unwrap(),
                    format!("{format_arg}", ident = 3.14),
                    "{{}}", "{format_arg}"
                );
            }}
        "#});
    }

    #[test]
    #[ignore = "long runtime"]
    fn pointer_pt(format_arg in FormatArgument::arbitrary_with(&[Trait::p])) {
        let t = trybuild2::TestCases::new();
        let format_arg = format_arg.to_string();
        let format_arg = format_arg.escape_default();
        t.pass_inline("pointer", &format!{r#"
            use interpolator::{{format, Formattable}};
            fn main() {{
                let p = &42;
                assert_eq!(
                    format("{format_arg}", [("ident", Formattable::pointer(&p))].into_iter().collect()).unwrap(),
                    format!("{format_arg}", ident = p),
                    "{{}}", "{format_arg}"
                );
            }}
        "#});
    }
}
