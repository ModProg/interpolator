# interpolator

[![CI Status](https://github.com/ModProg/interpolator/actions/workflows/test.yaml/badge.svg)](https://github.com/ModProg/interpolator/actions/workflows/test.yaml)
[![Documentation for `main`](https://img.shields.io/badge/docs-main-informational)](https://modprog.github.io/interpolator/interpolator/)
[![Crates.io](https://img.shields.io/crates/v/interpolator)](https://crates.io/crates/interpolator)
[![Docs.rs](https://img.shields.io/crates/v/interpolator?color=informational&label=docs.rs)](https://docs.rs/interpolator)

Runtime implementation of `format!`.

# `format`
Runtime version of `format!`.

Takes a string and a context, containing `Formattable` values, returns a
string.

```rs
use template::{format, Formattable};

let formatted = format(
    "{value:+05}", // could be dynamic
    &[("value", Formattable::display(&12))].into_iter().collect(),
)
.unwrap();

assert_eq!(formatted, format!("{:+05}", 12));
```

# `write`
Runtime version of `write!`.

Takes a mutable `Write` e.g. `&mut String`, a format string and a context,
containing `Formattable` values.
                                                                            
```rs
use template::{write, Formattable};
                                                                            
let mut buf = String::new();
write(
    &mut buf,
    "{value:+05}", // could be dynamic
    &[("value", Formattable::display(&12))].into_iter().collect(),
)
.unwrap();
                                                                            
assert_eq!(buf, format!("{:+05}", 12));
```

# Features
By default only `Display` is supported, the rest of the
[formatting traits](https://doc.rust-lang.org/std/fmt/index.html#formatting-traits)
can be enabled through the following features.

- `debug` enables `?`, `x?` and `X?` trait specifiers
- `number` enables `x`, `X`, `b`, `o`, `e` and `E` trait specifiers
- `pointer` enables `p` trait specifiers
