#[macro_export]
macro_rules! assert_dbg {
    ($lhs:expr, $rhs:expr $(, $($fmt:tt)*)?) => {
        ::std::assert_eq!(::std::format!("{:?}", $lhs), $rhs$(, $($fmt)*)?);
    };
}
