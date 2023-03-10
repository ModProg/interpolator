format_value! {debug(out, value, Debug):
    (None, None, A::None, S::None, false, false) => write!(out, "{value:?}"),
    (None, None, A::None, S::None, false, true) => write!(out, "{value:0?}"),
    (None, None, A::None, S::Plus, false, false) => write!(out, "{value:+?}"),
    (None, None, A::None, S::Plus, false, true) => write!(out, "{value:+0?}"),
    (None, None, A::None, S::Minus, false, false) => write!(out, "{value:-?}"),
    (None, None, A::None, S::Minus, false, true) => write!(out, "{value:-0?}"),
    (None, None, A::Left, S::None, false, false) => write!(out, "{value:<?}"),
    (None, None, A::Left, S::None, false, true) => write!(out, "{value:<0?}"),
    (None, None, A::Left, S::Plus, false, false) => write!(out, "{value:<+?}"),
    (None, None, A::Left, S::Plus, false, true) => write!(out, "{value:<+0?}"),
    (None, None, A::Left, S::Minus, false, false) => write!(out, "{value:<-?}"),
    (None, None, A::Left, S::Minus, false, true) => write!(out, "{value:<-0?}"),
    (None, None, A::Center, S::None, false, false) => write!(out, "{value:^?}"),
    (None, None, A::Center, S::None, false, true) => write!(out, "{value:^0?}"),
    (None, None, A::Center, S::Plus, false, false) => write!(out, "{value:^+?}"),
    (None, None, A::Center, S::Plus, false, true) => write!(out, "{value:^+0?}"),
    (None, None, A::Center, S::Minus, false, false) => write!(out, "{value:^-?}"),
    (None, None, A::Center, S::Minus, false, true) => write!(out, "{value:^-0?}"),
    (None, None, A::Right, S::None, false, false) => write!(out, "{value:>?}"),
    (None, None, A::Right, S::None, false, true) => write!(out, "{value:>0?}"),
    (None, None, A::Right, S::Plus, false, false) => write!(out, "{value:>+?}"),
    (None, None, A::Right, S::Plus, false, true) => write!(out, "{value:>+0?}"),
    (None, None, A::Right, S::Minus, false, false) => write!(out, "{value:>-?}"),
    (None, None, A::Right, S::Minus, false, true) => write!(out, "{value:>-0?}"),
    (None, None, A::None, S::None, true, false) => write!(out, "{value:#?}"),
    (None, None, A::None, S::None, true, true) => write!(out, "{value:#0?}"),
    (None, None, A::None, S::Plus, true, false) => write!(out, "{value:+#?}"),
    (None, None, A::None, S::Plus, true, true) => write!(out, "{value:+#0?}"),
    (None, None, A::None, S::Minus, true, false) => write!(out, "{value:-#?}"),
    (None, None, A::None, S::Minus, true, true) => write!(out, "{value:-#0?}"),
    (None, None, A::Left, S::None, true, false) => write!(out, "{value:<#?}"),
    (None, None, A::Left, S::None, true, true) => write!(out, "{value:<#0?}"),
    (None, None, A::Left, S::Plus, true, false) => write!(out, "{value:<+#?}"),
    (None, None, A::Left, S::Plus, true, true) => write!(out, "{value:<+#0?}"),
    (None, None, A::Left, S::Minus, true, false) => write!(out, "{value:<-#?}"),
    (None, None, A::Left, S::Minus, true, true) => write!(out, "{value:<-#0?}"),
    (None, None, A::Center, S::None, true, false) => write!(out, "{value:^#?}"),
    (None, None, A::Center, S::None, true, true) => write!(out, "{value:^#0?}"),
    (None, None, A::Center, S::Plus, true, false) => write!(out, "{value:^+#?}"),
    (None, None, A::Center, S::Plus, true, true) => write!(out, "{value:^+#0?}"),
    (None, None, A::Center, S::Minus, true, false) => write!(out, "{value:^-#?}"),
    (None, None, A::Center, S::Minus, true, true) => write!(out, "{value:^-#0?}"),
    (None, None, A::Right, S::None, true, false) => write!(out, "{value:>#?}"),
    (None, None, A::Right, S::None, true, true) => write!(out, "{value:>#0?}"),
    (None, None, A::Right, S::Plus, true, false) => write!(out, "{value:>+#?}"),
    (None, None, A::Right, S::Plus, true, true) => write!(out, "{value:>+#0?}"),
    (None, None, A::Right, S::Minus, true, false) => write!(out, "{value:>-#?}"),
    (None, None, A::Right, S::Minus, true, true) => write!(out, "{value:>-#0?}"),
    (None, Some(pr), A::None, S::None, false, false) => write!(out, "{value:.pr$?}"),
    (None, Some(pr), A::None, S::None, false, true) => write!(out, "{value:0.pr$?}"),
    (None, Some(pr), A::None, S::Plus, false, false) => write!(out, "{value:+.pr$?}"),
    (None, Some(pr), A::None, S::Plus, false, true) => write!(out, "{value:+0.pr$?}"),
    (None, Some(pr), A::None, S::Minus, false, false) => write!(out, "{value:-.pr$?}"),
    (None, Some(pr), A::None, S::Minus, false, true) => write!(out, "{value:-0.pr$?}"),
    (None, Some(pr), A::Left, S::None, false, false) => write!(out, "{value:<.pr$?}"),
    (None, Some(pr), A::Left, S::None, false, true) => write!(out, "{value:<0.pr$?}"),
    (None, Some(pr), A::Left, S::Plus, false, false) => write!(out, "{value:<+.pr$?}"),
    (None, Some(pr), A::Left, S::Plus, false, true) => write!(out, "{value:<+0.pr$?}"),
    (None, Some(pr), A::Left, S::Minus, false, false) => write!(out, "{value:<-.pr$?}"),
    (None, Some(pr), A::Left, S::Minus, false, true) => write!(out, "{value:<-0.pr$?}"),
    (None, Some(pr), A::Center, S::None, false, false) => write!(out, "{value:^.pr$?}"),
    (None, Some(pr), A::Center, S::None, false, true) => write!(out, "{value:^0.pr$?}"),
    (None, Some(pr), A::Center, S::Plus, false, false) => write!(out, "{value:^+.pr$?}"),
    (None, Some(pr), A::Center, S::Plus, false, true) => write!(out, "{value:^+0.pr$?}"),
    (None, Some(pr), A::Center, S::Minus, false, false) => write!(out, "{value:^-.pr$?}"),
    (None, Some(pr), A::Center, S::Minus, false, true) => write!(out, "{value:^-0.pr$?}"),
    (None, Some(pr), A::Right, S::None, false, false) => write!(out, "{value:>.pr$?}"),
    (None, Some(pr), A::Right, S::None, false, true) => write!(out, "{value:>0.pr$?}"),
    (None, Some(pr), A::Right, S::Plus, false, false) => write!(out, "{value:>+.pr$?}"),
    (None, Some(pr), A::Right, S::Plus, false, true) => write!(out, "{value:>+0.pr$?}"),
    (None, Some(pr), A::Right, S::Minus, false, false) => write!(out, "{value:>-.pr$?}"),
    (None, Some(pr), A::Right, S::Minus, false, true) => write!(out, "{value:>-0.pr$?}"),
    (None, Some(pr), A::None, S::None, true, false) => write!(out, "{value:#.pr$?}"),
    (None, Some(pr), A::None, S::None, true, true) => write!(out, "{value:#0.pr$?}"),
    (None, Some(pr), A::None, S::Plus, true, false) => write!(out, "{value:+#.pr$?}"),
    (None, Some(pr), A::None, S::Plus, true, true) => write!(out, "{value:+#0.pr$?}"),
    (None, Some(pr), A::None, S::Minus, true, false) => write!(out, "{value:-#.pr$?}"),
    (None, Some(pr), A::None, S::Minus, true, true) => write!(out, "{value:-#0.pr$?}"),
    (None, Some(pr), A::Left, S::None, true, false) => write!(out, "{value:<#.pr$?}"),
    (None, Some(pr), A::Left, S::None, true, true) => write!(out, "{value:<#0.pr$?}"),
    (None, Some(pr), A::Left, S::Plus, true, false) => write!(out, "{value:<+#.pr$?}"),
    (None, Some(pr), A::Left, S::Plus, true, true) => write!(out, "{value:<+#0.pr$?}"),
    (None, Some(pr), A::Left, S::Minus, true, false) => write!(out, "{value:<-#.pr$?}"),
    (None, Some(pr), A::Left, S::Minus, true, true) => write!(out, "{value:<-#0.pr$?}"),
    (None, Some(pr), A::Center, S::None, true, false) => write!(out, "{value:^#.pr$?}"),
    (None, Some(pr), A::Center, S::None, true, true) => write!(out, "{value:^#0.pr$?}"),
    (None, Some(pr), A::Center, S::Plus, true, false) => write!(out, "{value:^+#.pr$?}"),
    (None, Some(pr), A::Center, S::Plus, true, true) => write!(out, "{value:^+#0.pr$?}"),
    (None, Some(pr), A::Center, S::Minus, true, false) => write!(out, "{value:^-#.pr$?}"),
    (None, Some(pr), A::Center, S::Minus, true, true) => write!(out, "{value:^-#0.pr$?}"),
    (None, Some(pr), A::Right, S::None, true, false) => write!(out, "{value:>#.pr$?}"),
    (None, Some(pr), A::Right, S::None, true, true) => write!(out, "{value:>#0.pr$?}"),
    (None, Some(pr), A::Right, S::Plus, true, false) => write!(out, "{value:>+#.pr$?}"),
    (None, Some(pr), A::Right, S::Plus, true, true) => write!(out, "{value:>+#0.pr$?}"),
    (None, Some(pr), A::Right, S::Minus, true, false) => write!(out, "{value:>-#.pr$?}"),
    (None, Some(pr), A::Right, S::Minus, true, true) => write!(out, "{value:>-#0.pr$?}"),
    (Some(wi), None, A::None, S::None, false, false) => write!(out, "{value:wi$?}"),
    (Some(wi), None, A::None, S::None, false, true) => write!(out, "{value:0wi$?}"),
    (Some(wi), None, A::None, S::Plus, false, false) => write!(out, "{value:+wi$?}"),
    (Some(wi), None, A::None, S::Plus, false, true) => write!(out, "{value:+0wi$?}"),
    (Some(wi), None, A::None, S::Minus, false, false) => write!(out, "{value:-wi$?}"),
    (Some(wi), None, A::None, S::Minus, false, true) => write!(out, "{value:-0wi$?}"),
    (Some(wi), None, A::Left, S::None, false, false) => write!(out, "{value:<wi$?}"),
    (Some(wi), None, A::Left, S::None, false, true) => write!(out, "{value:<0wi$?}"),
    (Some(wi), None, A::Left, S::Plus, false, false) => write!(out, "{value:<+wi$?}"),
    (Some(wi), None, A::Left, S::Plus, false, true) => write!(out, "{value:<+0wi$?}"),
    (Some(wi), None, A::Left, S::Minus, false, false) => write!(out, "{value:<-wi$?}"),
    (Some(wi), None, A::Left, S::Minus, false, true) => write!(out, "{value:<-0wi$?}"),
    (Some(wi), None, A::Center, S::None, false, false) => write!(out, "{value:^wi$?}"),
    (Some(wi), None, A::Center, S::None, false, true) => write!(out, "{value:^0wi$?}"),
    (Some(wi), None, A::Center, S::Plus, false, false) => write!(out, "{value:^+wi$?}"),
    (Some(wi), None, A::Center, S::Plus, false, true) => write!(out, "{value:^+0wi$?}"),
    (Some(wi), None, A::Center, S::Minus, false, false) => write!(out, "{value:^-wi$?}"),
    (Some(wi), None, A::Center, S::Minus, false, true) => write!(out, "{value:^-0wi$?}"),
    (Some(wi), None, A::Right, S::None, false, false) => write!(out, "{value:>wi$?}"),
    (Some(wi), None, A::Right, S::None, false, true) => write!(out, "{value:>0wi$?}"),
    (Some(wi), None, A::Right, S::Plus, false, false) => write!(out, "{value:>+wi$?}"),
    (Some(wi), None, A::Right, S::Plus, false, true) => write!(out, "{value:>+0wi$?}"),
    (Some(wi), None, A::Right, S::Minus, false, false) => write!(out, "{value:>-wi$?}"),
    (Some(wi), None, A::Right, S::Minus, false, true) => write!(out, "{value:>-0wi$?}"),
    (Some(wi), None, A::None, S::None, true, false) => write!(out, "{value:#wi$?}"),
    (Some(wi), None, A::None, S::None, true, true) => write!(out, "{value:#0wi$?}"),
    (Some(wi), None, A::None, S::Plus, true, false) => write!(out, "{value:+#wi$?}"),
    (Some(wi), None, A::None, S::Plus, true, true) => write!(out, "{value:+#0wi$?}"),
    (Some(wi), None, A::None, S::Minus, true, false) => write!(out, "{value:-#wi$?}"),
    (Some(wi), None, A::None, S::Minus, true, true) => write!(out, "{value:-#0wi$?}"),
    (Some(wi), None, A::Left, S::None, true, false) => write!(out, "{value:<#wi$?}"),
    (Some(wi), None, A::Left, S::None, true, true) => write!(out, "{value:<#0wi$?}"),
    (Some(wi), None, A::Left, S::Plus, true, false) => write!(out, "{value:<+#wi$?}"),
    (Some(wi), None, A::Left, S::Plus, true, true) => write!(out, "{value:<+#0wi$?}"),
    (Some(wi), None, A::Left, S::Minus, true, false) => write!(out, "{value:<-#wi$?}"),
    (Some(wi), None, A::Left, S::Minus, true, true) => write!(out, "{value:<-#0wi$?}"),
    (Some(wi), None, A::Center, S::None, true, false) => write!(out, "{value:^#wi$?}"),
    (Some(wi), None, A::Center, S::None, true, true) => write!(out, "{value:^#0wi$?}"),
    (Some(wi), None, A::Center, S::Plus, true, false) => write!(out, "{value:^+#wi$?}"),
    (Some(wi), None, A::Center, S::Plus, true, true) => write!(out, "{value:^+#0wi$?}"),
    (Some(wi), None, A::Center, S::Minus, true, false) => write!(out, "{value:^-#wi$?}"),
    (Some(wi), None, A::Center, S::Minus, true, true) => write!(out, "{value:^-#0wi$?}"),
    (Some(wi), None, A::Right, S::None, true, false) => write!(out, "{value:>#wi$?}"),
    (Some(wi), None, A::Right, S::None, true, true) => write!(out, "{value:>#0wi$?}"),
    (Some(wi), None, A::Right, S::Plus, true, false) => write!(out, "{value:>+#wi$?}"),
    (Some(wi), None, A::Right, S::Plus, true, true) => write!(out, "{value:>+#0wi$?}"),
    (Some(wi), None, A::Right, S::Minus, true, false) => write!(out, "{value:>-#wi$?}"),
    (Some(wi), None, A::Right, S::Minus, true, true) => write!(out, "{value:>-#0wi$?}"),
    (Some(wi), Some(pr), A::None, S::None, false, false) => write!(out, "{value:wi$.pr$?}"),
    (Some(wi), Some(pr), A::None, S::None, false, true) => write!(out, "{value:0wi$.pr$?}"),
    (Some(wi), Some(pr), A::None, S::Plus, false, false) => write!(out, "{value:+wi$.pr$?}"),
    (Some(wi), Some(pr), A::None, S::Plus, false, true) => write!(out, "{value:+0wi$.pr$?}"),
    (Some(wi), Some(pr), A::None, S::Minus, false, false) => write!(out, "{value:-wi$.pr$?}"),
    (Some(wi), Some(pr), A::None, S::Minus, false, true) => write!(out, "{value:-0wi$.pr$?}"),
    (Some(wi), Some(pr), A::Left, S::None, false, false) => write!(out, "{value:<wi$.pr$?}"),
    (Some(wi), Some(pr), A::Left, S::None, false, true) => write!(out, "{value:<0wi$.pr$?}"),
    (Some(wi), Some(pr), A::Left, S::Plus, false, false) => write!(out, "{value:<+wi$.pr$?}"),
    (Some(wi), Some(pr), A::Left, S::Plus, false, true) => write!(out, "{value:<+0wi$.pr$?}"),
    (Some(wi), Some(pr), A::Left, S::Minus, false, false) => write!(out, "{value:<-wi$.pr$?}"),
    (Some(wi), Some(pr), A::Left, S::Minus, false, true) => write!(out, "{value:<-0wi$.pr$?}"),
    (Some(wi), Some(pr), A::Center, S::None, false, false) => write!(out, "{value:^wi$.pr$?}"),
    (Some(wi), Some(pr), A::Center, S::None, false, true) => write!(out, "{value:^0wi$.pr$?}"),
    (Some(wi), Some(pr), A::Center, S::Plus, false, false) => write!(out, "{value:^+wi$.pr$?}"),
    (Some(wi), Some(pr), A::Center, S::Plus, false, true) => write!(out, "{value:^+0wi$.pr$?}"),
    (Some(wi), Some(pr), A::Center, S::Minus, false, false) => write!(out, "{value:^-wi$.pr$?}"),
    (Some(wi), Some(pr), A::Center, S::Minus, false, true) => write!(out, "{value:^-0wi$.pr$?}"),
    (Some(wi), Some(pr), A::Right, S::None, false, false) => write!(out, "{value:>wi$.pr$?}"),
    (Some(wi), Some(pr), A::Right, S::None, false, true) => write!(out, "{value:>0wi$.pr$?}"),
    (Some(wi), Some(pr), A::Right, S::Plus, false, false) => write!(out, "{value:>+wi$.pr$?}"),
    (Some(wi), Some(pr), A::Right, S::Plus, false, true) => write!(out, "{value:>+0wi$.pr$?}"),
    (Some(wi), Some(pr), A::Right, S::Minus, false, false) => write!(out, "{value:>-wi$.pr$?}"),
    (Some(wi), Some(pr), A::Right, S::Minus, false, true) => write!(out, "{value:>-0wi$.pr$?}"),
    (Some(wi), Some(pr), A::None, S::None, true, false) => write!(out, "{value:#wi$.pr$?}"),
    (Some(wi), Some(pr), A::None, S::None, true, true) => write!(out, "{value:#0wi$.pr$?}"),
    (Some(wi), Some(pr), A::None, S::Plus, true, false) => write!(out, "{value:+#wi$.pr$?}"),
    (Some(wi), Some(pr), A::None, S::Plus, true, true) => write!(out, "{value:+#0wi$.pr$?}"),
    (Some(wi), Some(pr), A::None, S::Minus, true, false) => write!(out, "{value:-#wi$.pr$?}"),
    (Some(wi), Some(pr), A::None, S::Minus, true, true) => write!(out, "{value:-#0wi$.pr$?}"),
    (Some(wi), Some(pr), A::Left, S::None, true, false) => write!(out, "{value:<#wi$.pr$?}"),
    (Some(wi), Some(pr), A::Left, S::None, true, true) => write!(out, "{value:<#0wi$.pr$?}"),
    (Some(wi), Some(pr), A::Left, S::Plus, true, false) => write!(out, "{value:<+#wi$.pr$?}"),
    (Some(wi), Some(pr), A::Left, S::Plus, true, true) => write!(out, "{value:<+#0wi$.pr$?}"),
    (Some(wi), Some(pr), A::Left, S::Minus, true, false) => write!(out, "{value:<-#wi$.pr$?}"),
    (Some(wi), Some(pr), A::Left, S::Minus, true, true) => write!(out, "{value:<-#0wi$.pr$?}"),
    (Some(wi), Some(pr), A::Center, S::None, true, false) => write!(out, "{value:^#wi$.pr$?}"),
    (Some(wi), Some(pr), A::Center, S::None, true, true) => write!(out, "{value:^#0wi$.pr$?}"),
    (Some(wi), Some(pr), A::Center, S::Plus, true, false) => write!(out, "{value:^+#wi$.pr$?}"),
    (Some(wi), Some(pr), A::Center, S::Plus, true, true) => write!(out, "{value:^+#0wi$.pr$?}"),
    (Some(wi), Some(pr), A::Center, S::Minus, true, false) => write!(out, "{value:^-#wi$.pr$?}"),
    (Some(wi), Some(pr), A::Center, S::Minus, true, true) => write!(out, "{value:^-#0wi$.pr$?}"),
    (Some(wi), Some(pr), A::Right, S::None, true, false) => write!(out, "{value:>#wi$.pr$?}"),
    (Some(wi), Some(pr), A::Right, S::None, true, true) => write!(out, "{value:>#0wi$.pr$?}"),
    (Some(wi), Some(pr), A::Right, S::Plus, true, false) => write!(out, "{value:>+#wi$.pr$?}"),
    (Some(wi), Some(pr), A::Right, S::Plus, true, true) => write!(out, "{value:>+#0wi$.pr$?}"),
    (Some(wi), Some(pr), A::Right, S::Minus, true, false) => write!(out, "{value:>-#wi$.pr$?}"),
    (Some(wi), Some(pr), A::Right, S::Minus, true, true) => write!(out, "{value:>-#0wi$.pr$?}"),
}
