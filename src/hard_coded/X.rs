format_value! {X(out, value, UpperHex):
    (None, None, A::None, S::None, false, false) => write!(out, "{value:X}"),
    (None, None, A::None, S::None, false, true) => write!(out, "{value:0X}"),
    (None, None, A::None, S::Plus, false, false) => write!(out, "{value:+X}"),
    (None, None, A::None, S::Plus, false, true) => write!(out, "{value:+0X}"),
    (None, None, A::None, S::Minus, false, false) => write!(out, "{value:-X}"),
    (None, None, A::None, S::Minus, false, true) => write!(out, "{value:-0X}"),
    (None, None, A::Left, S::None, false, false) => write!(out, "{value:<X}"),
    (None, None, A::Left, S::None, false, true) => write!(out, "{value:<0X}"),
    (None, None, A::Left, S::Plus, false, false) => write!(out, "{value:<+X}"),
    (None, None, A::Left, S::Plus, false, true) => write!(out, "{value:<+0X}"),
    (None, None, A::Left, S::Minus, false, false) => write!(out, "{value:<-X}"),
    (None, None, A::Left, S::Minus, false, true) => write!(out, "{value:<-0X}"),
    (None, None, A::Center, S::None, false, false) => write!(out, "{value:^X}"),
    (None, None, A::Center, S::None, false, true) => write!(out, "{value:^0X}"),
    (None, None, A::Center, S::Plus, false, false) => write!(out, "{value:^+X}"),
    (None, None, A::Center, S::Plus, false, true) => write!(out, "{value:^+0X}"),
    (None, None, A::Center, S::Minus, false, false) => write!(out, "{value:^-X}"),
    (None, None, A::Center, S::Minus, false, true) => write!(out, "{value:^-0X}"),
    (None, None, A::Right, S::None, false, false) => write!(out, "{value:>X}"),
    (None, None, A::Right, S::None, false, true) => write!(out, "{value:>0X}"),
    (None, None, A::Right, S::Plus, false, false) => write!(out, "{value:>+X}"),
    (None, None, A::Right, S::Plus, false, true) => write!(out, "{value:>+0X}"),
    (None, None, A::Right, S::Minus, false, false) => write!(out, "{value:>-X}"),
    (None, None, A::Right, S::Minus, false, true) => write!(out, "{value:>-0X}"),
    (None, None, A::None, S::None, true, false) => write!(out, "{value:#X}"),
    (None, None, A::None, S::None, true, true) => write!(out, "{value:#0X}"),
    (None, None, A::None, S::Plus, true, false) => write!(out, "{value:+#X}"),
    (None, None, A::None, S::Plus, true, true) => write!(out, "{value:+#0X}"),
    (None, None, A::None, S::Minus, true, false) => write!(out, "{value:-#X}"),
    (None, None, A::None, S::Minus, true, true) => write!(out, "{value:-#0X}"),
    (None, None, A::Left, S::None, true, false) => write!(out, "{value:<#X}"),
    (None, None, A::Left, S::None, true, true) => write!(out, "{value:<#0X}"),
    (None, None, A::Left, S::Plus, true, false) => write!(out, "{value:<+#X}"),
    (None, None, A::Left, S::Plus, true, true) => write!(out, "{value:<+#0X}"),
    (None, None, A::Left, S::Minus, true, false) => write!(out, "{value:<-#X}"),
    (None, None, A::Left, S::Minus, true, true) => write!(out, "{value:<-#0X}"),
    (None, None, A::Center, S::None, true, false) => write!(out, "{value:^#X}"),
    (None, None, A::Center, S::None, true, true) => write!(out, "{value:^#0X}"),
    (None, None, A::Center, S::Plus, true, false) => write!(out, "{value:^+#X}"),
    (None, None, A::Center, S::Plus, true, true) => write!(out, "{value:^+#0X}"),
    (None, None, A::Center, S::Minus, true, false) => write!(out, "{value:^-#X}"),
    (None, None, A::Center, S::Minus, true, true) => write!(out, "{value:^-#0X}"),
    (None, None, A::Right, S::None, true, false) => write!(out, "{value:>#X}"),
    (None, None, A::Right, S::None, true, true) => write!(out, "{value:>#0X}"),
    (None, None, A::Right, S::Plus, true, false) => write!(out, "{value:>+#X}"),
    (None, None, A::Right, S::Plus, true, true) => write!(out, "{value:>+#0X}"),
    (None, None, A::Right, S::Minus, true, false) => write!(out, "{value:>-#X}"),
    (None, None, A::Right, S::Minus, true, true) => write!(out, "{value:>-#0X}"),
    (None, Some(pr), A::None, S::None, false, false) => write!(out, "{value:.pr$X}"),
    (None, Some(pr), A::None, S::None, false, true) => write!(out, "{value:0.pr$X}"),
    (None, Some(pr), A::None, S::Plus, false, false) => write!(out, "{value:+.pr$X}"),
    (None, Some(pr), A::None, S::Plus, false, true) => write!(out, "{value:+0.pr$X}"),
    (None, Some(pr), A::None, S::Minus, false, false) => write!(out, "{value:-.pr$X}"),
    (None, Some(pr), A::None, S::Minus, false, true) => write!(out, "{value:-0.pr$X}"),
    (None, Some(pr), A::Left, S::None, false, false) => write!(out, "{value:<.pr$X}"),
    (None, Some(pr), A::Left, S::None, false, true) => write!(out, "{value:<0.pr$X}"),
    (None, Some(pr), A::Left, S::Plus, false, false) => write!(out, "{value:<+.pr$X}"),
    (None, Some(pr), A::Left, S::Plus, false, true) => write!(out, "{value:<+0.pr$X}"),
    (None, Some(pr), A::Left, S::Minus, false, false) => write!(out, "{value:<-.pr$X}"),
    (None, Some(pr), A::Left, S::Minus, false, true) => write!(out, "{value:<-0.pr$X}"),
    (None, Some(pr), A::Center, S::None, false, false) => write!(out, "{value:^.pr$X}"),
    (None, Some(pr), A::Center, S::None, false, true) => write!(out, "{value:^0.pr$X}"),
    (None, Some(pr), A::Center, S::Plus, false, false) => write!(out, "{value:^+.pr$X}"),
    (None, Some(pr), A::Center, S::Plus, false, true) => write!(out, "{value:^+0.pr$X}"),
    (None, Some(pr), A::Center, S::Minus, false, false) => write!(out, "{value:^-.pr$X}"),
    (None, Some(pr), A::Center, S::Minus, false, true) => write!(out, "{value:^-0.pr$X}"),
    (None, Some(pr), A::Right, S::None, false, false) => write!(out, "{value:>.pr$X}"),
    (None, Some(pr), A::Right, S::None, false, true) => write!(out, "{value:>0.pr$X}"),
    (None, Some(pr), A::Right, S::Plus, false, false) => write!(out, "{value:>+.pr$X}"),
    (None, Some(pr), A::Right, S::Plus, false, true) => write!(out, "{value:>+0.pr$X}"),
    (None, Some(pr), A::Right, S::Minus, false, false) => write!(out, "{value:>-.pr$X}"),
    (None, Some(pr), A::Right, S::Minus, false, true) => write!(out, "{value:>-0.pr$X}"),
    (None, Some(pr), A::None, S::None, true, false) => write!(out, "{value:#.pr$X}"),
    (None, Some(pr), A::None, S::None, true, true) => write!(out, "{value:#0.pr$X}"),
    (None, Some(pr), A::None, S::Plus, true, false) => write!(out, "{value:+#.pr$X}"),
    (None, Some(pr), A::None, S::Plus, true, true) => write!(out, "{value:+#0.pr$X}"),
    (None, Some(pr), A::None, S::Minus, true, false) => write!(out, "{value:-#.pr$X}"),
    (None, Some(pr), A::None, S::Minus, true, true) => write!(out, "{value:-#0.pr$X}"),
    (None, Some(pr), A::Left, S::None, true, false) => write!(out, "{value:<#.pr$X}"),
    (None, Some(pr), A::Left, S::None, true, true) => write!(out, "{value:<#0.pr$X}"),
    (None, Some(pr), A::Left, S::Plus, true, false) => write!(out, "{value:<+#.pr$X}"),
    (None, Some(pr), A::Left, S::Plus, true, true) => write!(out, "{value:<+#0.pr$X}"),
    (None, Some(pr), A::Left, S::Minus, true, false) => write!(out, "{value:<-#.pr$X}"),
    (None, Some(pr), A::Left, S::Minus, true, true) => write!(out, "{value:<-#0.pr$X}"),
    (None, Some(pr), A::Center, S::None, true, false) => write!(out, "{value:^#.pr$X}"),
    (None, Some(pr), A::Center, S::None, true, true) => write!(out, "{value:^#0.pr$X}"),
    (None, Some(pr), A::Center, S::Plus, true, false) => write!(out, "{value:^+#.pr$X}"),
    (None, Some(pr), A::Center, S::Plus, true, true) => write!(out, "{value:^+#0.pr$X}"),
    (None, Some(pr), A::Center, S::Minus, true, false) => write!(out, "{value:^-#.pr$X}"),
    (None, Some(pr), A::Center, S::Minus, true, true) => write!(out, "{value:^-#0.pr$X}"),
    (None, Some(pr), A::Right, S::None, true, false) => write!(out, "{value:>#.pr$X}"),
    (None, Some(pr), A::Right, S::None, true, true) => write!(out, "{value:>#0.pr$X}"),
    (None, Some(pr), A::Right, S::Plus, true, false) => write!(out, "{value:>+#.pr$X}"),
    (None, Some(pr), A::Right, S::Plus, true, true) => write!(out, "{value:>+#0.pr$X}"),
    (None, Some(pr), A::Right, S::Minus, true, false) => write!(out, "{value:>-#.pr$X}"),
    (None, Some(pr), A::Right, S::Minus, true, true) => write!(out, "{value:>-#0.pr$X}"),
    (Some(wi), None, A::None, S::None, false, false) => write!(out, "{value:wi$X}"),
    (Some(wi), None, A::None, S::None, false, true) => write!(out, "{value:0wi$X}"),
    (Some(wi), None, A::None, S::Plus, false, false) => write!(out, "{value:+wi$X}"),
    (Some(wi), None, A::None, S::Plus, false, true) => write!(out, "{value:+0wi$X}"),
    (Some(wi), None, A::None, S::Minus, false, false) => write!(out, "{value:-wi$X}"),
    (Some(wi), None, A::None, S::Minus, false, true) => write!(out, "{value:-0wi$X}"),
    (Some(wi), None, A::Left, S::None, false, false) => write!(out, "{value:<wi$X}"),
    (Some(wi), None, A::Left, S::None, false, true) => write!(out, "{value:<0wi$X}"),
    (Some(wi), None, A::Left, S::Plus, false, false) => write!(out, "{value:<+wi$X}"),
    (Some(wi), None, A::Left, S::Plus, false, true) => write!(out, "{value:<+0wi$X}"),
    (Some(wi), None, A::Left, S::Minus, false, false) => write!(out, "{value:<-wi$X}"),
    (Some(wi), None, A::Left, S::Minus, false, true) => write!(out, "{value:<-0wi$X}"),
    (Some(wi), None, A::Center, S::None, false, false) => write!(out, "{value:^wi$X}"),
    (Some(wi), None, A::Center, S::None, false, true) => write!(out, "{value:^0wi$X}"),
    (Some(wi), None, A::Center, S::Plus, false, false) => write!(out, "{value:^+wi$X}"),
    (Some(wi), None, A::Center, S::Plus, false, true) => write!(out, "{value:^+0wi$X}"),
    (Some(wi), None, A::Center, S::Minus, false, false) => write!(out, "{value:^-wi$X}"),
    (Some(wi), None, A::Center, S::Minus, false, true) => write!(out, "{value:^-0wi$X}"),
    (Some(wi), None, A::Right, S::None, false, false) => write!(out, "{value:>wi$X}"),
    (Some(wi), None, A::Right, S::None, false, true) => write!(out, "{value:>0wi$X}"),
    (Some(wi), None, A::Right, S::Plus, false, false) => write!(out, "{value:>+wi$X}"),
    (Some(wi), None, A::Right, S::Plus, false, true) => write!(out, "{value:>+0wi$X}"),
    (Some(wi), None, A::Right, S::Minus, false, false) => write!(out, "{value:>-wi$X}"),
    (Some(wi), None, A::Right, S::Minus, false, true) => write!(out, "{value:>-0wi$X}"),
    (Some(wi), None, A::None, S::None, true, false) => write!(out, "{value:#wi$X}"),
    (Some(wi), None, A::None, S::None, true, true) => write!(out, "{value:#0wi$X}"),
    (Some(wi), None, A::None, S::Plus, true, false) => write!(out, "{value:+#wi$X}"),
    (Some(wi), None, A::None, S::Plus, true, true) => write!(out, "{value:+#0wi$X}"),
    (Some(wi), None, A::None, S::Minus, true, false) => write!(out, "{value:-#wi$X}"),
    (Some(wi), None, A::None, S::Minus, true, true) => write!(out, "{value:-#0wi$X}"),
    (Some(wi), None, A::Left, S::None, true, false) => write!(out, "{value:<#wi$X}"),
    (Some(wi), None, A::Left, S::None, true, true) => write!(out, "{value:<#0wi$X}"),
    (Some(wi), None, A::Left, S::Plus, true, false) => write!(out, "{value:<+#wi$X}"),
    (Some(wi), None, A::Left, S::Plus, true, true) => write!(out, "{value:<+#0wi$X}"),
    (Some(wi), None, A::Left, S::Minus, true, false) => write!(out, "{value:<-#wi$X}"),
    (Some(wi), None, A::Left, S::Minus, true, true) => write!(out, "{value:<-#0wi$X}"),
    (Some(wi), None, A::Center, S::None, true, false) => write!(out, "{value:^#wi$X}"),
    (Some(wi), None, A::Center, S::None, true, true) => write!(out, "{value:^#0wi$X}"),
    (Some(wi), None, A::Center, S::Plus, true, false) => write!(out, "{value:^+#wi$X}"),
    (Some(wi), None, A::Center, S::Plus, true, true) => write!(out, "{value:^+#0wi$X}"),
    (Some(wi), None, A::Center, S::Minus, true, false) => write!(out, "{value:^-#wi$X}"),
    (Some(wi), None, A::Center, S::Minus, true, true) => write!(out, "{value:^-#0wi$X}"),
    (Some(wi), None, A::Right, S::None, true, false) => write!(out, "{value:>#wi$X}"),
    (Some(wi), None, A::Right, S::None, true, true) => write!(out, "{value:>#0wi$X}"),
    (Some(wi), None, A::Right, S::Plus, true, false) => write!(out, "{value:>+#wi$X}"),
    (Some(wi), None, A::Right, S::Plus, true, true) => write!(out, "{value:>+#0wi$X}"),
    (Some(wi), None, A::Right, S::Minus, true, false) => write!(out, "{value:>-#wi$X}"),
    (Some(wi), None, A::Right, S::Minus, true, true) => write!(out, "{value:>-#0wi$X}"),
    (Some(wi), Some(pr), A::None, S::None, false, false) => write!(out, "{value:wi$.pr$X}"),
    (Some(wi), Some(pr), A::None, S::None, false, true) => write!(out, "{value:0wi$.pr$X}"),
    (Some(wi), Some(pr), A::None, S::Plus, false, false) => write!(out, "{value:+wi$.pr$X}"),
    (Some(wi), Some(pr), A::None, S::Plus, false, true) => write!(out, "{value:+0wi$.pr$X}"),
    (Some(wi), Some(pr), A::None, S::Minus, false, false) => write!(out, "{value:-wi$.pr$X}"),
    (Some(wi), Some(pr), A::None, S::Minus, false, true) => write!(out, "{value:-0wi$.pr$X}"),
    (Some(wi), Some(pr), A::Left, S::None, false, false) => write!(out, "{value:<wi$.pr$X}"),
    (Some(wi), Some(pr), A::Left, S::None, false, true) => write!(out, "{value:<0wi$.pr$X}"),
    (Some(wi), Some(pr), A::Left, S::Plus, false, false) => write!(out, "{value:<+wi$.pr$X}"),
    (Some(wi), Some(pr), A::Left, S::Plus, false, true) => write!(out, "{value:<+0wi$.pr$X}"),
    (Some(wi), Some(pr), A::Left, S::Minus, false, false) => write!(out, "{value:<-wi$.pr$X}"),
    (Some(wi), Some(pr), A::Left, S::Minus, false, true) => write!(out, "{value:<-0wi$.pr$X}"),
    (Some(wi), Some(pr), A::Center, S::None, false, false) => write!(out, "{value:^wi$.pr$X}"),
    (Some(wi), Some(pr), A::Center, S::None, false, true) => write!(out, "{value:^0wi$.pr$X}"),
    (Some(wi), Some(pr), A::Center, S::Plus, false, false) => write!(out, "{value:^+wi$.pr$X}"),
    (Some(wi), Some(pr), A::Center, S::Plus, false, true) => write!(out, "{value:^+0wi$.pr$X}"),
    (Some(wi), Some(pr), A::Center, S::Minus, false, false) => write!(out, "{value:^-wi$.pr$X}"),
    (Some(wi), Some(pr), A::Center, S::Minus, false, true) => write!(out, "{value:^-0wi$.pr$X}"),
    (Some(wi), Some(pr), A::Right, S::None, false, false) => write!(out, "{value:>wi$.pr$X}"),
    (Some(wi), Some(pr), A::Right, S::None, false, true) => write!(out, "{value:>0wi$.pr$X}"),
    (Some(wi), Some(pr), A::Right, S::Plus, false, false) => write!(out, "{value:>+wi$.pr$X}"),
    (Some(wi), Some(pr), A::Right, S::Plus, false, true) => write!(out, "{value:>+0wi$.pr$X}"),
    (Some(wi), Some(pr), A::Right, S::Minus, false, false) => write!(out, "{value:>-wi$.pr$X}"),
    (Some(wi), Some(pr), A::Right, S::Minus, false, true) => write!(out, "{value:>-0wi$.pr$X}"),
    (Some(wi), Some(pr), A::None, S::None, true, false) => write!(out, "{value:#wi$.pr$X}"),
    (Some(wi), Some(pr), A::None, S::None, true, true) => write!(out, "{value:#0wi$.pr$X}"),
    (Some(wi), Some(pr), A::None, S::Plus, true, false) => write!(out, "{value:+#wi$.pr$X}"),
    (Some(wi), Some(pr), A::None, S::Plus, true, true) => write!(out, "{value:+#0wi$.pr$X}"),
    (Some(wi), Some(pr), A::None, S::Minus, true, false) => write!(out, "{value:-#wi$.pr$X}"),
    (Some(wi), Some(pr), A::None, S::Minus, true, true) => write!(out, "{value:-#0wi$.pr$X}"),
    (Some(wi), Some(pr), A::Left, S::None, true, false) => write!(out, "{value:<#wi$.pr$X}"),
    (Some(wi), Some(pr), A::Left, S::None, true, true) => write!(out, "{value:<#0wi$.pr$X}"),
    (Some(wi), Some(pr), A::Left, S::Plus, true, false) => write!(out, "{value:<+#wi$.pr$X}"),
    (Some(wi), Some(pr), A::Left, S::Plus, true, true) => write!(out, "{value:<+#0wi$.pr$X}"),
    (Some(wi), Some(pr), A::Left, S::Minus, true, false) => write!(out, "{value:<-#wi$.pr$X}"),
    (Some(wi), Some(pr), A::Left, S::Minus, true, true) => write!(out, "{value:<-#0wi$.pr$X}"),
    (Some(wi), Some(pr), A::Center, S::None, true, false) => write!(out, "{value:^#wi$.pr$X}"),
    (Some(wi), Some(pr), A::Center, S::None, true, true) => write!(out, "{value:^#0wi$.pr$X}"),
    (Some(wi), Some(pr), A::Center, S::Plus, true, false) => write!(out, "{value:^+#wi$.pr$X}"),
    (Some(wi), Some(pr), A::Center, S::Plus, true, true) => write!(out, "{value:^+#0wi$.pr$X}"),
    (Some(wi), Some(pr), A::Center, S::Minus, true, false) => write!(out, "{value:^-#wi$.pr$X}"),
    (Some(wi), Some(pr), A::Center, S::Minus, true, true) => write!(out, "{value:^-#0wi$.pr$X}"),
    (Some(wi), Some(pr), A::Right, S::None, true, false) => write!(out, "{value:>#wi$.pr$X}"),
    (Some(wi), Some(pr), A::Right, S::None, true, true) => write!(out, "{value:>#0wi$.pr$X}"),
    (Some(wi), Some(pr), A::Right, S::Plus, true, false) => write!(out, "{value:>+#wi$.pr$X}"),
    (Some(wi), Some(pr), A::Right, S::Plus, true, true) => write!(out, "{value:>+#0wi$.pr$X}"),
    (Some(wi), Some(pr), A::Right, S::Minus, true, false) => write!(out, "{value:>-#wi$.pr$X}"),
    (Some(wi), Some(pr), A::Right, S::Minus, true, true) => write!(out, "{value:>-#0wi$.pr$X}"),
}
