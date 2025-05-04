pub(crate) fn is_initial(ch: char) -> bool {
    // TODO: disallow Mc, Me, Nd
    is_letter(ch) || is_special_initial(ch)
}

pub(crate) fn is_standard(ch: char) -> bool {
    is_initial(ch) || is_digit(ch) || is_peculiar_initial(ch)
}

pub(crate) fn is_peculiar_initial(ch: char) -> bool {
    matches!(ch, '+' | '-' | '.')
}

fn is_letter(ch: char) -> bool {
    // TODO: support Lu, Ll, Lt, Lm, Lo, Mn, Mc, Me, Nd, Nl, No, Pd, Pc, Po, Sc, Sm, Sk, So, Co, U+200C, U+200D
    ch.is_ascii_alphabetic()
}

fn is_digit(ch: char) -> bool {
    // TODO: support Nd, Nl, No
    ch.is_ascii_digit()
}

fn is_special_initial(ch: char) -> bool {
    matches!(
        ch,
        '!' | '$' | '%' | '&' | '*' | '/' | ':' | '<' | '=' | '>' | '?' | '@' | '^' | '_' | '~'
    )
}
