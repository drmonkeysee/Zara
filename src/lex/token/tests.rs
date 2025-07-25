use super::*;

mod token {
    use super::*;
    use crate::testutil::ok_or_fail;

    #[test]
    fn display_bytevector() {
        let token = Token {
            kind: TokenKind::ByteVector,
            span: 0..4,
        };

        assert_eq!(token.to_string(), "BYTEVECTOR[0..4]");
    }

    #[test]
    fn display_comment() {
        let token = Token {
            kind: TokenKind::Comment,
            span: 0..5,
        };

        assert_eq!(token.to_string(), "COMMENT[0..5]");
    }

    #[test]
    fn display_comment_blockbegin() {
        let token = Token {
            kind: TokenKind::CommentBlockBegin { depth: 1 },
            span: 0..10,
        };

        assert_eq!(token.to_string(), "COMMENTBEGIN<1>[0..10]");
    }

    #[test]
    fn display_comment_blockfragment() {
        let token = Token {
            kind: TokenKind::CommentBlockFragment { depth: 1 },
            span: 0..10,
        };

        assert_eq!(token.to_string(), "COMMENTFRAGMENT<1>[0..10]");
    }

    #[test]
    fn display_comment_blockend() {
        let token = Token {
            kind: TokenKind::CommentBlockEnd,
            span: 0..10,
        };

        assert_eq!(token.to_string(), "COMMENTEND[0..10]");
    }

    #[test]
    fn display_datum_comment() {
        let token = Token {
            kind: TokenKind::CommentDatum,
            span: 0..2,
        };

        assert_eq!(token.to_string(), "DATUMCOMMENT[0..2]");
    }

    #[test]
    fn display_directive() {
        let token = Token {
            kind: TokenKind::DirectiveCase(false),
            span: 0..10,
        };

        assert_eq!(token.to_string(), "FOLDCASE<false>[0..10]");
    }

    #[test]
    fn bool_token() {
        let token = Token {
            kind: TokenKind::Boolean(false),
            span: 0..2,
        };

        assert_eq!(token.to_string(), "BOOL[0..2]");
    }

    #[test]
    fn char_token() {
        let token = Token {
            kind: TokenKind::Character('a'),
            span: 0..3,
        };

        assert_eq!(token.to_string(), "CHAR[0..3]");
    }

    #[test]
    fn string_token() {
        let token = Token {
            kind: TokenKind::String("foo".to_owned()),
            span: 0..5,
        };

        assert_eq!(token.to_string(), "STR[0..5]");
    }

    #[test]
    fn integer_token() {
        let token = Token {
            kind: TokenKind::Number(Number::real(42)),
            span: 0..5,
        };

        assert_eq!(token.to_string(), "NUM<INT>[0..5]");
    }

    #[test]
    fn float_token() {
        let token = Token {
            kind: TokenKind::Number(Number::real(4.2)),
            span: 0..5,
        };

        assert_eq!(token.to_string(), "NUM<FLT>[0..5]");
    }

    #[test]
    fn rational_token() {
        let r = ok_or_fail!(Real::reduce(4, 5));
        let token = Token {
            kind: TokenKind::Number(Number::real(r)),
            span: 0..5,
        };

        assert_eq!(token.to_string(), "NUM<RAT>[0..5]");
    }

    #[test]
    fn complex_token() {
        let token = Token {
            kind: TokenKind::Number(Number::complex(3, 5)),
            span: 0..5,
        };

        assert_eq!(token.to_string(), "NUM<CPX>[0..5]");
    }

    #[test]
    fn display_label_definition() {
        let token = Token {
            kind: TokenKind::LabelDef("12345".to_owned()),
            span: 0..7,
        };

        assert_eq!(token.to_string(), "LABELDEF[0..7]");
    }

    #[test]
    fn display_label_reference() {
        let token = Token {
            kind: TokenKind::LabelRef("12345".to_owned()),
            span: 0..7,
        };

        assert_eq!(token.to_string(), "LABELREF[0..7]");
    }

    #[test]
    fn display_paren_left() {
        let token = Token {
            kind: TokenKind::ParenLeft,
            span: 0..1,
        };

        assert_eq!(token.to_string(), "LEFTPAREN[0..1]");
    }

    #[test]
    fn display_paren_right() {
        let token = Token {
            kind: TokenKind::ParenRight,
            span: 0..1,
        };

        assert_eq!(token.to_string(), "RIGHTPAREN[0..1]");
    }

    #[test]
    fn display_pair_joiner() {
        let token = Token {
            kind: TokenKind::PairJoiner,
            span: 0..1,
        };

        assert_eq!(token.to_string(), "PAIR[0..1]");
    }

    #[test]
    fn display_quasiquote() {
        let token = Token {
            kind: TokenKind::Quasiquote,
            span: 0..1,
        };

        assert_eq!(token.to_string(), "QUASIQUOTE[0..1]");
    }

    #[test]
    fn display_quote() {
        let token = Token {
            kind: TokenKind::Quote,
            span: 0..1,
        };

        assert_eq!(token.to_string(), "QUOTE[0..1]");
    }

    #[test]
    fn display_string_begin() {
        let token = Token {
            kind: TokenKind::StringBegin {
                s: "foo".to_owned(),
                line_cont: false,
            },
            span: 0..1,
        };

        assert_eq!(token.to_string(), "STRBEGIN[0..1]");
    }

    #[test]
    fn display_string_begin_with_line_continuation() {
        let token = Token {
            kind: TokenKind::StringBegin {
                s: "foo\\".to_owned(),
                line_cont: true,
            },
            span: 0..1,
        };

        assert_eq!(token.to_string(), "STRBEGIN<\\>[0..1]");
    }

    #[test]
    fn display_string_fragment() {
        let token = Token {
            kind: TokenKind::StringFragment {
                s: "foo".to_owned(),
                line_cont: false,
            },
            span: 0..1,
        };

        assert_eq!(token.to_string(), "STRFRAGMENT[0..1]");
    }

    #[test]
    fn display_string_fragment_with_line_continuation() {
        let token = Token {
            kind: TokenKind::StringFragment {
                s: "foo \\".to_owned(),
                line_cont: true,
            },
            span: 0..1,
        };

        assert_eq!(token.to_string(), "STRFRAGMENT<\\>[0..1]");
    }

    #[test]
    fn display_string_end() {
        let token = Token {
            kind: TokenKind::StringEnd("foo".to_owned()),
            span: 0..1,
        };

        assert_eq!(token.to_string(), "STREND[0..1]");
    }

    #[test]
    fn display_string_discard() {
        let token = Token {
            kind: TokenKind::StringDiscard,
            span: 0..1,
        };

        assert_eq!(token.to_string(), "STRDISCARD[0..1]");
    }

    #[test]
    fn display_unquote() {
        let token = Token {
            kind: TokenKind::Unquote,
            span: 0..1,
        };

        assert_eq!(token.to_string(), "UNQUOTE[0..1]");
    }

    #[test]
    fn display_unquote_splice() {
        let token = Token {
            kind: TokenKind::UnquoteSplice,
            span: 0..2,
        };

        assert_eq!(token.to_string(), "UNQUOTESPLICE[0..2]");
    }

    #[test]
    fn display_vector() {
        let token = Token {
            kind: TokenKind::Vector,
            span: 0..2,
        };

        assert_eq!(token.to_string(), "VECTOR[0..2]");
    }

    #[test]
    fn display_identifier() {
        let token = Token {
            kind: TokenKind::Identifier("foo".to_owned()),
            span: 0..3,
        };

        assert_eq!(token.to_string(), "IDENTIFIER[0..3]");
    }

    #[test]
    fn display_identifier_start() {
        let token = Token {
            kind: TokenKind::IdentifierBegin("foo".to_owned()),
            span: 0..3,
        };

        assert_eq!(token.to_string(), "IDENTBEGIN[0..3]");
    }

    #[test]
    fn display_identifier_fragment() {
        let token = Token {
            kind: TokenKind::IdentifierFragment("foo".to_owned()),
            span: 0..3,
        };

        assert_eq!(token.to_string(), "IDENTFRAGMENT[0..3]");
    }

    #[test]
    fn display_identifier_end() {
        let token = Token {
            kind: TokenKind::IdentifierEnd("foo".to_owned()),
            span: 0..3,
        };

        assert_eq!(token.to_string(), "IDENTEND[0..3]");
    }

    #[test]
    fn display_identifier_discard() {
        let token = Token {
            kind: TokenKind::IdentifierDiscard,
            span: 0..3,
        };

        assert_eq!(token.to_string(), "IDENTDISCARD[0..3]");
    }

    #[test]
    fn display_imaginary() {
        let token = Token {
            kind: TokenKind::Imaginary(4.into()),
            span: 0..3,
        };

        assert_eq!(token.to_string(), "IMAGINARY<INT>[0..3]");
    }

    #[test]
    fn no_token_continuation() {
        let kind = TokenKind::Quote;

        assert!(kind.to_continuation().is_none());
    }

    #[test]
    fn block_comment_open_continuation() {
        let kind = TokenKind::CommentBlockBegin { depth: 2 };

        assert!(matches!(
            kind.to_continuation(),
            Some(TokenContinuation::BlockComment { depth: 2 })
        ));
    }

    #[test]
    fn block_comment_fragment_continuation() {
        let kind = TokenKind::CommentBlockFragment { depth: 2 };

        assert!(matches!(
            kind.to_continuation(),
            Some(TokenContinuation::BlockComment { depth: 2 })
        ));
    }

    #[test]
    fn string_open_continuation() {
        let kind = TokenKind::StringBegin {
            s: "".to_owned(),
            line_cont: false,
        };

        assert!(matches!(
            kind.to_continuation(),
            Some(TokenContinuation::StringLiteral { line_cont: false })
        ));
    }

    #[test]
    fn string_fragment_continuation() {
        let kind = TokenKind::StringFragment {
            s: "".to_owned(),
            line_cont: true,
        };

        assert!(matches!(
            kind.to_continuation(),
            Some(TokenContinuation::StringLiteral { line_cont: true })
        ));
    }

    #[test]
    fn identifier_open_continuation() {
        let kind = TokenKind::IdentifierBegin("".to_owned());

        assert!(matches!(
            kind.to_continuation(),
            Some(TokenContinuation::VerbatimIdentifier)
        ));
    }

    #[test]
    fn identifier_fragment_continuation() {
        let kind = TokenKind::IdentifierFragment("".to_owned());

        assert!(matches!(
            kind.to_continuation(),
            Some(TokenContinuation::VerbatimIdentifier)
        ));
    }
}

mod tokenerror {
    use super::*;

    #[test]
    fn display_expected_boolean() {
        let err = TokenError {
            kind: TokenErrorKind::BooleanExpected(true),
            span: 0..1,
        };

        assert_eq!(err.to_string(), "expected boolean literal: true");
    }

    #[test]
    fn display_expected_bytevector() {
        let err = TokenError {
            kind: TokenErrorKind::ByteVectorExpected,
            span: 0..1,
        };

        assert_eq!(err.to_string(), "expected bytevector literal: #u8(…)");
    }

    #[test]
    fn display_expected_character() {
        let err = TokenError {
            kind: TokenErrorKind::CharacterExpected,
            span: 0..1,
        };

        assert_eq!(err.to_string(), "expected character literal");
    }

    #[test]
    fn display_expected_character_hex() {
        let err = TokenError {
            kind: TokenErrorKind::CharacterExpectedHex,
            span: 0..1,
        };

        assert_eq!(err.to_string(), "expected character hex-sequence");
    }

    #[test]
    fn display_invalid_character_hex() {
        let err = TokenError {
            kind: TokenErrorKind::CharacterInvalidHex,
            span: 0..1,
        };

        assert_eq!(
            err.to_string(),
            "character hex-sequence out of valid range: [0x0, 0x10ffff]"
        );
    }

    #[test]
    fn display_expected_directive() {
        let err = TokenError {
            kind: TokenErrorKind::DirectiveExpected,
            span: 0..1,
        };

        assert_eq!(
            err.to_string(),
            "expected directive: fold-case or no-fold-case"
        );
    }

    #[test]
    fn display_invalid_directive() {
        let err = TokenError {
            kind: TokenErrorKind::DirectiveInvalid,
            span: 0..1,
        };

        assert_eq!(
            err.to_string(),
            "unsupported directive: expected fold-case or no-fold-case"
        );
    }

    #[test]
    fn display_invalid_escape() {
        let cases = [
            TokenErrorKind::IdentifierEscapeInvalid { at: 1, ch: 'B' },
            TokenErrorKind::StringEscapeInvalid { at: 1, ch: 'B' },
        ];
        for case in cases {
            let err = TokenError {
                kind: case,
                span: 0..1,
            };

            assert_eq!(err.to_string(), "invalid escape sequence: \\B");
        }
    }

    #[test]
    fn display_expected_string_hex() {
        let cases = [
            TokenErrorKind::IdentifierExpectedHex { at: 1 },
            TokenErrorKind::StringExpectedHex { at: 1 },
        ];
        for case in cases {
            let err = TokenError {
                kind: case,
                span: 0..1,
            };

            assert_eq!(err.to_string(), "expected hex-escape");
        }
    }

    #[test]
    fn display_invalid_string_hex() {
        let cases = [
            TokenErrorKind::IdentifierInvalidHex { at: 1 },
            TokenErrorKind::StringInvalidHex { at: 1 },
        ];
        for case in cases {
            let err = TokenError {
                kind: case,
                span: 0..1,
            };

            assert_eq!(
                err.to_string(),
                "hex-escape out of valid range: [0x0, 0x10ffff]"
            );
        }
    }

    #[test]
    fn display_unterminated_string_hex() {
        let cases = [
            TokenErrorKind::IdentifierUnterminatedHex { at: 1 },
            TokenErrorKind::StringUnterminatedHex { at: 1 },
        ];
        for case in cases {
            let err = TokenError {
                kind: case,
                span: 0..1,
            };

            assert_eq!(err.to_string(), "unterminated hex-escape");
        }
    }

    #[test]
    fn display_invalid_hash() {
        let err = TokenError {
            kind: TokenErrorKind::HashInvalid,
            span: 0..1,
        };

        assert_eq!(err.to_string(), "invalid #-literal");
    }

    #[test]
    fn display_unterminated_hash() {
        let err = TokenError {
            kind: TokenErrorKind::HashUnterminated,
            span: 0..1,
        };

        assert_eq!(err.to_string(), "unterminated #-literal");
    }

    #[test]
    fn no_continuation() {
        let kind = TokenErrorKind::CharacterExpected;

        assert!(kind.to_continuation().is_none());
    }

    #[test]
    fn string_invalid_sequence_continuation() {
        let kind = TokenErrorKind::StringEscapeInvalid { at: 0, ch: 'c' };

        assert!(matches!(
            kind.to_continuation(),
            Some(TokenContinuation::SubstringError)
        ));
    }

    #[test]
    fn string_hex_continuation() {
        let kind = TokenErrorKind::StringExpectedHex { at: 1 };

        assert!(matches!(
            kind.to_continuation(),
            Some(TokenContinuation::SubstringError)
        ));
    }

    #[test]
    fn identifier_invalid_sequence_continuation() {
        let kind = TokenErrorKind::IdentifierEscapeInvalid { at: 0, ch: 'c' };

        assert!(matches!(
            kind.to_continuation(),
            Some(TokenContinuation::SubidentifierError)
        ));
    }

    #[test]
    fn identifier_hex_continuation() {
        let kind = TokenErrorKind::IdentifierExpectedHex { at: 1 };

        assert!(matches!(
            kind.to_continuation(),
            Some(TokenContinuation::SubidentifierError)
        ));
    }

    #[test]
    fn no_sub_index() {
        let kind = TokenErrorKind::CharacterExpected;

        assert!(kind.sub_idx().is_none());
    }

    #[test]
    fn invalid_sequence_sub_index() {
        let cases = [
            TokenErrorKind::IdentifierEscapeInvalid { at: 3, ch: 'c' },
            TokenErrorKind::StringEscapeInvalid { at: 3, ch: 'c' },
        ];
        for case in cases {
            assert!(matches!(case.sub_idx(), Some(3)));
        }
    }

    #[test]
    fn hex_sub_index() {
        let cases = [
            TokenErrorKind::IdentifierExpectedHex { at: 3 },
            TokenErrorKind::StringExpectedHex { at: 3 },
        ];
        for case in cases {
            assert!(matches!(case.sub_idx(), Some(3)));
        }
    }

    #[test]
    fn number_sub_index() {
        let cases = [
            TokenErrorKind::NumberInvalidDecimalPoint {
                at: 3,
                radix: "foo",
            },
            TokenErrorKind::NumberInvalidExponent {
                at: 3,
                radix: "foo",
            },
            TokenErrorKind::NumberUnexpectedDecimalPoint { at: 3 },
        ];
        for case in cases {
            assert!(matches!(case.sub_idx(), Some(3)));
        }
    }

    #[test]
    fn display_block_comment_unterminated() {
        let err = TokenError {
            kind: TokenErrorKind::BlockCommentUnterminated,
            span: 0..1,
        };

        assert_eq!(err.to_string(), "unterminated block comment");
    }

    #[test]
    fn display_string_unterminated() {
        let err = TokenError {
            kind: TokenErrorKind::StringUnterminated,
            span: 0..1,
        };

        assert_eq!(err.to_string(), "unterminated string literal");
    }

    #[test]
    fn display_invalid_identifier() {
        let err = TokenError {
            kind: TokenErrorKind::IdentifierInvalid('{'),
            span: 0..1,
        };

        assert_eq!(err.to_string(), "invalid identifier character: {");
    }

    #[test]
    fn display_identifier_unterminated() {
        let err = TokenError {
            kind: TokenErrorKind::IdentifierUnterminated,
            span: 0..1,
        };

        assert_eq!(err.to_string(), "unterminated verbatim identifier");
    }

    #[test]
    fn display_label_invalid() {
        let err = TokenError {
            kind: TokenErrorKind::LabelInvalid,
            span: 0..1,
        };

        assert_eq!(
            err.to_string(),
            "invalid datum label: only digits [0, 9] allowed"
        );
    }

    #[test]
    fn display_label_unterminated() {
        let err = TokenError {
            kind: TokenErrorKind::LabelUnterminated,
            span: 0..1,
        };

        assert_eq!(err.to_string(), "unterminated datum label");
    }

    #[test]
    fn display_number_expected() {
        let err = TokenError {
            kind: TokenErrorKind::NumberExpected,
            span: 0..1,
        };

        assert_eq!(err.to_string(), "expected numeric literal");
    }

    #[test]
    fn display_number_invalid() {
        let err = TokenError {
            kind: TokenErrorKind::NumberInvalid,
            span: 0..1,
        };

        assert_eq!(err.to_string(), "invalid numeric literal");
    }

    #[test]
    fn display_number_invalid_decimal() {
        let err = TokenError {
            kind: TokenErrorKind::NumberInvalidDecimalPoint {
                at: 1,
                radix: "foo",
            },
            span: 0..1,
        };

        assert_eq!(
            err.to_string(),
            "foo radix does not support decimal notation"
        );
    }

    #[test]
    fn display_number_invalid_exponent() {
        let err = TokenError {
            kind: TokenErrorKind::NumberInvalidExponent {
                at: 1,
                radix: "foo",
            },
            span: 0..1,
        };

        assert_eq!(
            err.to_string(),
            "foo radix does not support scientific notation"
        );
    }

    #[test]
    fn display_number_unexpected_decimal() {
        let err = TokenError {
            kind: TokenErrorKind::NumberUnexpectedDecimalPoint { at: 1 },
            span: 0..1,
        };

        assert_eq!(err.to_string(), "unexpected decimal point");
    }

    #[test]
    fn display_imaginary_missing_sign() {
        let err = TokenError {
            kind: TokenErrorKind::ImaginaryMissingSign,
            span: 0..1,
        };

        assert_eq!(err.to_string(), "missing explicit sign on imaginary number");
    }

    #[test]
    fn display_imaginary_malformed() {
        let err = TokenError {
            kind: TokenErrorKind::ImaginaryInvalid,
            span: 0..1,
        };

        assert_eq!(err.to_string(), "invalid imaginary literal");
    }

    #[test]
    fn display_rational_invalid() {
        let err = TokenError {
            kind: TokenErrorKind::RationalInvalid,
            span: 0..1,
        };

        assert_eq!(err.to_string(), "invalid rational literal");
    }

    #[test]
    fn display_complex_invalid() {
        let err = TokenError {
            kind: TokenErrorKind::ComplexInvalid,
            span: 0..1,
        };

        assert_eq!(err.to_string(), "invalid complex literal");
    }

    #[test]
    fn display_polar_invalid() {
        let err = TokenError {
            kind: TokenErrorKind::PolarInvalid,
            span: 0..1,
        };

        assert_eq!(err.to_string(), "invalid polar literal");
    }

    #[test]
    fn display_numeric_error() {
        let err = TokenError {
            kind: NumericError::DivideByZero.into(),
            span: 0..1,
        };

        assert_eq!(err.to_string(), "numeric error - divide by zero");
    }

    #[test]
    fn display_expected_exactness() {
        let err = TokenError {
            kind: TokenErrorKind::ExactnessExpected { at: 0 },
            span: 0..1,
        };

        assert_eq!(err.to_string(), "expected exactness prefix, one of: #e #i");
    }

    #[test]
    fn display_expected_radix() {
        let err = TokenError {
            kind: TokenErrorKind::RadixExpected { at: 0 },
            span: 0..1,
        };

        assert_eq!(
            err.to_string(),
            "expected radix prefix, one of: #b #o #d #x"
        );
    }
}

mod tokencontinuation {
    use super::*;

    #[test]
    fn block_comment() {
        let cont = TokenContinuation::BlockComment { depth: 10 };

        assert!(matches!(
            cont.into(),
            TokenErrorKind::BlockCommentUnterminated
        ));
    }

    #[test]
    fn string_fragment() {
        let cont = TokenContinuation::StringLiteral { line_cont: false };

        assert!(matches!(cont.into(), TokenErrorKind::StringUnterminated));
    }

    #[test]
    fn string_error() {
        let cont = TokenContinuation::SubstringError;

        assert!(matches!(cont.into(), TokenErrorKind::StringUnterminated));
    }

    #[test]
    fn identifier_fragment() {
        let cont = TokenContinuation::VerbatimIdentifier;

        assert!(matches!(
            cont.into(),
            TokenErrorKind::IdentifierUnterminated
        ));
    }

    #[test]
    fn identifier_error() {
        let cont = TokenContinuation::SubidentifierError;

        assert!(matches!(
            cont.into(),
            TokenErrorKind::IdentifierUnterminated
        ));
    }
}
