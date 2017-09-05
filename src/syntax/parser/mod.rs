use internal::*;
use syntax::form::*;
use utils::*;

fn escape_string(s: &str) -> Result<String, &str> {
    let mut cs = s.chars();
    let mut r = String::new();
    while let Some(ch) = cs.next() {
        match ch {
            '\\' => {
                if let Some(n) = cs.next() {
                    match n {
                        'n' => r.push('\n'),
                        'r' => r.push('\r'),
                        't' => r.push('\t'),
                        _ => return Err("Invalid escape in string")
                    }
                } else {
                    return Err("Unexpected end of string")
                }
            },
            _ => r.push(ch)
        }
    }
    Ok(r)
}

fn make_spanned_binexpr(op: BinOp, lhs: Form, rhs: Form) -> Form {
    let span = lhs.tag.pos.union(&rhs.tag.pos);
    Form::new(span, Expr::Binary(op, box lhs, box rhs))
}

mod grammar {
    include!(concat!(env!("OUT_DIR"), "/grammar.rs"));
}

pub use self::grammar::*;
// pub use self::grammar::ParseError;

pub fn parse(src: &str, interner: &mut Interner) -> Result<Vec<Def>, ParseError> {
    module(src, interner)
}

#[cfg(test)]
mod tests {
    use super::*;
    use types::*;
    use syntax::form::*;
    use internal::*;
    use utils::*;
    // #[test]
    // fn case_lexer_identifier() {
    //     assert_eq!(identifier("  __fuck", &mut Interner::new()), Ok("__fuck"));
    //     assert_eq!(identifier("    __fuck1234", &mut Interner::new()), Ok("__fuck1234"));
    // }
    #[test]
    fn case_parse_type_constant() {
        assert_eq!(type_expression("  Fuck", &mut Interner::new()), Ok(Type::Con("Fuck".to_string())))
    }
    #[test]
    fn case_parse_type_variable() {
        assert_eq!(type_expression("     shit", &mut Interner::new()), Ok(Type::Var("shit".to_string())))
    }
    #[test]
    fn case_parse_arrow_type() {
        let output = Type::Arr(
            box Type::Var("shit".to_string()),
            box Type::Con("Fuck".to_string())
        );
        assert_eq!(type_expression("     shit -> Fuck", &mut Interner::new()), Ok(output))
    }
    #[test]
    fn case_parse_composite_type() {
        let output = Type::Comp(
            box Type::Var("shit".to_string()),
            box Type::Con("Fuck".to_string())
        );
        assert_eq!(type_expression("     shit  Fuck", &mut Interner::new()), Ok(output))
    }
    #[test]
    fn case_parse_composite_and_arrow_type() {
        let output = Type::Arr(
            box Type::Comp(
                box Type::Var("shit".to_string()),
                box Type::Con("Fuck".to_string())
            ),
            box Type::Var("a".to_string())
        );
        assert_eq!(type_expression("     shit  Fuck -> a", &mut Interner::new()), Ok(output))
    }
    #[test]
    fn case_parse_product_type() {
        let output = Type::Prod(
            box Type::Con("Shit".to_string()),
            box Type::Var("fuck".to_string())
        );
        assert_eq!(type_expression("     Shit * fuck", &mut Interner::new()), Ok(output))
    }
    #[test]
    fn case_parse_product_and_arrow_type() {
        let output = Type::Arr(
            box Type::Prod(
                box Type::Con("Shit".to_string()),
                box Type::Var("fuck".to_string())
            ),
            box Type::Con("Fuck".to_string())
        );
        assert_eq!(type_expression("     Shit  *    fuck -> Fuck", &mut Interner::new()), Ok(output))
    }
    #[test]
    fn case_parse_product_with_arrow_type() {
        let output = Type::Prod(
            box Type::Con("Shit".to_string()),
            box Type::Arr(
                box Type::Var("fuck".to_string()),
                box Type::Con("Fuck".to_string())
            )
        );
        assert_eq!(type_expression("     Shit  *  (  fuck -> Fuck)", &mut Interner::new()), Ok(output))
    }
    #[test]
    fn case_parse_arrow_and_product_type() {
        let output = Type::Arr(
            box Type::Con("Fuck".to_string()),
            box Type::Prod(
                box Type::Con("Shit".to_string()),
                box Type::Var("fuck".to_string())
            )
        );
        assert_eq!(type_expression("   Fuck ->    Shit * fuck", &mut Interner::new()), Ok(output))
    }
    #[test]
    fn case_parse_mono_arrow_to_arrow_type() {
        let output = Scheme::Mono(
            Type::Arr(
                box Type::Con("Fuck".to_string()),
                box Type::Arr(
                    box Type::Con("Shit".to_string()),
                    box Type::Var("fuck".to_string())
                )
            )
        );
        assert_eq!(type_scheme("   Fuck ->    Shit -> fuck", &mut Interner::new()), Ok(output))
    }
    #[test]
    fn case_parse_poly_product_with_arrow_type() {
        let output = Scheme::Poly(
            vec!["fuck".to_string()],
            Type::Prod(
                box Type::Con("Shit".to_string()),
                box Type::Arr(
                    box Type::Var("fuck".to_string()),
                    box Type::Con("Fuck".to_string())
                )
            )
        );
        assert_eq!(type_scheme("forall fuck.     Shit  *  (  fuck -> Fuck)", &mut Interner::new()), Ok(output))
    }

    #[test]
    fn case_parse_function_definition() {
        {
            let form = Form::new(Span::new(16, 22), Expr::Abs(
                Lambda {
                    param: vec![VarDecl("a".to_string(), Scheme::Slot), VarDecl("b".to_string(), Scheme::Slot)],
                    body: box Form::new(Span::new(16,22), Expr::Binary(
                        BinOp::Add,
                        box Form::new(Span::new(16,18), Expr::Var("a".to_string())),
                        box Form::new(Span::new(20,22), Expr::Var("b".to_string()))
                    ))
                }
            ));
            let output = vec![Def::value(Span::new(0,22), "fuck".to_string(), box form)];
            assert_eq!(module("def fuck(a, b) = a + b", &mut Interner::new()), Ok(output))
        }
        {
            let form = Form::new(Span::new(13, 19), Expr::Abs(
                Lambda {
                    param: vec![VarDecl("a".to_string(), Scheme::Slot)],
                    body: box Form::new(Span::new(13,19), Expr::Binary(
                        BinOp::Add,
                        box Form::new(Span::new(13,15), Expr::Var("a".to_string())),
                        box Form::new(Span::new(17,19), Expr::Lit(Lit::Int(1)))
                    ))
                }
            ));
            let output = vec![Def::value(Span::new(0,19), "fuck".to_string(), box form)];
            assert_eq!(module("def fuck(a) = a + 1", &mut Interner::new()), Ok(output))
        }
        {
            let src = 
"
fuck: Int -> Int
def fuck(a) = a + 1

shit: forall a b. a * b -> a * b
def shit(p) = p

";
            // let output = vec![Def::value(Span::new(0,19), "fuck".to_string(), box form)];
            println!("{:?}", module(src, &mut Interner::new()));
            assert!(true)
        }
    }

    #[test]
    fn case_parse_type_definition() {
        let src = "
data Fuck a {
    Fucker(Int, Float),
    Shit {
        mf: Int,
        next: Fuck
    }
}

type S b = Fuck b
";
        println!("{:?}", module(src, &mut Interner::new()));
        assert!(true);
    }

}
