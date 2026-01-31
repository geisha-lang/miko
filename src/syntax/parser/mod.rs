use std::cell::RefCell;
use crate::internal::*;
use crate::syntax::form::*;
use crate::utils::*;
use crate::types::*;

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
    Form::new(span, Expr::Binary(op, Box::new(lhs), Box::new(rhs)))
}

peg::parser! {
    pub grammar geisha_parser(interner: &RefCell<Interner>) for str {
        // Lexer helpers
        rule whitespace() = quiet!{[' ' | '\n' | '\r' | '\t']*}
        rule whitespace_inline() = quiet!{[' ' | '\t']*}

        rule keywords() = "def" / "if" / "else" / "let" / "in" / "type" / "data" / "match" / "concept" / "instance"

        rule lexeme<T>(x: rule<T>) -> T
            = whitespace() !keywords() t:x() { t }

        rule reserved<T>(x: rule<T>) -> T
            = whitespace() r:x() !identifier_char() { r }

        rule parens<T>(x: rule<T>) -> T
            = lexeme(<"(">) t:x() lexeme(<")">) { t }

        rule spanned<T>(x: rule<T>) -> (Span, T)
            = start:position!() p:x() end:position!() {
                let span = Span::new(start, end);
                (span, p)
            }

        rule identifier_char()
            = ['a'..='z' | 'A'..='Z' | '0'..='9' | '_']

        rule string_content() -> &'input str
            = $((!['"'] [_] / "\\\"")*)

        // Lex rules
        rule identifier() -> Id
            = id:lexeme(<$(['a'..='z' | 'A'..='Z' | '_'] identifier_char()*)>) {
                interner.borrow_mut().intern(id)
            }

        rule type_constant_identifier() -> &'input str
            = lexeme(<$(['A'..='Z'] identifier_char()*)>)

        rule type_variable_identifier() -> &'input str
            = lexeme(<$(['a'..='z'] identifier_char()*)>)

        rule integer() -> &'input str
            = lexeme(<$("-"?['1'..='9']['0'..='9']*)>)
            / lexeme(<$("0")>)

        rule float() -> &'input str
            = lexeme(<$("-"?['0'..='9']"."['0'..='9']+("e" "-"? ['1'..='9']['0'..='9']*)?)>)

        rule string() -> &'input str
            = lexeme(<"\"" s:string_content() "\"" {s}>)

        // Top level definition
        rule definition_delimite() = whitespace_inline() (";" / "\n" / "\r\n")

        pub rule module() -> Vec<Def>
            = d:definition()+ whitespace() { d }

        pub rule definition() -> Def
            = t:forward_declaration() definition_delimite()* d:spanned(<form_definition()>) {?
                let (forward_name, scm) = t;
                let (span, (name, mut form)) = d;
                if forward_name == name {
                    form.tag.annotate = Some(scm);
                    Ok(Def::value(span, name, Box::new(form)))
                } else {
                    Err("Forward declaration should be followed by it's definition")
                }
            }
            / d:spanned(<form_definition()>) {
                let (span, (name, form)) = d;
                Def::value(span, name, Box::new(form))
            }
            / type_definition()

        rule forward_declaration() -> (Id, Scheme)
            = n:identifier() lexeme(<":">) t:type_scheme() { (n, t) }

        rule form_definition() -> (Id, Form)
            = value_definition()
            / function_definition()

        rule value_definition() -> (Id, Form)
            = reserved(<"def">) name:identifier() lexeme(<"=">) value:expression() { (name, value) }

        rule function_definition() -> (Id, Form)
            = reserved(<"def">) name:identifier()
              lexeme(<"(">) ps:parameter_sequence() lexeme(<")">)
              lexeme(<"=">) body:expression() {
                  let span = body.tag.pos.clone();
                  let lambda = Expr::Abs(Lambda { param: ps, body: Box::new(body) });
                  (name, Form::new(span, lambda))
              }

        rule type_definition() -> Def
            = reserved(<"type">) a:spanned(<type_alias()>) {
                let (span, (name, item)) = a;
                Def {
                    ident: interner.borrow_mut().intern(name.as_str()),
                    pos: span,
                    node: item
                }
            }
            / reserved(<"data">) a:spanned(<type_algebra()>) {
                let (span, (name, item)) = a;
                Def {
                    ident: interner.borrow_mut().intern(name.as_str()),
                    pos: span,
                    node: item
                }
            }
            / reserved(<"concept">) a:spanned(<concept_definition()>) {
                let (span, (name, item)) = a;
                Def {
                    ident: interner.borrow_mut().intern(name.as_str()),
                    pos: span,
                    node: item
                }
            }
            / reserved(<"instance">) a:spanned(<instance_definition()>) {
                let (span, (name, item)) = a;
                Def {
                    ident: interner.borrow_mut().intern(name.as_str()),
                    pos: span,
                    node: item
                }
            }

        // Concept definition: concept Eq a { eq: a * a -> Bool }
        rule concept_definition() -> (String, Item)
            = n:type_constant_identifier() p:type_parameter_sequence()
              s:concept_superclasses()?
              lexeme(<"{">) m:concept_method() ** lexeme(<",">) lexeme(<"}">) {
                let params: Vec<Id> = p.iter().map(|s| interner.borrow_mut().intern(s)).collect();
                let superclasses = s.unwrap_or_default();
                (n.to_string(), Item::Concept {
                    type_params: params,
                    superclasses,
                    methods: m,
                })
            }

        rule concept_superclasses() -> Vec<TypeConstraint>
            = lexeme(<"(">) cs:type_constraint() ++ lexeme(<",">) lexeme(<")">) lexeme(<"=>">) { cs }

        rule type_constraint() -> TypeConstraint
            = c:type_constant_identifier() v:type_variable_identifier() {
                TypeConstraint {
                    concept: c.to_string(),
                    type_var: v.to_string(),
                }
            }

        rule concept_method() -> MethodDecl
            = n:identifier() lexeme(<":">) t:type_scheme() {
                MethodDecl { name: n, ty: t }
            }

        // Instance definition: instance Eq Int { def eq(x, y) = x == y }
        // or with constraints: instance (Eq a) => Eq (List a) { ... }
        rule instance_definition() -> (String, Item)
            = cs:instance_constraints()? cn:type_constant_identifier()
              tas:instance_type_args()
              lexeme(<"{">) ms:instance_method() ** lexeme(<",">) lexeme(<"}">) {
                let constraints = cs.unwrap_or_default();
                // Generate a unique name for the instance
                let instance_name = format!("instance_{}_{}", cn, tas.iter().map(|t| t.to_string()).collect::<Vec<_>>().join("_"));
                (instance_name, Item::Instance {
                    concept_name: cn.to_string(),
                    type_args: tas,
                    constraints,
                    methods: ms,
                })
            }

        rule instance_constraints() -> Vec<TypeConstraint>
            = lexeme(<"(">) cs:type_constraint() ++ lexeme(<",">) lexeme(<")">) lexeme(<"=>">) { cs }

        rule instance_type_args() -> Vec<Type>
            = ts:instance_type_arg()+ { ts }

        rule instance_type_arg() -> Type
            = parens(<type_expression()>)
            / t:type_terminal() { t }

        rule instance_method() -> MethodImpl
            = reserved(<"def">) n:identifier()
              lexeme(<"(">) ps:parameter_sequence() lexeme(<")">)
              lexeme(<"=">) body:expression() {
                let span = body.tag.pos.clone();
                let lambda = Expr::Abs(Lambda { param: ps, body: Box::new(body) });
                MethodImpl { name: n, body: Box::new(Form::new(span, lambda)) }
            }

        rule type_alias() -> (String, Item)
            = n:type_constant_identifier() p:type_parameter_sequence() lexeme(<"=">) a:type_scheme() {
                let param = p.into_iter().map(|s| interner.borrow_mut().intern(s)).collect();
                (n.to_string(), Item::Alias(param, Box::new(a)))
            }

        rule type_algebra() -> (String, Item)
            = n:type_constant_identifier() p:type_parameter_sequence()
              lexeme(<"{">) v:type_algebra_variant() ++ lexeme(<",">) lexeme(<"}">) {
                let param = p.into_iter().map(|s| interner.borrow_mut().intern(s)).collect();
                (n.to_string(), Item::Alg(param, v))
              }

        rule type_algebra_variant() -> Variant
            = n:identifier() ts:parens(<type_expression() ++ lexeme(<",">)>) {
                let body = ts.into_iter().enumerate().map(|(i, t)| Field { name: None, ty: Box::new(t) }).collect();
                Variant { name: interner.borrow().trace(n).to_string(), body: VariantBody::Tuple(body) }
            }
            / n:identifier() fs:struct_fields() {
                Variant { name: interner.borrow().trace(n).to_string(), body: VariantBody::Struct(fs) }
            }
            / n:identifier() {
                // Unit variant (no fields)
                Variant { name: interner.borrow().trace(n).to_string(), body: VariantBody::Unit }
            }

        rule struct_fields() -> Vec<Field>
            = lexeme(<"{">) fs:struct_field_declaration() ++ lexeme(<",">) lexeme(<"}">) { fs }

        rule struct_field_declaration() -> Field
            = n:identifier() lexeme(<":">) t:type_expression() {
                Field { name: Some(interner.borrow().trace(n).to_string()), ty: Box::new(t) }
            }

        // Type scheme
        rule type_parameter_sequence() -> Vec<&'input str>
            = type_variable_identifier() ** whitespace()

        rule type_constant() -> Type
            = n:type_constant_identifier() { Type::Con(n.to_string()) }

        rule type_variable() -> Type
            = n:type_variable_identifier() { Type::Var(n.to_string()) }

        rule type_factor() -> Type
            = v:type_composition() { Type::compose_n(v) }
            / parens(<type_expression()>)

        rule type_terminal() -> Type
            = type_constant() / type_variable()

        rule type_composition() -> Vec<Type>
            = type_terminal() ++ whitespace()

        pub rule type_expression() -> Type
            = precedence! {
                lhs:(@) lexeme(<"->">) rhs:@ { Type::Arr(Box::new(lhs), Box::new(rhs)) }
                --
                lhs:(@) lexeme(<"*">) rhs:@ { Type::Prod(Box::new(lhs), Box::new(rhs)) }
                --
                t:type_factor() { t }
            }

        rule type_binders() -> Vec<&'input str>
            = type_variable_identifier() ++ lexeme(<",">)

        pub rule type_scheme() -> Scheme
            = reserved(<"forall">) cs:scheme_constraints() lexeme(<".">) b:type_expression() {
                // forall (Eq a, Ord b). a * b -> Bool
                // Extract type variables from constraints
                let binders: Vec<String> = cs.iter().map(|c| c.type_var.clone()).collect();
                let constraints = cs.into_iter()
                    .map(|c| crate::types::SchemeConstraint {
                        concept: c.concept,
                        type_arg: c.type_var,
                    })
                    .collect();
                Scheme::PolyConstrained(binders, constraints, b)
            }
            / reserved(<"forall">) f:type_variable_identifier()* lexeme(<".">) b:type_expression() {
                let binders = f.into_iter().map(|s| s.to_string()).collect();
                Scheme::Poly(binders, b)
            }
            / b:type_expression() { Scheme::Mono(b) }

        rule scheme_constraints() -> Vec<TypeConstraint>
            = lexeme(<"(">) cs:scheme_constraint() ++ lexeme(<",">) lexeme(<")">) { cs }

        rule scheme_constraint() -> TypeConstraint
            = c:type_constant_identifier() v:type_variable_identifier() {
                TypeConstraint {
                    concept: c.to_string(),
                    type_var: v.to_string(),
                }
            }

        // Expression form
        rule spanned_form(e: rule<Expr>) -> Form
            = _e:spanned(<e()>) {
                let (s, expr) = _e;
                Form::new(s, expr)
            }

        pub rule expression() -> Form
            = precedence! {
                lhs:(@) lexeme(<"||">) rhs:@ { make_spanned_binexpr(BinOp::Or, lhs, rhs) }
                --
                lhs:(@) lexeme(<"&&">) rhs:@ { make_spanned_binexpr(BinOp::And, lhs, rhs) }
                --
                lhs:(@) lexeme(<"<=">) rhs:@ { make_spanned_binexpr(BinOp::Le, lhs, rhs) }
                lhs:(@) lexeme(<">=">) rhs:@ { make_spanned_binexpr(BinOp::Ge, lhs, rhs) }
                lhs:(@) lexeme(<"<">) rhs:@ { make_spanned_binexpr(BinOp::Lt, lhs, rhs) }
                lhs:(@) lexeme(<">">) rhs:@ { make_spanned_binexpr(BinOp::Gt, lhs, rhs) }
                lhs:(@) lexeme(<"==">) rhs:@ { make_spanned_binexpr(BinOp::Eq, lhs, rhs) }
                lhs:(@) lexeme(<"!=">) rhs:@ { make_spanned_binexpr(BinOp::Ne, lhs, rhs) }
                --
                lhs:(@) lexeme(<"+">) rhs:@ { make_spanned_binexpr(BinOp::Add, lhs, rhs) }
                lhs:(@) lexeme(<"-">) rhs:@ { make_spanned_binexpr(BinOp::Sub, lhs, rhs) }
                --
                lhs:(@) lexeme(<"*">) rhs:@ { make_spanned_binexpr(BinOp::Mul, lhs, rhs) }
                lhs:(@) lexeme(<"/">) rhs:@ { make_spanned_binexpr(BinOp::Div, lhs, rhs) }
                lhs:(@) lexeme(<"%">) rhs:@ { make_spanned_binexpr(BinOp::Rem, lhs, rhs) }
                --
                f:form_factor() { f }
            }

        rule form_factor() -> Form
            = start:position!() h:_form_factor() t:_apply()* {
                let mut f = h;
                for (end, app) in t {
                    let param = app.into_iter().map(|e| Box::new(e)).collect();
                    let span = Span::new(start, end);
                    f = Form::new(span, Expr::Apply(Box::new(f), param))
                }
                f
            }

        rule _form_factor() -> Form
            = spanned_form(<factor_expr()>) / parens(<expression()>)

        rule factor_expr() -> Expr
            = ifelse() / letin() / matchexpr() / lambda() / lit() / var() / list() / block()

        rule ifelse() -> Expr
            = reserved(<"if">) c:parens(<expression()>)
              t:expression() reserved(<"else">) f:expression() {
                  Expr::If(Box::new(c), Box::new(t), Box::new(f))
              }

        rule letin() -> Expr
            = reserved(<"let">) v:variable_declaration() lexeme(<"=">) val:expression()
              reserved(<"in">) e:expression() {
                Expr::Let(v, Box::new(val), Box::new(e))
            }

        // Match expression: match expr { pattern -> body, ... }
        rule matchexpr() -> Expr
            = reserved(<"match">) scrutinee:expression() lexeme(<"{">)
              arms:match_arm() ++ lexeme(<",">) lexeme(<"}">) {
                Expr::Match(Box::new(scrutinee), arms)
            }

        rule match_arm() -> MatchArm
            = p:pattern() g:match_guard()? lexeme(<"->">) b:expression() {
                MatchArm { pattern: p, guard: g, body: Box::new(b) }
            }

        rule match_guard() -> E
            = reserved(<"if">) cond:parens(<expression()>) { Box::new(cond) }

        // Pattern matching patterns
        rule pattern() -> Pattern
            = constructor_pattern()
            / literal_pattern()
            / wildcard_pattern()
            / variable_pattern()

        rule constructor_pattern() -> Pattern
            = n:constructor_identifier() ps:parens(<pattern() ** lexeme(<",">)>) {
                Pattern::Constructor(interner.borrow().trace(n).to_string(), ps)
            }
            / n:constructor_identifier() {
                // Unit constructor (no fields)
                Pattern::Constructor(interner.borrow().trace(n).to_string(), vec![])
            }

        rule constructor_identifier() -> Id
            = id:lexeme(<$(['A'..='Z'] identifier_char()*)>) {
                interner.borrow_mut().intern(id)
            }

        rule wildcard_pattern() -> Pattern
            = lexeme(<"_">) { Pattern::Wildcard }

        rule variable_pattern() -> Pattern
            = n:identifier() { Pattern::Var(n) }

        rule literal_pattern() -> Pattern
            = l:lit() {
                match l {
                    Expr::Lit(lit) => Pattern::Lit(lit),
                    _ => unreachable!()
                }
            }

        rule lambda() -> Expr
            = p:parens(<parameter_sequence()>) lexeme(<"->">) to:expression() {
                Expr::Abs(Lambda { param: p, body: Box::new(to) })
            }

        rule var() -> Expr
            = n:identifier() { Expr::Var(n) }

        rule list() -> Expr
            = lexeme(<"[">) l:expression() ** lexeme(<",">) lexeme(<"]">) {
                Expr::List(l.into_iter().map(|e| Box::new(e)).collect())
            }

        rule block() -> Expr
            = lexeme(<"{">) l:expression() ** lexeme(<",">) lexeme(<"}">) {
                Expr::Block(l.into_iter().map(|e| Box::new(e)).collect())
            }

        rule lit() -> Expr
            = float_lit() / integer_lit() / string_lit() / bool_lit()

        rule float_lit() -> Expr
            = f:float() {?
                f.parse::<f64>().map(|v| Expr::Lit(Lit::Float(v)))
                    .or(Err("Parse float failed"))
            }

        rule integer_lit() -> Expr
            = i:integer() {?
                i.parse::<i32>()
                    .map(|v| Expr::Lit(Lit::Int(v)))
                    .or_else(|_| {
                        i.parse::<f64>().map(|v| Expr::Lit(Lit::Float(v)))
                            .or(Err("Parse integer failed"))
                    })
            }

        rule string_lit() -> Expr
            = s:string() {?
                escape_string(s)
                    .map(|s| Expr::Lit(Lit::Str(s)))
                    .or(Err("Parse string failed"))
            }

        rule bool_lit() -> Expr
            = b:lexeme(<$("true" / "false")>) {
                Expr::Lit(Lit::Bool(match b {
                    "true" => true,
                    "false" => false,
                    _ => unreachable!()
                }))
            }

        rule _apply() -> (usize, Vec<Form>)
            = l:parens(<apply_list()>) end:position!() { (end, l) }

        rule apply_list() -> Vec<Form>
            = expression() ** lexeme(<",">)

        rule parameter_sequence() -> Vec<VarDecl>
            = variable_declaration() ** lexeme(<",">)

        rule variable_declaration() -> VarDecl
            = n:identifier() lexeme(<":">) t:type_expression() {
                VarDecl(n, Scheme::Mono(t))
            }
            / n:identifier() { VarDecl(n, Scheme::Slot) }
    }
}

pub use self::geisha_parser::*;

pub fn parse(src: &str, interner: &mut Interner) -> Result<Vec<Def>, peg::error::ParseError<peg::str::LineCol>> {
    let cell = RefCell::new(std::mem::take(interner));
    let result = module(src, &cell);
    *interner = cell.into_inner();
    result
}

pub fn parse_type(src: &str, interner: &mut Interner) -> Scheme {
    let cell = RefCell::new(std::mem::take(interner));
    let result = type_scheme(src, &cell);
    *interner = cell.into_inner();
    match result {
        Ok(scm) => scm,
        Err(err) => panic!("Parse type failed: {:?}", err)
    }
}

pub fn parse_expr(src: &str, interner: &mut Interner) -> Result<Form, peg::error::ParseError<peg::str::LineCol>> {
    let cell = RefCell::new(std::mem::take(interner));
    let result = expression(src, &cell);
    *interner = cell.into_inner();
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn case_parse_type_constant() {
        let cell = RefCell::new(Interner::new());
        assert_eq!(type_expression("  Fuck", &cell), Ok(Type::Con("Fuck".to_string())))
    }
    #[test]
    fn case_parse_type_variable() {
        let cell = RefCell::new(Interner::new());
        assert_eq!(type_expression("     shit", &cell), Ok(Type::Var("shit".to_string())))
    }
    #[test]
    fn case_parse_arrow_type() {
        let output = Type::Arr(
            Box::new(Type::Var("shit".to_string())),
            Box::new(Type::Con("Fuck".to_string()))
        );
        let cell = RefCell::new(Interner::new());
        assert_eq!(type_expression("     shit -> Fuck", &cell), Ok(output))
    }
    #[test]
    fn case_parse_composite_type() {
        let output = Type::Comp(
            Box::new(Type::Var("shit".to_string())),
            Box::new(Type::Con("Fuck".to_string()))
        );
        let cell = RefCell::new(Interner::new());
        assert_eq!(type_expression("     shit  Fuck", &cell), Ok(output))
    }
    #[test]
    fn case_parse_composite_and_arrow_type() {
        let output = Type::Arr(
            Box::new(Type::Comp(
                Box::new(Type::Var("shit".to_string())),
                Box::new(Type::Con("Fuck".to_string()))
            )),
            Box::new(Type::Var("a".to_string()))
        );
        let cell = RefCell::new(Interner::new());
        assert_eq!(type_expression("     shit  Fuck -> a", &cell), Ok(output))
    }
    #[test]
    fn case_parse_product_type() {
        let output = Type::Prod(
            Box::new(Type::Con("Shit".to_string())),
            Box::new(Type::Var("fuck".to_string()))
        );
        let cell = RefCell::new(Interner::new());
        assert_eq!(type_expression("     Shit * fuck", &cell), Ok(output))
    }
    #[test]
    fn case_parse_product_and_arrow_type() {
        let output = Type::Arr(
            Box::new(Type::Prod(
                Box::new(Type::Con("Shit".to_string())),
                Box::new(Type::Var("fuck".to_string()))
            )),
            Box::new(Type::Con("Fuck".to_string()))
        );
        let cell = RefCell::new(Interner::new());
        assert_eq!(type_expression("     Shit  *    fuck -> Fuck", &cell), Ok(output))
    }
    #[test]
    fn case_parse_product_with_arrow_type() {
        let output = Type::Prod(
            Box::new(Type::Con("Shit".to_string())),
            Box::new(Type::Arr(
                Box::new(Type::Var("fuck".to_string())),
                Box::new(Type::Con("Fuck".to_string()))
            ))
        );
        let cell = RefCell::new(Interner::new());
        assert_eq!(type_expression("     Shit  *  (  fuck -> Fuck)", &cell), Ok(output))
    }
    #[test]
    fn case_parse_arrow_and_product_type() {
        let output = Type::Arr(
            Box::new(Type::Con("Fuck".to_string())),
            Box::new(Type::Prod(
                Box::new(Type::Con("Shit".to_string())),
                Box::new(Type::Var("fuck".to_string()))
            ))
        );
        let cell = RefCell::new(Interner::new());
        assert_eq!(type_expression("   Fuck ->    Shit * fuck", &cell), Ok(output))
    }
    #[test]
    fn case_parse_mono_arrow_to_arrow_type() {
        // Arrow types are left-associative in our precedence grammar:
        // (Fuck -> Shit) -> fuck
        let output = Scheme::Mono(
            Type::Arr(
                Box::new(Type::Arr(
                    Box::new(Type::Con("Fuck".to_string())),
                    Box::new(Type::Con("Shit".to_string()))
                )),
                Box::new(Type::Var("fuck".to_string()))
            )
        );
        let cell = RefCell::new(Interner::new());
        assert_eq!(type_scheme("   Fuck ->    Shit -> fuck", &cell), Ok(output))
    }
    #[test]
    fn case_parse_poly_product_with_arrow_type() {
        let output = Scheme::Poly(
            vec!["fuck".to_string()],
            Type::Prod(
                Box::new(Type::Con("Shit".to_string())),
                Box::new(Type::Arr(
                    Box::new(Type::Var("fuck".to_string())),
                    Box::new(Type::Con("Fuck".to_string()))
                ))
            )
        );
        let cell = RefCell::new(Interner::new());
        assert_eq!(type_scheme("forall fuck.     Shit  *  (  fuck -> Fuck)", &cell), Ok(output))
    }

    #[test]
    fn case_parse_function_definition() {
        {
            let cell = RefCell::new(Interner::new());
            let result = module("def fuck(a, b) = a + b", &cell);
            assert!(result.is_ok());
        }
        {
            let cell = RefCell::new(Interner::new());
            let result = module("def fuck(a) = a + 1", &cell);
            assert!(result.is_ok());
        }
        {
            let src =
"
fuck: Int -> Int
def fuck(a) = a + 1

shit: forall a b. a * b -> a * b
def shit(p) = p

";
            let cell = RefCell::new(Interner::new());
            println!("{:?}", module(src, &cell));
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
        let cell = RefCell::new(Interner::new());
        println!("{:?}", module(src, &cell));
        assert!(true);
    }

    #[test]
    fn case_parse_adt_with_unit_variant() {
        let src = "
data Option a {
    None,
    Some(a)
}
";
        let cell = RefCell::new(Interner::new());
        let result = module(src, &cell);
        println!("{:?}", result);
        assert!(result.is_ok());
        let defs = result.unwrap();
        assert_eq!(defs.len(), 1);
    }

    #[test]
    fn case_parse_match_expression() {
        let src = "
data Option a {
    None,
    Some(a)
}

def unwrap(opt) = match opt {
    None -> 0,
    Some(x) -> x
}
";
        let cell = RefCell::new(Interner::new());
        let result = module(src, &cell);
        println!("{:?}", result);
        assert!(result.is_ok());
    }

    #[test]
    fn case_parse_match_with_literals() {
        let src = "
def check(x) = match x {
    0 -> 1,
    1 -> 2,
    n -> n + 1
}
";
        let cell = RefCell::new(Interner::new());
        let result = module(src, &cell);
        println!("{:?}", result);
        assert!(result.is_ok());
    }

    #[test]
    fn case_parse_match_with_wildcard() {
        let src = "
def foo(x) = match x {
    0 -> 1,
    _ -> 0
}
";
        let cell = RefCell::new(Interner::new());
        let result = module(src, &cell);
        println!("{:?}", result);
        assert!(result.is_ok());
    }

    #[test]
    fn case_parse_concept_definition() {
        let src = "
concept Eq a {
    eq: a * a -> Bool
}
";
        let cell = RefCell::new(Interner::new());
        let result = module(src, &cell);
        println!("{:?}", result);
        assert!(result.is_ok());
        let defs = result.unwrap();
        assert_eq!(defs.len(), 1);
    }

    #[test]
    fn case_parse_instance_definition() {
        let src = "
instance Eq Int {
    def eq(x, y) = x == y
}
";
        let cell = RefCell::new(Interner::new());
        let result = module(src, &cell);
        println!("{:?}", result);
        assert!(result.is_ok());
    }

    #[test]
    fn case_parse_instance_with_constraint() {
        let src = "
instance (Eq a) => Eq (List a) {
    def eq(xs, ys) = true
}
";
        let cell = RefCell::new(Interner::new());
        let result = module(src, &cell);
        println!("{:?}", result);
        assert!(result.is_ok());
    }

    #[test]
    fn case_parse_constrained_forall() {
        // Test constrained forall type scheme
        let cell = RefCell::new(Interner::new());
        let result = type_scheme("forall (Eq a). a * a -> Bool", &cell);
        println!("{:?}", result);
        assert!(result.is_ok());

        if let Ok(Scheme::PolyConstrained(vars, constraints, _body)) = result {
            assert_eq!(vars.len(), 1);
            assert_eq!(vars[0], "a");
            assert_eq!(constraints.len(), 1);
            assert_eq!(constraints[0].concept, "Eq");
            assert_eq!(constraints[0].type_arg, "a");
        } else {
            panic!("Expected PolyConstrained scheme");
        }
    }

    #[test]
    fn case_parse_constrained_forall_multiple() {
        // Test constrained forall with multiple constraints
        let cell = RefCell::new(Interner::new());
        let result = type_scheme("forall (Eq a, Ord b). a * b -> Bool", &cell);
        println!("{:?}", result);
        assert!(result.is_ok());

        if let Ok(Scheme::PolyConstrained(vars, constraints, _body)) = result {
            assert_eq!(vars.len(), 2);
            assert_eq!(constraints.len(), 2);
        } else {
            panic!("Expected PolyConstrained scheme");
        }
    }

}
