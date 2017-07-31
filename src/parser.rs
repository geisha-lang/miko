use pest::*;

use syntax::*;
use types::*;
use utils::*;
use std::str::FromStr;
use std::collections::LinkedList;
use std::borrow::BorrowMut;

fn form(e: Expr) -> P<Form> {
    P(Form::new(Pos::new(0, 0), e))
}

impl_rdp! {
    grammar! {
        type_anno = { (["forall"] ~ type_bounds ~ ["."])? ~ type_expr }
        type_expr = {
            { type_factor }
            fn_type = {< type_arr }
            prod_type = {< type_prod }
        }
        type_factor = { ["("] ~ type_expr ~ [")"] | type_con | type_var }
        type_con = @{ ['A'..'Z'] ~ alphabet }
        type_var = @{ ['a'..'z'] ~ alphabet }
        type_arr = { ["->"] }
        type_prod = { ["*"] }

        type_bounds = { type_var ~ ([","] ~ type_var)* }

        name = @{
            (['a'..'z'] | ['A'..'Z'] | ["_"]) ~ alphabet
        }
        alphabet = @{
            (['a'..'z'] | ['A'..'Z'] | ["_"] | ["$"] | ['0'..'9'])*
        }

        var_decl = { name ~ ([":"] ~ type_anno)? }
        assign = { var_decl ~ ["="] ~ expr }


        bind = { ["def"] ~ assign }

        data_def = { ["data"] ~ type_con ~ ["{"] ~ data_variants ~ ["}"] }
        data_variants = { data_variant ~ ([","] ~ data_variant)* }
        data_variant = { name ~ (struct_varint | tuple_varint) }
        struct_varint = { ["{"] ~ struct_fields ~ ["}"] }
        struct_fields = { var_decl ~ ([","] ~ var_decl)* }
        tuple_varint = { ["("] ~ tuple_fields ~ [")"] }
        tuple_fields = { type_expr ~ ([","] ~ type_expr)* }

        type_alias = { ["type"] ~ type_con ~ ["="] ~ type_factor }

        expr = {
            { factor }
            or         = { op_or }
            and        = { op_and }
            comparison = { op_comparison }
            list_op    = {< op_list }
            add_sub    = { op_add_sub }
            mul_div    = { op_mul_div }
        }


        op_or          = { ["||"] }
        op_and         = { ["&&"] }
        op_comparison  = { ["<="] | [">="] | ["<"] | [">"] | ["!="] | ["=="] }
        op_list        = { ["::"] | ["++"] }
        op_add_sub     = { ["+"] | ["-"] }
        op_mul_div     = { ["*"] | ["/"] | ["%"] }
        op_unary       = { ["-"] }


        factor  = { factor_ ~ apply_ }
        factor_ = {
            ifelse | letin | lambda | lit | var | list | block | 
            op_unary ~ factor |
            ["("] ~ expr ~ [")"]
        }
        apply_  = { (["("] ~ exprlist? ~ [")"])* }

        lambda  = { ["("] ~ param_decls? ~ [")"] ~ ["->"] ~ expr }
        param_decls = { var_decl ~ ([","] ~ var_decl)* }

        lit = @{ string | float | number | boolean }
        string = { ["\""] ~ string_raw ~ ["\""] }
        string_raw = { (escape_sequence | !(["\""] | ["\\"]) ~ any)* }
        escape_sequence =  {
            ["\\"] ~ (["a"] | ["b"] | ["f"] | ["n"] | ["r"] | ["t"] | ["v"] | ["z"] | ["\""] |
                      ["'"] | ["\\"]) |
            ["\\"] ~ ["\r"]? ~ ["\n"] 
        }
        float = { ['0'..'9']+ ~ ["."] ~ ['0'..'9']+ }
        number = { ['0'..'9']+ }
        boolean = { ["true"] | ["false"] }

        var = { name }
        list = { ["["] ~ exprlist? ~ ["]"] }
        block = { ["{"] ~ (expr ~ ([";"] ~ expr)*)? ~ ["}"] }
        // apply = { factor ~ ["("] ~ exprlist? ~ [")"] }
        letin = { ["let"] ~ assign ~ ([","] ~ assign)* ~ ["in"] ~ expr }
        ifelse = { ["if"] ~ ["("] ~ expr ~ [")"] ~ expr ~ ["else"] ~ expr }


        exprlist = { expr ~ ([","] ~ expr)* }

        whitespace = _{ [" "] | ["\t"] | ["\u{000C}"] | ["\r"] | ["\n"] }
    }

    process! {

        // Just for test
        _type_expr(&self) -> P<Type> {
            (_: type_expr, ty: _type()) => ty
        }

        _type(&self) -> P<Type> {
            (&type_con: type_con, _: alphabet) => P(Type::Con(type_con.to_string())),
            (&type_var: type_var, _: alphabet) => P(Type::Var(type_var.to_string())),
            (_: type_factor, t: _type()) => t,
            (_: fn_type, left: _type(), _: type_arr, right: _type()) => P(Type::Arr(left, right)),
            (_: prod_type, left: _type(), _: type_prod, right: _type()) => P(Type::Prod(left, right)),
        }
        _type_anno(&self) -> Scheme {
            (_: type_bounds, tvs: _bound_tv(), _: type_expr, ty: _type()) => {
                Scheme::Poly(tvs, ty)
            },
            (_: type_expr, ty: _type()) => Scheme::Mono(ty),
        }
        _bound_tv(&self) -> Vec<Name> {
            (&tv: type_var, mut tail: _bound_tv()) => {
                tail.push(tv.to_string());
                tail
            },
            () => Vec::new()
        }

        __bind(&self) -> P<Def> {
            (_: bind, bd: _bind()) => bd
        }
        _bind(&self) -> P<Def> {
            (_: assign, ass: _assign()) => {
                let (VarDecl(name, ty), mut val) = ass;
                val.tag.annotate = Some(ty);
                // f.settype(ty);
                Def::value(Pos::new(0,0), name, val)
            }
        }


        _lit(&self) -> Lit {
            (&boolean: boolean) => Lit::Bool(match boolean {
                "true" => true,
                "false" => false,
                _ => unreachable!()
            }),
            (&float: float) => Lit::Float(f64::from_str(float).unwrap()),
            (&number: number) => Lit::Int(i32::from_str(number).unwrap()),
            (_: string, &string: string_raw) => Lit::Str(string.to_string()),
        }

        // Reversed list
        _exprlist(&self) -> Vec< P<Form> > {
            (_: expr, head: _expr(), mut tail: _exprlist()) => {
                // unimplemented!()
                tail.push(head);
                tail
            },
            () => Vec::new()
        }

        _factor(&self) -> P<Form> {
            (head: _factor_(), applist: _apply_()) => {
                let mut node = head;
                for app in applist {
                    node = form(Expr::Apply(node, app));
                }
                node
            }
        }

        _apply_(&self) -> LinkedList< Vec< P<Form> > > {
            (_: exprlist, head: _exprlist(), mut tail: _apply_()) => {
                tail.push_front(head);
                tail
            },
            () => LinkedList::new()
        }
        _factor_(&self) -> P<Form> {
            (_: lit, val: _lit()) => form(Expr::Lit(val)),
            (_: list, mut list: _exprlist()) => form(Expr::List({
                list.reverse();
                list
            })),
            (_: ifelse, cond: __expr(), tr: __expr(), fl: __expr()) => {
                form(Expr::If(cond, tr, fl))
            },
            (_: var, &n: name, _: alphabet) => form(Expr::Var(n.to_string())),
            (_: letin, seq: _assigns(), t: __expr()) => {
                let mut that = t;
                for (var, val) in seq {
                    that = form(Expr::Let(var, val, that));
                }
                that
            },
            (_: block, mut ct: _exprlist()) => {
                ct.reverse();
                form(Expr::Block(ct))
            },
            (_: lambda, _: param_decls, mut from: _params(), to: __expr()) => {
                from.reverse();
                form(Expr::Abs(Lambda {
                    param: from,
                    body: to
                }))
            },
        }

        _params(&self) -> Vec<VarDecl> {
            (_: var_decl, head: _var_decl(), mut tail: _params()) => {
                tail.push(head);
                tail
            },
            () => Vec::new(),
        }

        // Just for test
        __expr(&self) -> P<Form> {
            (_: expr, then: _expr()) => then,
        }
        _expr(&self) -> P<Form> {
            (_: or, left: _expr(), _, right: _expr()) => {
                form(Expr::Binary(
                        BinOp::Or,
                        left,
                        right))
            },
            (_: and, left: _expr(), _, right: _expr()) => {
                form(Expr::Binary(
                        BinOp::And,
                        left,
                        right))
            },
            (_: add_sub, left: _expr(), &op, right: _expr()) => {
                form(Expr::Binary(
                        BinOp::take(op),
                        left,
                        right))
            },
            (_: mul_div, left: _expr(), &op, right: _expr()) => {
                form(Expr::Binary(
                        BinOp::take(op),
                        left,
                        right))
            },
            (_: comparison, left: _expr(), &op, right: _expr()) => {
                form(Expr::Binary(
                        BinOp::take(op),
                        left,
                        right))
            },
            (_: factor, _: factor_, f: _factor()) => f,
        }

        _assign(&self) -> (VarDecl, P<Form>) {
            (_: var_decl, t: _var_decl(), s: __expr()) => (t, s),
        }
        _var_decl(&self) -> VarDecl {
            (&n: name, _: alphabet, _: type_anno, ty: _type_anno()) => VarDecl(n.to_string(), ty),
            (&n: name, _: alphabet) => VarDecl(n.to_string(), Scheme::slot()),
        }

        // Reversed list
        _assigns(&self) -> LinkedList< (VarDecl, P<Form>) > {
            (_: assign, head: _assign(), mut tail: _assigns()) => {
                tail.push_back(head);
                tail
            },
            () => LinkedList::new()
        }
    }
}


#[cfg(test)]
mod tests {
    use parser::*;
    use syntax::*;
    use utils::*;

    macro_rules! test_non_terminal {
        ($input: expr, $non_terminal: ident, $rule: ident, $expected: expr) => (
            let mut parser = Rdp::new(StringInput::new($input));
            assert!(parser.$non_terminal());
            assert_eq!(parser.$rule(), $expected);
        )
    }
    macro_rules! test_non_terminal_dump {
        ($input: expr, $non_terminal: ident, $rule: ident, $expected: expr) => (
            let mut parser = Rdp::new(StringInput::new($input));
            parser.$non_terminal();
            println!("{:?}", parser.queue());
            assert_eq!(parser.$rule(), $expected);
        )
    }


    fn s(src: &'static str) -> String {
        String::from(src)
    }

    #[test]
    fn parse_type_con() {
        test_non_terminal!("Fuck", type_con, _type, P(Type::Con(String::from("Fuck"))));
    }

    #[test]
    fn parse_type_var() {
        test_non_terminal!("fuck", type_var, _type, P(Type::Var(String::from("fuck"))));
    }

    #[test]
    fn parse_type_expr() {
        test_non_terminal!("shit * Fuck -> Shit", type_expr, _type_expr,
            P(Type::Arr(
                P(Type::Prod(
                    P(Type::Var(String::from("shit"))),
                    P(Type::Con(String::from("Fuck")))
                )),
                P(Type::Con(String::from("Shit"))))));
    }

    #[test]
    fn parse_lit() {
        test_non_terminal!("true", boolean, _lit, (Lit::Bool(true)));
        test_non_terminal!("12345", number, _lit, (Lit::Int(12345)));
        test_non_terminal!("12345.123", float, _lit, (Lit::Float(12345.123)));
        test_non_terminal!("\"deep dark fantasy\"", string, _lit, (Lit::Str(String::from("deep dark fantasy"))));
    }

    #[test]
    fn parse_factor() {
        test_non_terminal!(
            "true", lit, _factor,
            form(Expr::Lit(Lit::Bool(true))));
        test_non_terminal!(
            "fuck", var, _factor,
            form(Expr::Var(s("fuck"))));
    }

    #[test]
    fn parse_expr() {
        use self::BinOp::*;
        use self::Expr::*;
        test_non_terminal!(
            "a * 3 + 1.2 / fuck * 4",
            expr, __expr,
            form(Binary(
                Add,
                form(Binary(
                    Mul,
                    form(Var(s("a"))),
                    form(Lit(self::Lit::Int(3))))),
                form(Binary(
                    Mul,
                    form(Binary(
                        Div,
                        form(Lit(self::Lit::Float(1.2))),
                        form(Var(s("fuck")))
                    )),
                    form(Lit(self::Lit::Int(4)))
                )))));
        test_non_terminal!(
            "let a: Fuck = shit in a",
            expr, __expr,
            form(Let(
                VarDecl(s("a"), Scheme::Mono(P(Type::Con(s("Fuck"))))),
                form(Var(s("shit"))),
                form(Var(s("a")))
            ))
        );
        test_non_terminal!(
            "(a: Fuck, b) -> let c = a in {
                c + b
            }",
            expr, __expr,
            form(Abs(Lambda {
                param: vec![VarDecl(s("a"), Scheme::con("Fuck")), VarDecl(s("b"), Scheme::slot())],
                body: form(Let(
                    VarDecl(s("c"), Scheme::slot()),
                    form(Var(s("a"))),
                    form(Block(
                        vec![
                            form(Binary(
                                Add,
                                form(Var(s("c"))),
                                form(Var(s("b")))
                            ))
                        ]
                    ))
                ))
            }))
        );
    }

    #[test]
    fn parse_fun_def() {
        use self::BinOp::*;
        use self::Expr::*;
        use self::Type::*;
        let ty1 = Scheme::Mono(P(
            Arr(
                P(Prod(
                    P(Con(s("Fuck"))),
                    P(Con(s("Fuck")))
                )),
                P(Con(s("Fuck")))
            )
        ));
        test_non_terminal!(
            "def shit: Fuck * Fuck -> Fuck = (a: Fuck, b) -> let c = a in {
                c + b
            }",
            bind, __bind,
            Def::value(
                Pos::new(0,0), "shit",
                P(Form::annotated(Pos::new(0,0), ty1, Abs(Lambda {
                    param: vec![VarDecl(s("a"), Scheme::con("Fuck")), VarDecl(s("b"), Scheme::slot())],
                    body: form(Let(
                        VarDecl(s("c"), Scheme::slot()),
                        form(Expr::Var(s("a"))),
                        form(Block(
                            vec![
                                form(Binary(
                                    Add,
                                    form(Expr::Var(s("c"))),
                                    form(Expr::Var(s("b")))
                                ))
                            ]
                        ))
                    ))
                })))
            )
        );
    }
}
