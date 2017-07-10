// #![feature(trace_macros)]
// use std::iter::FromIterator;
// use std::str::from_utf8;

// use nom::*;
// use nom::IResult::*;
// use nom_locate::LocatedSpan;

// use syntax::*;

// pub type Span<'a> = LocatedSpan<&'a str>;

// fn make_span(s: &Span) -> Pos {
//     Pos::new(s.line as usize, s.get_column_utf8().unwrap())
// }


// macro_rules! regex (
//     ($input:expr, $regex:expr) => (
//         {
//             use nom::Slice;
//             use nom;

//             regex_bytes!(RE, $regex);

//             if let Some(first_match) = RE.find($input.fragment.as_bytes()) {
//                 nom::IResult::Done($input.slice(first_match.end()..), $input.slice(first_match.start()..first_match.end()))
//             } else {
//                 let output: nom::IResult<_, _> = nom::IResult::Error(error_code!(nom::ErrorKind::RegexpFind));

//                 output
//             }
//         }
//     )
// );

// /// type_decl := ":" type_expr
// named!(type_decl( Span ) -> Scheme, do_parse!(
//     tag!(":") >>
//     t: type_expr >>
//     (Scheme::Mono(P(t)))
// ));

// /// type_expr := type_term ["->" type_expr]
// named!(pub type_expr( Span ) -> Type, alt_complete!(
//     do_parse!(
//         t: type_term >>
//         tag!("->") >>
//         e: type_expr >>
//         ({
//             println!("{:?}", t);
//             println!("{:?}", e);
//             Type::Arr(P(t), P(e))
//         }))
//     | type_term));

// /// type_term := type_factor ["*"" type_term]
// named!(pub type_term( Span ) -> Type, alt_complete!(
//     do_parse!(
//         f: type_factor >>
//         tag!("*") >>
//         t: type_term >>
//         ({
//             println!("{:?}", f);
//             println!("{:?}", t);
//             Type::Prod(P(f), P(t))}))
//     | type_factor));

// /// type_factor := type_con | type_var | "(" type_expr ")"
// named!(pub type_factor( Span ) -> Type, alt_complete!(
//     type_con | type_var | delimited!(
//         tag!("("),
//         type_expr,
//         tag!(")")) ));

// /// type_var := [a-z]\w*
// named!(type_var( Span ) -> Type, ws!(do_parse!(
//     t: regex!(r"([a-z][$_a-zA-Z\d]*)") >>
//     // t: ws!(alphanumeric) >>
//     (Type::Var(t.to_string()))
// )));

// /// type_con := [A-Z]\w*
// named!(type_con( Span ) -> Type, ws!(do_parse!(
//     t: regex!(r"([A-Z][$_a-zA-Z\d]*)") >>
//     // t: ws!(alphanumeric) >>
//     (Type::Con(t.to_string()))
// )));


// /// lambda := ["(" [params] ")"] "->" expr
// named!(lambda( Span ) -> Form, do_parse!(
//     pos: position!() >>
//     ps: delimited!(tag!("("), alt_complete!(lambda_param | empty_params), tag!(")")) >>
//     ws!(tag!("->")) >>
//     res: expr >>
//     (Form::abs(make_span(&pos), ps, res))
// ));

// fn empty_params<T>(i: T) -> IResult<T, Vec<VarDecl>> {
//     IResult::Done(i, vec![])
// }
// named!(lambda_param( Span ) -> Vec<VarDecl>, separated_list!(tag!(","), var_decl));

// /// var_decl := ident [type_decl]
// named!(var_decl( Span ) -> VarDecl, do_parse!(
//     name: ident >>
//     ty: opt!(type_decl) >>
//     (VarDecl(name, match ty {
//         Some(scm) => scm,
//         None => Scheme::slot(),
//     }))
// ));

// /// var := [a-zA-Z$_]\w*
// named!(var( Span ) -> Form, do_parse!(
//     pos: position!() >>
//     id: ident >>
//     (Form::new(make_span(&pos), Expr::Var(id)))
// ));

// named!(ident( Span ) -> String, ws!(do_parse!(
//     id: regex!(r"[a-zA-Z$_]\w*") >>
//     (id.to_string())
// )));

// named!(expr( Span ) -> Form, alt_complete!( var ));


// #[cfg(test)]
// mod tests {
//     use nom::*;
//     use nom::IResult::*;
//     use nom_locate::LocatedSpan;

//     use syntax::*;
//     use parser::*;

//     #[test]
//     fn test_parse_type_con() {
//         assert_eq!(
//             type_con(Span::new("Shit")).unwrap().1,
//             Type::Con(String::from("Shit"))
//         );
//     }

//     #[test]
//     fn test_parse_type_var() {
//         assert_eq!(
//             type_var(Span::new("shit")).unwrap().1,
//             Type::Var(String::from("shit"))
//         );
//     }

//     #[test]
//     fn test_parse_type_factor() {
//         assert_eq!(
//             type_factor(Span::new("(shit)")).unwrap().1,
//             Type::Var(String::from("shit"))
//         )
//     }

//     #[test]
//     fn test_parse_type_term() {
//         assert_eq!(
//             type_term(Span::new("shit * Fuck")).unwrap().1,
//             Type::Prod(
//                 P(Type::Var(String::from("shit"))),
//                 P(Type::Con(String::from("Fuck")))
//             )
//         );
//         assert_eq!(
//             type_term(Span::new("shit * Fuck * a")).unwrap().1,
//             Type::Prod(
//                 P(Type::Var(String::from("shit"))),
//                 P(Type::Prod(
//                     P(Type::Con(String::from("Fuck"))),
//                     P(Type::Var(String::from("a"))),
//                 ))
//             )
//         );
//     }

//     #[test]
//     fn test_parse_type_expr() {
//         assert_eq!(
//             type_expr(Span::new("shit * Fuck -> Shit")).unwrap().1,
//             Type::Arr(
//                 P(Type::Prod(
//                     P(Type::Var(String::from("shit"))),
//                     P(Type::Con(String::from("Fuck")))
//                 )),
//                 P(Type::Con(String::from("Shit")))));
//     }

//     #[test]
//     fn test_parse_type_decl() {
//         let res = type_decl(Span::new(": fuck -> Shit"));
//         // println!("{:?}", res);
//         assert_eq!(
//             res.unwrap().1,
//             Scheme::Mono(P(Type::Arr(P(Type::Var(String::from("fuck"))), P(Type::Con(String::from("Shit")))))));
//     }

//     #[test]
//     fn test_parse_lambda() {
//         assert_eq!(
//             lambda(Span::new("() -> id")).unwrap().1,
//             Form::new(Pos::new(1, 1), Expr::Abs(P(Lambda {
//                 param: vec![],
//                 body: P(Form::new(Pos::new(1, 7), Expr::Var(String::from("id"))))
//             })))
//         );
//     }
// }
