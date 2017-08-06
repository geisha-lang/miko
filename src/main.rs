extern crate rl;
use rl::utils::*;
use rl::syntax::*;
use rl::parser::*;
use rl::types::*;
use rl::typeinfer::*;
use rl::codegen::llvm::*;

use std::io;
use std::io::Write;
use std::ops::Deref;
use std::collections::HashMap;
fn repl() {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut input = String::new();

    let mut generator = LLVMCodegen::new("repl");

    'main: loop {
        print!("> ");
        stdout.flush().unwrap();
        input.clear();
        stdin.read_line(&mut input).ok().expect("Failed to read line");
        if input.as_str() == ".quit\n" {
            break;
        }
        let mut res: Vec<_> = parse(input.as_str()).into_iter().collect();
        println!("Parsed syntax tree:");
        println!("{:?}", res);

        let ty_op = Scheme::Poly(vec!["a".to_string()],
                                 P(Type::Arr(P(Type::Prod(P(Type::Var("a".to_string())),
                                                          P(Type::Var("a".to_string())))),
                                             P(Type::Var("a".to_string())))));

        
        let mut ty_infer = infer::Infer::new();
        let mut ty_env = infer::Infer::new_env();

        ty_env.insert(String::from("+"), ty_op);

        let inf = ty_infer.infer_defs(&ty_env, &mut res);
        match inf {
            Ok(_) => {
                println!("Typed AST:");
                println!("{:?}", res);
                println!("LLVM IR:");
                for def in res {
                    generator.gen_top_level(def.deref(), &VarEnv::new());
                }
                generator.dump();
            },
            Err(e) => println!("{:?}", e)
        }


    }
}

fn main() {
    // println!("{:?}", type_term(Span::new("(a) * ASDBCa * Fuck * Shit * c")));
    // println!("{:?}", type_factor(Span::new("shit")));
    // println!("{:?}", type_factor(Span::new("Fuck")));
    // println!("{:?}", type_expr(Span::new("a")));
    repl();
}