extern crate miko;
use miko::utils::*;
use miko::syntax::parser::*;
use miko::types::*;
use miko::typeinfer::*;
use miko::codegen::emit::*;
use miko::core::*;
use miko::internal::*;
use std::io;
use std::io::Write;
use std::ops::Deref;

fn repl() {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut input = String::new();

    let mut generator = LLVMEmit::new("repl");

    'main: loop {
        print!("> ");
        stdout.flush().unwrap();
        input.clear();
        stdin.read_line(&mut input).ok().expect("Failed to read line");
        if input.as_str() == ".quit\n" {
            break;
        }
        let mut res: Vec<_> = parse(input.as_str(), &mut Interner::new()).unwrap();
        println!("Parsed syntax tree:");
        println!("{:?}", res);

        let ty_op = Scheme::Poly(vec!["a".to_string()],
                                 Type::Arr(P(Type::Prod(P(Type::Var("a".to_string())),
                                                        P(Type::Var("a".to_string())))),
                                           P(Type::Var("a".to_string()))));


        let mut ty_infer = infer::Infer::new();
        let mut ty_env = infer::Infer::new_env();

        ty_env.insert(String::from("+"), ty_op);

        let inf = ty_infer.infer_defs(&ty_env, &mut res);
        match inf {
            Ok(_) => {
                // let env = types.unwrap();
                println!("Typed AST:");
                println!("{:?}", res);
                println!("Core term:");
                let (top, _) = K::go(res);
                println!("{:?}", top);
                println!("LLVM IR:");
                for def in top.values() {
                    generator.gen_top_level(def.deref(), &VarEnv::new());
                }
                generator.dump();
            }
            Err(e) => println!("{:?}", e),
        }


    }
}

fn main() {
    repl();
}