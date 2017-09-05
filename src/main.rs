extern crate miko;
use miko::utils::*;
use miko::syntax::parser::*;
use miko::typeinfer::*;
use miko::codegen::emit::*;
use miko::core::*;
use std::io;
use std::io::Write;
use std::ops::Deref;

fn repl() {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut input = String::new();

    let mut interner = Interner::new();

    let ty_op = parse_type("forall a. a * a -> a", &mut interner);

    let mut _ty_env = Infer::new_env();
    let prelude = {
        let ops = ["+", "-", "*", "/"];
        let mut v: Vec<_> = ops.into_iter().map(|n| (interner.intern(n), ty_op.clone())).collect();
        v.push((interner.intern("puts"), parse_type("String -> Void", &mut interner)));
        _ty_env.extend_n(v)
    };

    'main: loop {
        print!("> ");
        stdout.flush().unwrap();
        input.clear();
        stdin.read_line(&mut input).ok().expect("Failed to read line");
        if input.as_str() == ".quit\n" {
            break;
        }
        let mut res: Vec<_> = parse(input.as_str(), &mut interner).unwrap();
        println!("Parsed syntax tree:");
        println!("{:#?}", res);

        println!("String intern: ");
        println!("{:#?}", interner);

        let infered = {
            let mut ty_infer = Infer::new(&mut interner);
            ty_infer.infer_defs(&prelude, &mut res)
        };
        match infered {
            Ok(_) => {
                // let env = types.unwrap();
                println!("Typed AST:");
                println!("{:#?}", res);
                println!("Core term:");
                let (top, _) = K::go(res, &mut interner);
                println!("{:#?}", top);
                println!("LLVM IR:");
                let mut generator = LLVMEmit::new("repl", &mut interner);
                for def in top.values() {
                    generator.gen_top_level(def.deref(), &VarEnv::new());
                }
                generator.dump();
            }
            Err(e) => println!("{:#?}", e),
        }



    }
}

fn main() {
    repl();
}