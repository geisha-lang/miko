extern crate miko;
extern crate argparse;

use miko::utils::*;
use miko::syntax::parser::*;
use miko::typeinfer::*;
use miko::codegen::*;
use miko::core::*;
use miko::types::*;
use std::fs;
use std::io;
use std::io::Write;
use std::io::Read;
use std::ops::Deref;
use std::error::Error;


#[derive(Clone, Debug)]
enum CompileError {
    TypeError(TypeError),
    ParseError(ParseError),
    Normal(String)
}

#[derive(Copy, Clone, Debug)]
enum EmitType {
    LLVMIR,
    StaticLibrary,
    Excutable,
}


fn load_prelude<'a, 'b: 'a>(interner: &mut Interner, env: &'b TypeEnv) -> TypeEnv<'a> {
    macro_rules! add_symbols {
        ($($name:tt : $ty:expr),*) => {{
            let mut v = vec![];

        $({
            let id = interner.intern($name);
            let ty = parse_type($ty, interner);
            v.push((id, ty));
        })*
            env.extend_n(v)

        }};
    }
    let e = add_symbols!(
        "+" : "forall a. a * a -> a",
        "-" : "forall a. a * a -> a",
        "*" : "forall a. a * a -> a",
        "/" : "forall a. a * a -> a",
        "<" : "forall a. a * a -> Bool",
        ">" : "forall a. a * a -> Bool",
        "<=" : "forall a. a * a -> Bool",
        ">=" : "forall a. a * a -> Bool",
        "==" : "forall a. a * a -> Bool",
        "||" : "Bool * Bool -> Bool",
        "&&" : "Bool * Bool -> Bool",
        "printLn" : "String -> Void",
        "putNumber" : "Int -> Void"
    );
    e
}

fn run_repl() {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut input = String::new();

    let mut interner = Interner::new();


    let mut _ty_env = Infer::new_env();
    let mut prelude = load_prelude(&mut interner, &mut _ty_env);

    'main: loop {
        print!("> ");
        stdout.flush().unwrap();
        input.clear();
        stdin.read_line(&mut input).ok().expect("Failed to read line");
        if input.as_str() == ".quit\n" {
            break;
        }

//        let parsed = parse(input.as_str(), &mut interner);
//        if let Err(why) = parsed {
//            println!("{}", why);
//            continue;
//        }
//        let mut res = parsed.unwrap();
//        println!("Parsed syntax tree:");
//        println!("{:#?}", res);
//
//        println!("String intern: ");
//        println!("{:#?}", interner);
//
//        let infered = {
//            let mut ty_infer = Infer::new(&mut interner);
//            ty_infer.infer_defs(&prelude, &mut res)
//        };
//        match infered {
//            Ok(_) => {
//                // let env = types.unwrap();
//                println!("Typed AST:");
//                println!("{:#?}", res);
//                println!("Core term:");
//                let (top, _) = K::go(res, &mut interner);
//                println!("{:#?}", top);
//                println!("LLVM IR:");
//                let mut generator = LLVMEmit::new("repl", &mut interner);
//                for def in top.values() {
//                    generator.gen_top_level(def.deref(), &VarEnv::new());
//                }
//                generator.dump();
//            }
//            Err(e) => println!("Type error: {:?}", e),
//        }
    }
}

fn compile(name: &str, src: &str) -> Result<LLVMCodegen, CompileError> {
    let mut inter = Interner::new();

    parse(src, &mut inter)
    .map_err(|e| CompileError::ParseError(e))
    .and_then(|mut defs| {
        let env = Infer::new_env();
        let prelude = load_prelude(&mut inter, &env);
        let mut infer = Infer::new(&mut inter);
        infer.infer_defs(&prelude, &mut defs)
            .map_err(|te| CompileError::TypeError(te))
            .map(|()| defs)
    })
    .and_then(|defs|{
        let (mut top, _) = K::go(defs, &mut inter);
        let main_id = inter.intern("main");
        let mut emitter = LLVMEmit::new(name, &mut inter);
    //    emitter.close_function_pass();
        let main_fn = top.remove(&main_id);
        let env = VarEnv::new();
        if let Some(mf) = main_fn {
            emitter.gen_main(mf.deref(), &env);
        }
        for def in top.values() {
            println!("{:#?}", def);
            emitter.gen_top_level(def.deref(), &env);
        }
        Ok(emitter.generator)
    })
}


fn main() {
    use argparse::{ArgumentParser, StoreTrue, Store, StoreConst};

    let mut repl = false;
    let mut emit_type = EmitType::Excutable;
    let mut input_file = String::new();
    let mut out_file = String::new();

    {
        let mut ap = ArgumentParser::new();
        ap.set_description("Geisha language compiler");
        ap.refer(&mut repl)
            .add_option(&["-r", "--repl"], StoreTrue,
                        "run REPL");
        ap.refer(&mut out_file)
            .add_option(&["-o", "--out"], Store,
                        "File name to output");
        ap.refer(&mut emit_type)
            .add_option(&["-e", "--emit-llvm"], StoreConst(EmitType::LLVMIR),
                        "Emit LLVM IR");
//        ap.refer(&mut emit_type)
//            .add_option(&["-e", "--emit-exe"], StoreConst(EmitType::Excutable),
//                        "Emit excutable file");
        ap.refer(&mut input_file)
            .add_argument("input", Store,
                        "Input file");
        ap.parse_args_or_exit();
    }

    if repl {
        run_repl();
        return;
    }
    if out_file.len() == 0 {
        eprintln!("No output file name");
        ::std::process::exit(1);
    }
    if input_file.len() == 0 {
        eprintln!("No input file name");
        ::std::process::exit(1);
    }

    fs::File::open(input_file.as_str())
        .map_err(|e| CompileError::Normal("open file failed: ".to_string() + e.description()))
        .and_then(|mut f| {
            let mut src = String::new();
            f.read_to_string(&mut src);
            compile(input_file.as_str(), src.as_str())
        })
        .and_then(|gen| {
            match emit_type {
                EmitType::LLVMIR => {
                    gen.module.dump();
                    Ok(())
                }
                EmitType::Excutable => {
                    let LLVMCodegen { mut module, .. } = gen;
                    module_emit_file(out_file.as_str(), &mut module)
                        .map_err(|e| CompileError::Normal(e))
                }
                _ => unimplemented!()
            }
        })
        .map_err(|e| {
            eprintln!("Compiling error:");
            eprintln!("{:?}", e);
            ::std::process::exit(1);
//            eprintln!()
        });




}
