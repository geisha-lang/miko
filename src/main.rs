use clap::Parser;

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


#[derive(Clone, Debug)]
#[allow(dead_code)]
enum CompileError {
    TypeError(TypeError),
    ParseError(String),
    Normal(String)
}

#[derive(Copy, Clone, Debug, Default, PartialEq)]
#[allow(dead_code)]
enum EmitType {
    LLVMIR,
    StaticLibrary,
    #[default]
    Executable,
}

#[derive(Parser, Debug)]
#[command(name = "miko")]
#[command(about = "Geisha language compiler")]
struct Args {
    /// Run REPL mode
    #[arg(short = 'r', long = "repl")]
    repl: bool,

    /// Output file name
    #[arg(short = 'o', long = "out")]
    out_file: Option<String>,

    /// Emit LLVM IR instead of object file
    #[arg(short = 'e', long = "emit-llvm")]
    emit_llvm: bool,

    /// Input file
    input: Option<String>,
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


    let _ty_env = Infer::new_env();
    let _prelude = load_prelude(&mut interner, &_ty_env);

    loop {
        print!("> ");
        stdout.flush().unwrap();
        input.clear();
        stdin.read_line(&mut input).ok().expect("Failed to read line");
        if input.as_str() == ".quit\n" {
            break;
        }
        // REPL implementation is incomplete
        println!("REPL not fully implemented");
    }
}

fn compile(name: &str, src: &str) -> Result<LLVMCodegen, CompileError> {
    let mut inter = Interner::new();

    // Parse source
    let mut defs = parse(src, &mut inter)
        .map_err(|e| CompileError::ParseError(format!("{}", e)))?;

    // Type inference
    let adt_registry = {
        let env = Infer::new_env();
        let prelude = load_prelude(&mut inter, &env);
        let mut infer = Infer::new(&mut inter);
        infer.infer_defs(&prelude, &mut defs)
            .map_err(|te| CompileError::TypeError(te))?;
        infer.adt_registry
    };

    // K-conversion (core term generation)
    let (mut top, _) = K::go(defs, &mut inter, adt_registry.clone());

    // Code generation
    let main_id = inter.intern("main");
    let mut emitter = LLVMEmit::new(name, &mut inter, adt_registry);
    let main_fn = top.remove(&main_id);
    let env = VarEnv::new();
    if let Some(mf) = main_fn {
        emitter.gen_main(mf.deref(), &env);
    }
    for def in top.values() {
        emitter.gen_top_level(def.deref(), &env);
    }
    Ok(emitter.generator)
}


fn main() {
    let args = Args::parse();

    let emit_type = if args.emit_llvm {
        EmitType::LLVMIR
    } else {
        EmitType::Executable
    };

    if args.repl {
        run_repl();
        return;
    }

    let out_file = match args.out_file {
        Some(f) => f,
        None => {
            eprintln!("No output file name");
            std::process::exit(1);
        }
    };

    let input_file = match args.input {
        Some(f) => f,
        None => {
            eprintln!("No input file name");
            std::process::exit(1);
        }
    };

    let result = fs::File::open(&input_file)
        .map_err(|e| CompileError::Normal(format!("open file failed: {}", e)))
        .and_then(|mut f| {
            let mut src = String::new();
            f.read_to_string(&mut src).map_err(|e| CompileError::Normal(format!("read failed: {}", e)))?;
            compile(&input_file, &src)
        })
        .and_then(|gen| {
            match emit_type {
                EmitType::LLVMIR => {
                    gen.module.dump();
                    Ok(())
                }
                EmitType::Executable => {
                    let LLVMCodegen { mut module, .. } = gen;
                    module_emit_file(&out_file, &mut module)
                        .map_err(|e| CompileError::Normal(e))
                }
                _ => unimplemented!()
            }
        });

    if let Err(e) = result {
        eprintln!("Compiling error:");
        eprintln!("{:?}", e);
        std::process::exit(1);
    }
}
