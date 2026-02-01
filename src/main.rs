use clap::Parser;

use miko::utils::*;
use miko::syntax::parser::*;
use miko::typeinfer::*;
use miko::codegen::*;
use miko::core::*;
use miko::types::*;
use std::env;
use std::fs;
use std::io;
use std::io::Write;
use std::io::Read;
use std::ops::Deref;
use std::process::Command;

// Default path to the runtime library, set by build.rs
const DEFAULT_RUNTIME_PATH: &str = env!("GEISHA_RUNTIME_PATH");

/// Get runtime library path: CLI arg > env var > compiled default
fn get_runtime_path(cli_arg: Option<&str>) -> String {
    cli_arg
        .map(String::from)
        .or_else(|| env::var("GEISHA_RUNTIME").ok())
        .unwrap_or_else(|| DEFAULT_RUNTIME_PATH.to_string())
}

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
    ObjectFile,
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

    /// Compile only, do not link (emit object file)
    #[arg(short = 'c', long = "compile-only")]
    compile_only: bool,

    /// Path to runtime library (overrides GEISHA_RUNTIME env var)
    #[arg(long = "runtime")]
    runtime: Option<String>,

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
    let (adt_registry, instantiation_registry) = {
        let env = Infer::new_env();
        let prelude = load_prelude(&mut inter, &env);
        let mut infer = Infer::new(&mut inter);
        infer.infer_defs(&prelude, &mut defs)
            .map_err(|te| CompileError::TypeError(te))?;
        (infer.adt_registry, infer.instantiation_registry)
    };

    // K-conversion (core term generation) with monomorphization
    let (mut top, _) = K::go(defs, &mut inter, adt_registry.clone(), instantiation_registry);

    // Escape analysis for stack allocation optimization
    let escape_info = miko::core::escape::analyze_all(&top, &adt_registry, 128);

    // Code generation
    let main_id = inter.intern("main");
    let mut emitter = LLVMEmit::new(name, &mut inter, adt_registry, escape_info);
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


fn link_executable(obj_file: &str, out_file: &str, runtime_path: &str) -> Result<(), CompileError> {
    // Note: object file must come before static library for proper symbol resolution
    let status = Command::new("cc")
        .arg("-o")
        .arg(out_file)
        .arg(obj_file)
        .arg(runtime_path)
        .status()
        .map_err(|e| CompileError::Normal(format!("failed to run linker: {}", e)))?;

    if !status.success() {
        return Err(CompileError::Normal(format!(
            "linking failed with exit code: {}",
            status.code().unwrap_or(-1)
        )));
    }

    // Clean up intermediate object file
    let _ = fs::remove_file(obj_file);

    Ok(())
}

fn main() {
    let args = Args::parse();

    let emit_type = if args.emit_llvm {
        EmitType::LLVMIR
    } else if args.compile_only {
        EmitType::ObjectFile
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
                EmitType::ObjectFile => {
                    let LLVMCodegen { mut module, .. } = gen;
                    module_emit_file(&out_file, &mut module)
                        .map_err(|e| CompileError::Normal(e))
                }
                EmitType::Executable => {
                    let LLVMCodegen { mut module, .. } = gen;
                    // Generate object file to temp location
                    let obj_file = format!("{}.o", out_file);
                    module_emit_file(&obj_file, &mut module)
                        .map_err(|e| CompileError::Normal(e))?;
                    // Link with runtime to produce executable
                    let runtime_path = get_runtime_path(args.runtime.as_deref());
                    link_executable(&obj_file, &out_file, &runtime_path)
                }
            }
        });

    if let Err(e) = result {
        eprintln!("Compiling error:");
        eprintln!("{:?}", e);
        std::process::exit(1);
    }
}
