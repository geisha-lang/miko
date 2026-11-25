use clap::Parser;
use std::env;
use std::fs;
use std::io::{self, Read, Write};
use std::path::PathBuf;
use std::process::Command;

use miko::codegen::{module_emit_file, optimize_module};
use miko::compile::{compile_project, compile_str, CompileError};

const DEFAULT_RUNTIME_PATH: &str = env!("GEISHA_RUNTIME_PATH");

fn get_runtime_path(cli_arg: Option<&str>) -> String {
    cli_arg
        .map(String::from)
        .or_else(|| env::var("GEISHA_RUNTIME").ok())
        .unwrap_or_else(|| DEFAULT_RUNTIME_PATH.to_string())
}

#[derive(Copy, Clone, Debug, Default, PartialEq)]
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

    /// Source root directory for module resolution
    #[arg(long = "src-root")]
    src_root: Option<String>,

    /// Input file
    input: Option<String>,
}

fn run_repl() {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut input = String::new();

    loop {
        print!("> ");
        stdout.flush().unwrap();
        input.clear();
        if stdin.read_line(&mut input).is_err() || input.is_empty() || input.as_str() == ".quit\n" {
            break;
        }
        println!("REPL not fully implemented");
    }
}

fn link_executable(obj_file: &str, out_file: &str, runtime_path: &str) -> Result<(), CompileError> {
    let status = Command::new("cc")
        .arg("-pthread")
        .arg("-o")
        .arg(out_file)
        .arg(obj_file)
        .arg(runtime_path)
        .status()
        .map_err(|e| CompileError::Normal(format!("failed to run linker: {}", e)))?;

    let _ = fs::remove_file(obj_file);

    if !status.success() {
        return Err(CompileError::Normal(format!(
            "linking failed with exit code: {}",
            status.code().unwrap_or(-1)
        )));
    }
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

    let out_file = match (&emit_type, args.out_file) {
        (EmitType::LLVMIR, f) => f.unwrap_or_default(),
        (_, Some(f)) => f,
        (_, None) => {
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

    let result = if let Some(ref root) = args.src_root {
        let src_root = PathBuf::from(root);
        let entry_path = PathBuf::from(&input_file);
        compile_project(&entry_path, &src_root)
    } else {
        fs::File::open(&input_file)
            .map_err(|e| CompileError::Normal(format!("open file failed: {}", e)))
            .and_then(|mut f| {
                let mut src = String::new();
                f.read_to_string(&mut src)
                    .map_err(|e| CompileError::Normal(format!("read failed: {}", e)))?;
                compile_str(&input_file, &src)
            })
    }
    .and_then(|mut codegen| match emit_type {
        // IMPORTANT: keep `codegen` alive while emitting/dumping `codegen.module` so `codegen.context` is NOT dropped early!
        EmitType::LLVMIR => {
            let _ = optimize_module(&codegen.module);
            codegen.module.dump();
            Ok(())
        }
        EmitType::ObjectFile => {
            module_emit_file(&out_file, &mut codegen.module).map_err(CompileError::Normal)
        }
        EmitType::Executable => {
            let obj_file = format!("{}.o", out_file);
            module_emit_file(&obj_file, &mut codegen.module).map_err(CompileError::Normal)?;
            let runtime_path = get_runtime_path(args.runtime.as_deref());
            link_executable(&obj_file, &out_file, &runtime_path)
        }
    });

    if let Err(e) = result {
        eprintln!("Compiling error:\n{}", e);
        std::process::exit(1);
    }
}
