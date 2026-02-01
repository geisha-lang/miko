use clap::Parser;

use miko::utils::*;
use miko::syntax::parser::*;
use miko::typeinfer::*;
use miko::codegen::*;
use miko::core::*;
use miko::types::*;
use miko::modules::ModuleLoader;
use std::cell::RefCell;
use std::env;
use std::fs;
use std::io;
use std::io::Write;
use std::io::Read;
use std::ops::Deref;
use std::path::{Path, PathBuf};
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

    /// Source root directory for module resolution
    #[arg(long = "src-root")]
    src_root: Option<String>,

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

/// Compile a multi-file project starting from an entry file.
///
/// This function:
/// 1. Loads the entry file and all modules it depends on
/// 2. Resolves dependencies and determines compilation order
/// 3. Resolves `use` statements to bring names into scope
/// 4. Combines all definitions and compiles them together
fn compile_project(
    entry_file: &Path,
    src_root: &Path,
) -> Result<LLVMCodegen, CompileError> {
    use std::collections::HashMap;
    use miko::modules::{NameInfo, resolve_imports_with_visibility, ImportError};

    let interner = RefCell::new(Interner::new());
    let mut loader = ModuleLoader::new(src_root.to_path_buf());

    // Load the entry module (this recursively loads submodules)
    loader
        .load_entry(entry_file, &interner)
        .map_err(|e| CompileError::Normal(format!("{}", e)))?;

    // Get the entry module path
    let entry_segments = loader.get(&{
        let rel = entry_file.strip_prefix(src_root).unwrap_or(entry_file);
        let stem = rel.with_extension("");
        let final_path = if stem.file_name().map(|n| n == "mod").unwrap_or(false) {
            stem.parent().unwrap_or(&stem).to_path_buf()
        } else {
            stem.to_path_buf()
        };
        final_path.components()
            .filter_map(|c| c.as_os_str().to_str())
            .collect::<Vec<_>>()
            .join(".")
    }).map(|m| m.path.to_string_with(&interner.borrow())).unwrap_or_default();

    // First pass: collect all qualified names for import resolution (with visibility)
    let mut available_names: HashMap<String, NameInfo> = HashMap::new();
    let mut all_defs: Vec<miko::syntax::form::Def> = Vec::new();

    for module in loader.loaded_modules() {
        let module_path = module.path.to_string_with(&interner.borrow());

        // Iterate over defs with visibility information
        for (visibility, def) in &module.defs {
            let mut def = def.clone();
            let is_public = visibility.is_public();

            // For submodules, rename definitions with qualified names
            if module_path != entry_segments && !entry_segments.is_empty() {
                let relative_path = if module_path.starts_with(&entry_segments) {
                    module_path[entry_segments.len()..].trim_start_matches('.').to_string()
                } else {
                    module_path.clone()
                };

                if !relative_path.is_empty() {
                    // Rename the definition with qualified name
                    let orig_name = interner.borrow().trace(def.ident).to_string();
                    let qualified_name = format!("{}.{}", relative_path, orig_name);
                    let qualified_id = interner.borrow_mut().intern(&qualified_name);
                    def.ident = qualified_id;

                    // Register the qualified name for import resolution (with visibility)
                    available_names.insert(qualified_name, NameInfo {
                        qualified_id,
                        is_public,
                    });
                }
            } else {
                // Entry module definitions - register with just their name
                let name = interner.borrow().trace(def.ident).to_string();
                available_names.insert(name, NameInfo {
                    qualified_id: def.ident,
                    is_public,
                });
            }
            all_defs.push(def);
        }
    }

    // Second pass: resolve imports with visibility checking
    let mut import_aliases: Vec<(miko::utils::Id, miko::utils::Id)> = Vec::new();
    let mut all_import_errors: Vec<ImportError> = Vec::new();

    for module in loader.loaded_modules() {
        let module_path = module.path.to_string_with(&interner.borrow());

        // Only resolve imports for the entry module (other modules use qualified names)
        if module_path == entry_segments || entry_segments.is_empty() {
            let (import_map, errors) = resolve_imports_with_visibility(
                &module.imports,
                &module_path,
                &available_names,
                &mut interner.borrow_mut(),
            );

            // Collect errors
            all_import_errors.extend(errors);

            // Collect the import aliases
            for (local_id, qualified_id) in import_map.iter() {
                import_aliases.push((*local_id, *qualified_id));
            }
        }
    }

    // Report any visibility errors
    if !all_import_errors.is_empty() {
        let error_msgs: Vec<String> = all_import_errors.iter().map(|e| match e {
            ImportError::PrivateItem { name, module } => {
                format!("cannot import private item '{}' from module '{}'", name, module)
            }
            ImportError::NotFound { name, module } => {
                format!("item '{}' not found in module '{}'", name, module)
            }
        }).collect();
        return Err(CompileError::Normal(error_msgs.join("\n")));
    }

    // Now compile with the collected definitions
    let mut inter = interner.into_inner();

    // Type inference with import aliases
    let (adt_registry, instantiation_registry) = {
        let env = Infer::new_env();
        let prelude = load_prelude(&mut inter, &env);
        let mut infer = Infer::new(&mut inter);

        // Pass import aliases to infer_defs for adding to the type environment
        infer.infer_defs_with_imports(&prelude, &mut all_defs, &import_aliases)
            .map_err(|te| CompileError::TypeError(te))?;
        (infer.adt_registry, infer.instantiation_registry)
    };

    // K-conversion (core term generation) with monomorphization and import resolution
    let import_map: std::collections::HashMap<miko::utils::Id, miko::utils::Id> =
        import_aliases.into_iter().collect();
    let (mut top, _) = K::go_with_imports(all_defs, &mut inter, adt_registry.clone(), instantiation_registry, import_map);

    // Escape analysis for stack allocation optimization
    let escape_info = miko::core::escape::analyze_all(&top, &adt_registry, 128);

    // Code generation
    let name = entry_file.to_string_lossy();
    let main_id = inter.intern("main");
    let mut emitter = LLVMEmit::new(&name, &mut inter, adt_registry, escape_info);
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

    // Determine if we should use multi-file compilation
    let use_project_mode = args.src_root.is_some();

    let result = if use_project_mode {
        // Multi-file project compilation
        let src_root = PathBuf::from(args.src_root.as_ref().unwrap());
        let entry_path = PathBuf::from(&input_file);
        compile_project(&entry_path, &src_root)
    } else {
        // Single-file compilation (existing behavior)
        fs::File::open(&input_file)
            .map_err(|e| CompileError::Normal(format!("open file failed: {}", e)))
            .and_then(|mut f| {
                let mut src = String::new();
                f.read_to_string(&mut src).map_err(|e| CompileError::Normal(format!("read failed: {}", e)))?;
                compile(&input_file, &src)
            })
    }
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
