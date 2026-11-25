//! End-to-end compilation driver (single-file & multi-file projects).

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::ops::Deref;
use std::path::Path;

use crate::codegen::*;
use crate::core::*;
use crate::internal::VarDecl;
use crate::modules::{
    resolve_imports_with_visibility, DependencyGraph, ImportError, ModuleLoader, NameInfo,
};
use crate::syntax::form::*;
use crate::syntax::parser::*;
use crate::typeinfer::*;
use crate::types::*;
use crate::utils::*;

#[derive(Clone, Debug)]
pub enum CompileError {
    TypeError(TypeError, Option<String>),
    ParseError(String),
    Normal(String),
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompileError::TypeError(te, Some(src)) => {
                let span = te.span();
                if span.end > 0 && span.start <= src.len() {
                    let (line, col, snippet) = locate_span(src, span);
                    writeln!(f, "Type error at {}:{}: {}", line, col, te)?;
                    writeln!(f, "  | {}", snippet)?;
                    Ok(())
                } else {
                    write!(f, "Type error: {}", te)
                }
            }
            CompileError::TypeError(te, None) => write!(f, "Type error: {}", te),
            CompileError::ParseError(msg) => write!(f, "Parse error: {}", msg),
            CompileError::Normal(msg) => write!(f, "{}", msg),
        }
    }
}

fn locate_span(src: &str, span: Span) -> (usize, usize, &str) {
    let clamped = span.start.min(src.len());
    let mut line = 1usize;
    let mut line_start = 0usize;
    for (idx, b) in src.bytes().enumerate() {
        if idx >= clamped {
            break;
        }
        if b == b'\n' {
            line += 1;
            line_start = idx + 1;
        }
    }
    let line_end = src[line_start..]
        .find('\n')
        .map(|off| line_start + off)
        .unwrap_or(src.len());
    let col = clamped.saturating_sub(line_start) + 1;
    (line, col, src[line_start..line_end].trim_end_matches('\r'))
}

pub fn load_prelude<'a, 'b: 'a>(interner: &mut Interner, env: &'b TypeEnv) -> TypeEnv<'a> {
    let builtins = [
        ("printLn", "String -> Void"),
        ("putNumber", "Int -> Void"),
        ("putFloat", "Float -> Void"),
        ("putChar", "Char -> Void"),
        ("cellNew", "forall a. a -> Cell a"),
        ("cellGet", "forall a. Cell a -> a"),
        ("cellSet", "forall a. (Cell a, a) -> Void"),
        ("arrayNew", "forall a. (Int, a) -> MutArray a"),
        ("arrayLen", "forall a. MutArray a -> Int"),
        ("arrayGet", "forall a. (MutArray a, Int) -> a"),
        ("arraySet", "forall a. (MutArray a, Int, a) -> Void"),
        ("bitAnd", "(Int, Int) -> Int"),
        ("bitOr", "(Int, Int) -> Int"),
        ("bitXor", "(Int, Int) -> Int"),
        ("bitNot", "Int -> Int"),
        ("bitShl", "(Int, Int) -> Int"),
        ("bitShr", "(Int, Int) -> Int"),
        ("bitUshr", "(Int, Int) -> Int"),
        ("bitAnd64", "(Int64, Int64) -> Int64"),
        ("bitOr64", "(Int64, Int64) -> Int64"),
        ("bitXor64", "(Int64, Int64) -> Int64"),
        ("bitNot64", "Int64 -> Int64"),
        ("bitShl64", "(Int64, Int) -> Int64"),
        ("bitShr64", "(Int64, Int) -> Int64"),
        ("bitUshr64", "(Int64, Int) -> Int64"),
        ("charToInt", "Char -> Int"),
        ("intToChar", "Int -> Char"),
        ("intToI64", "Int -> Int64"),
        ("i64ToInt", "Int64 -> Int"),
        ("floatToBits64", "Float -> Int64"),
        ("bits64ToFloat", "Int64 -> Float"),
        ("intToFloat", "Int -> Float"),
        ("floatToInt", "Float -> Int"),
        ("strLen", "String -> Int"),
        ("strByteAt", "(String, Int) -> Int"),
        ("strSlice", "(String, Int, Int) -> String"),
        ("strConcat", "(String, String) -> String"),
        ("strFromBytes", "(MutArray Int, Int) -> String"),
        ("ptrNull", "() -> Ptr"),
        ("ptrIsNull", "Ptr -> Bool"),
        ("ptrAllocBytes", "Int -> Ptr"),
        ("ptrByteAt", "(Ptr, Int) -> Int"),
        ("ptrSetByte", "(Ptr, Int, Int) -> Void"),
        ("ptrToString", "Ptr -> String"),
        ("sysArgc", "() -> Int"),
        ("sysArgv", "Int -> String"),
        // Concurrency runtime intrinsics (wrapped by `std.sync`, `std.task`, `std.actor`)
        ("__concAtomicNew", "Int -> AtomicInt"),
        ("__concAtomicLoad", "AtomicInt -> Int"),
        ("__concAtomicStore", "(AtomicInt, Int) -> Void"),
        ("__concAtomicAdd", "(AtomicInt, Int) -> Int"),
        ("__concAtomicCas", "(AtomicInt, Int, Int) -> Bool"),
        ("__concMVarNew", "forall a. (Send a) => a -> MVar a"),
        ("__concMVarEmpty", "forall a. (Send a) => () -> MVar a"),
        ("__concMVarTake", "forall a. (Send a) => MVar a -> a"),
        ("__concMVarPut", "forall a. (Send a) => (MVar a, a) -> Void"),
        ("__concMVarRead", "forall a. (Send a) => MVar a -> a"),
        ("__concMVarTryTake", "forall a. (Send a) => MVar a -> (Bool, a)"),
        ("__concChanNew", "forall a. (Send a) => Int -> Chan a"),
        ("__concChanSend", "forall a. (Send a) => (Chan a, a) -> Bool"),
        ("__concChanRecv", "forall a. (Send a) => Chan a -> (Bool, a)"),
        ("__concChanTryRecv", "forall a. (Send a) => Chan a -> (Bool, a)"),
        ("__concChanClose", "forall a. Chan a -> Void"),
        ("__concChanIsClosed", "forall a. Chan a -> Bool"),
        ("__concTaskSpawn", "forall a. (Send a) => (() -> a) -> Task a"),
        ("__concTaskAwait", "forall a. (Send a) => Task a -> a"),
        ("__concTaskIsReady", "forall a. Task a -> Bool"),
        ("__concTaskWaitAny", "forall a. (MutArray (Task a), Int, Int) -> Void"),
        ("__concNumCpu", "() -> Int"),
        ("__concSleepMs", "Int -> Void"),
        ("__concActorSpawn", "forall s. (() -> s) -> Actor s"),
        ("__concActorCall", "forall s r. (Send r) => (Actor s, s -> r) -> Task r"),
        ("__concActorSend", "forall s. (Actor s, s -> Void) -> Void"),
        ("__concActorStop", "forall s. Actor s -> Void"),
    ];
    let mut v = Vec::with_capacity(builtins.len());
    for (name, sig) in builtins {
        let id = interner.intern(name);
        let ty = parse_type(sig, interner);
        v.push((id, ty));
    }
    env.extend_n(v)
}

/// Compile a standalone Geisha source string (supports inline `mod`, `use`, `open`, `pub` as well!).
pub fn compile_str(name: &str, src: &str) -> Result<LLVMCodegen, CompileError> {
    let interner = RefCell::new(Interner::new());
    let mut loader = ModuleLoader::new(Path::new(".").to_path_buf());
    loader
        .load_single_source(Path::new(name), src, &interner)
        .map_err(|e| CompileError::ParseError(e.to_string()))?;

    compile_loaded_modules(&loader, "", interner, Some(src.to_string()))
}

/// Compile a multi-file Geisha project starting from `entry_file` under `src_root`.
pub fn compile_project(entry_file: &Path, src_root: &Path) -> Result<LLVMCodegen, CompileError> {
    let interner = RefCell::new(Interner::new());
    let mut loader = ModuleLoader::new(src_root.to_path_buf());
    loader
        .load_entry(entry_file, &interner)
        .map_err(|e| CompileError::Normal(e.to_string()))?;

    let entry_segments = loader
        .path_to_segments(entry_file)
        .map(|s| s.join("."))
        .unwrap_or_default();

    compile_loaded_modules(&loader, &entry_segments, interner, None)
}

fn relative_module_prefix(mod_key: &str, entry_key: &str) -> String {
    if mod_key == entry_key {
        String::new()
    } else if !entry_key.is_empty() && mod_key.starts_with(&format!("{}.", entry_key)) {
        mod_key[entry_key.len() + 1..].to_string()
    } else {
        mod_key.to_string()
    }
}

fn extract_use_target_module(use_item: &UseItem, interner: &Interner) -> String {
    match use_item {
        UseItem::Single(p) | UseItem::Alias(p, _) => p
            .module_path()
            .iter()
            .map(|id| interner.trace(*id))
            .collect::<Vec<_>>()
            .join("."),
        UseItem::Multiple(p, _) | UseItem::Glob(p) => p.to_string_with(interner),
    }
}

fn compile_loaded_modules(
    loader: &ModuleLoader,
    entry_key: &str,
    interner: RefCell<Interner>,
    single_source: Option<String>,
) -> Result<LLVMCodegen, CompileError> {
    // 1. Build module dependency graph and compute topological order
    let loaded_mods = loader.loaded_modules();
    let known_keys: HashSet<String> = loaded_mods
        .iter()
        .map(|m| m.path.to_string_with(&interner.borrow()))
        .collect();

    // Map relative prefix -> canonical loader key
    let mut prefix_to_key: HashMap<String, String> = HashMap::new();
    for k in &known_keys {
        prefix_to_key.insert(relative_module_prefix(k, entry_key), k.clone());
    }

    let mut dep_graph = DependencyGraph::new();
    for m in &loaded_mods {
        let m_key = m.path.to_string_with(&interner.borrow());
        let m_prefix = relative_module_prefix(&m_key, entry_key);
        let mut deps = Vec::new();

        if known_keys.contains("std.result")
            && m_key != "std.result"
            && m.defs.iter().any(|(_, d)| def_contains_try(d))
        {
            deps.push("std.result".to_string());
        }

        for (_, _, child_key) in &m.submodules {
            if known_keys.contains(child_key) && child_key != &m_key && !deps.contains(child_key) {
                deps.push(child_key.clone());
            }
        }

        for (_, use_item) in &m.imports {
            let raw_mod = extract_use_target_module(use_item, &interner.borrow());
            if raw_mod.is_empty() {
                continue;
            }
            let cand_rel = if m_prefix.is_empty() {
                raw_mod.clone()
            } else {
                format!("{}.{}", m_prefix, raw_mod)
            };
            let target_key = prefix_to_key
                .get(&cand_rel)
                .or_else(|| prefix_to_key.get(&raw_mod))
                .cloned();
            if let Some(tk) = target_key {
                if tk != m_key && !deps.contains(&tk) {
                    deps.push(tk);
                }
            }
        }

        dep_graph.add_module(&m_key, deps);
    }

    let topo_keys = dep_graph
        .topological_sort()
        .map_err(|e| CompileError::Normal(e.to_string()))?;

    // 2. Register qualified names & per-module local rename tables
    let mut available_names: HashMap<String, NameInfo> = HashMap::new();
    let mut module_local_renames: HashMap<String, HashMap<Id, Id>> = HashMap::new();
    let mut module_type_renames: HashMap<String, HashMap<String, String>> = HashMap::new();
    let mut module_ctor_renames: HashMap<String, HashMap<String, String>> = HashMap::new();

    for m_key in &topo_keys {
        let Some(m) = loader.get(m_key) else {
            continue;
        };
        let prefix = relative_module_prefix(m_key, entry_key);
        let mut val_renames = HashMap::new();
        let mut ty_renames = HashMap::new();
        let mut ctor_renames = HashMap::new();

        for (vis, def) in &m.defs {
            let orig_name = interner.borrow().trace(def.ident).to_string();
            let q_name = if prefix.is_empty() {
                orig_name.clone()
            } else {
                format!("{}.{}", prefix, orig_name)
            };
            let q_id = interner.borrow_mut().intern(&q_name);

            available_names.insert(
                q_name.clone(),
                NameInfo {
                    qualified_id: q_id,
                    is_public: vis.is_public(),
                    defined_in_module: prefix.clone(),
                },
            );

            if !prefix.is_empty() {
                val_renames.insert(def.ident, q_id);
                if matches!(def.node, Item::Alg(..) | Item::Alias(..) | Item::Concept { .. }) {
                    ty_renames.insert(orig_name.clone(), q_name.clone());
                }
            }

            // Register ADT constructors and Concept methods into module namespace as well
            match &def.node {
                Item::Alg(_, variants) => {
                    for v in variants {
                        let ctor_id = interner.borrow_mut().intern(&v.name);
                        let q_ctor = if prefix.is_empty() {
                            v.name.clone()
                        } else {
                            format!("{}.{}", prefix, v.name)
                        };
                        let q_ctor_id = interner.borrow_mut().intern(&q_ctor);
                        available_names.insert(
                            q_ctor.clone(),
                            NameInfo {
                                qualified_id: q_ctor_id,
                                is_public: vis.is_public(),
                                defined_in_module: prefix.clone(),
                            },
                        );
                        if !prefix.is_empty() {
                            val_renames.insert(ctor_id, q_ctor_id);
                            ctor_renames.insert(v.name.clone(), q_ctor);
                        }
                    }
                }
                Item::Concept { methods, .. } => {
                    for md in methods {
                        let m_orig = interner.borrow().trace(md.name).to_string();
                        let q_m = if prefix.is_empty() {
                            m_orig
                        } else {
                            format!("{}.{}", prefix, m_orig)
                        };
                        let q_m_id = interner.borrow_mut().intern(&q_m);
                        available_names.insert(
                            q_m,
                            NameInfo {
                                qualified_id: q_m_id,
                                is_public: vis.is_public(),
                                defined_in_module: prefix.clone(),
                            },
                        );
                        if !prefix.is_empty() {
                            val_renames.insert(md.name, q_m_id);
                        }
                    }
                }
                _ => {}
            }
        }

        module_local_renames.insert(m_key.clone(), val_renames);
        module_type_renames.insert(m_key.clone(), ty_renames);
        module_ctor_renames.insert(m_key.clone(), ctor_renames);
    }

    // Export built-in concurrent handle types from their canonical `std.*` modules as well
    for (mod_name, builtin_tys) in [
        ("std.sync", &["AtomicInt", "MVar", "Chan"][..]),
        ("std.task", &["Task"][..]),
        ("std.actor", &["Actor"][..]),
    ] {
        if known_keys.contains(mod_name) {
            for &ty_name in builtin_tys {
                let raw_id = interner.borrow_mut().intern(ty_name);
                available_names.insert(
                    format!("{}.{}", mod_name, ty_name),
                    NameInfo {
                        qualified_id: raw_id,
                        is_public: true,
                        defined_in_module: mod_name.to_string(),
                    },
                );
            }
        }
    }

    // 3. Resolve imports per module in topological order (supporting `pub use` re-exports!)
    //    and rewrite each module's AST into the unified qualified namespace.
    let mut all_defs: Vec<Def> = Vec::new();
    let mut import_errors: Vec<String> = Vec::new();

    for m_key in &topo_keys {
        let Some(m) = loader.get(m_key) else {
            continue;
        };
        let prefix = relative_module_prefix(m_key, entry_key);

        let (imap, errs) = resolve_imports_with_visibility(
            &m.imports,
            &prefix,
            &available_names,
            &mut interner.borrow_mut(),
        );

        for e in errs {
            match e {
                ImportError::PrivateItem { name, module } => {
                    import_errors.push(format!(
                        "cannot import private item '{}' from module '{}'",
                        name, module
                    ));
                }
                ImportError::NotFound { name, module } => {
                    import_errors.push(format!(
                        "item '{}' not found in module '{}'",
                        name, module
                    ));
                }
            }
        }

        // Handle `pub use` re-exports so downstream modules can import them!
        for (vis, use_item) in &m.imports {
            if !vis.is_public() {
                continue;
            }
            match use_item {
                UseItem::Single(p) => {
                    if let Some(name_id) = p.name() {
                        if let Some(&target_qid) = imap.resolve(&name_id) {
                            let local_str = interner.borrow().trace(name_id).to_string();
                            let export_key = if prefix.is_empty() {
                                local_str
                            } else {
                                format!("{}.{}", prefix, local_str)
                            };
                            available_names.insert(
                                export_key,
                                NameInfo {
                                    qualified_id: target_qid,
                                    is_public: true,
                                    defined_in_module: prefix.clone(),
                                },
                            );
                        }
                    }
                }
                UseItem::Alias(_, alias_id) => {
                    if let Some(&target_qid) = imap.resolve(alias_id) {
                        let local_str = interner.borrow().trace(*alias_id).to_string();
                        let export_key = if prefix.is_empty() {
                            local_str
                        } else {
                            format!("{}.{}", prefix, local_str)
                        };
                        available_names.insert(
                            export_key,
                            NameInfo {
                                qualified_id: target_qid,
                                is_public: true,
                                defined_in_module: prefix.clone(),
                            },
                        );
                    }
                }
                UseItem::Multiple(_, specs) => {
                    for spec in specs {
                        let lid = spec.local_name();
                        if let Some(&target_qid) = imap.resolve(&lid) {
                            let local_str = interner.borrow().trace(lid).to_string();
                            let export_key = if prefix.is_empty() {
                                local_str
                            } else {
                                format!("{}.{}", prefix, local_str)
                            };
                            available_names.insert(
                                export_key,
                                NameInfo {
                                    qualified_id: target_qid,
                                    is_public: true,
                                    defined_in_module: prefix.clone(),
                                },
                            );
                        }
                    }
                }
                UseItem::Glob(_) => {
                    for (&lid, &target_qid) in imap.iter() {
                        let local_str = interner.borrow().trace(lid).to_string();
                        let export_key = if prefix.is_empty() {
                            local_str
                        } else {
                            format!("{}.{}", prefix, local_str)
                        };
                        available_names.entry(export_key).or_insert(NameInfo {
                            qualified_id: target_qid,
                            is_public: true,
                            defined_in_module: prefix.clone(),
                        });
                    }
                }
            }
        }

        // Build combined symbol rewrite table for module `M`: imports first, then M's own definitions override glob imports
        let mut combined_val_map: HashMap<Id, Id> = HashMap::new();
        let mut combined_ty_map: HashMap<String, String> = HashMap::new();
        let mut combined_ctor_map: HashMap<String, String> = HashMap::new();

        for (&lid, &qid) in imap.iter() {
            combined_val_map.insert(lid, qid);
            let l_str = interner.borrow().trace(lid).to_string();
            let q_str = interner.borrow().trace(qid).to_string();
            if l_str.chars().next().map(|c| c.is_uppercase()).unwrap_or(false) {
                combined_ty_map.insert(l_str.clone(), q_str.clone());
                combined_ctor_map.insert(l_str, q_str);
            }
        }
        if let Some(own_vals) = module_local_renames.get(m_key) {
            for (&k, &v) in own_vals {
                combined_val_map.insert(k, v);
            }
        }
        if let Some(own_tys) = module_type_renames.get(m_key) {
            for (k, v) in own_tys {
                combined_ty_map.insert(k.clone(), v.clone());
            }
        }
        if let Some(own_ctors) = module_ctor_renames.get(m_key) {
            for (k, v) in own_ctors {
                combined_ctor_map.insert(k.clone(), v.clone());
            }
        }

        let ok_ctor = combined_ctor_map
            .get("Ok")
            .cloned()
            .unwrap_or_else(|| {
                if available_names.contains_key("std.result.Ok")
                    && !available_names.contains_key("Ok")
                {
                    "std.result.Ok".to_string()
                } else {
                    "Ok".to_string()
                }
            });
        let err_ctor = combined_ctor_map
            .get("Err")
            .cloned()
            .unwrap_or_else(|| {
                if available_names.contains_key("std.result.Err")
                    && !available_names.contains_key("Err")
                {
                    "std.result.Err".to_string()
                } else {
                    "Err".to_string()
                }
            });

        let mut module_defs: Vec<Def> = m.defs.iter().map(|(_, d)| d.clone()).collect();
        {
            let mut inter_mut = interner.borrow_mut();
            for d in &mut module_defs {
                desugar_try_in_def(d, &ok_ctor, &err_ctor, &mut inter_mut);
            }
        }

        let mut rewriter = ModuleAstRewriter {
            current_prefix: &prefix,
            val_map: &combined_val_map,
            ty_map: &combined_ty_map,
            ctor_map: &combined_ctor_map,
            available_names: &available_names,
            interner: &interner,
            errors: &mut import_errors,
        };

        for def in module_defs {
            let rewritten = rewriter.rewrite_def(def);
            all_defs.push(rewritten);
        }
    }

    if !import_errors.is_empty() {
        return Err(CompileError::Normal(import_errors.join("\n")));
    }

    let mut inter = interner.into_inner();

    // 4. Hindley-Milner + Concept type inference
    let (
        adt_registry,
        concept_registry,
        instance_registry,
        concept_methods,
        instantiation_registry,
    ) = {
        let env = Infer::new_env();
        let prelude = load_prelude(&mut inter, &env);
        let mut infer = Infer::new(&mut inter);
        infer
            .infer_defs(&prelude, &mut all_defs)
            .map_err(|te| CompileError::TypeError(te, single_source.clone()))?;
        (
            infer.adt_registry,
            infer.concept_registry,
            infer.instance_registry,
            infer.concept_methods,
            infer.instantiation_registry,
        )
    };

    // 5. K-conversion + demand-driven worklist monomorphization + static concept dispatch
    let k_out = K::go_full(
        all_defs,
        &mut inter,
        adt_registry.clone(),
        concept_registry,
        instance_registry,
        concept_methods,
        instantiation_registry,
        HashMap::new(),
    );

    // 5b. Verify that closures passed across threads/actors only capture `Send` values & globals
    verify_concurrent_closures(&k_out, &inter, &adt_registry)
        .map_err(|te| CompileError::TypeError(te, single_source))?;

    // 6. Fixed-point escape analysis
    let escape_info = crate::core::escape::analyze_all(&k_out.functions, &adt_registry, 128);

    // 7. LLVM 20 IR generation
    let mut top = k_out.functions;
    let main_id = inter.intern("main");
    let main_fn = top.remove(&main_id);

    let mut fn_entries: Vec<_> = top.into_iter().collect();
    fn_entries.sort_by_key(|(id, _)| inter.trace(*id).to_string());

    let mod_name = if entry_key.is_empty() {
        "geisha_module"
    } else {
        entry_key
    };
    let mut emitter = LLVMEmit::new(mod_name, &mut inter, adt_registry, escape_info);
    emitter.register_externs(&k_out.externs);
    emitter.declare_globals(&k_out.globals);

    let env = VarEnv::new();
    if let Some(mf) = main_fn {
        emitter.gen_main(mf.deref(), &k_out.globals, &env);
    }

    for (_, def) in fn_entries {
        emitter.gen_top_level(def.deref(), &env);
    }

    Ok(emitter.generator)
}

/// Interprocedural fixed-point verification that closures passed to `spawn` / `par*` / `Actor`
/// do not capture `!Send` local variables (`Cell`, `MutArray`, `Ptr`, `Vec`, etc.) or reference `!Send` globals.
fn verify_concurrent_closures(
    k_out: &KOutput,
    interner: &Interner,
    adt_registry: &AdtRegistry,
) -> Result<(), TypeError> {
    fn intrinsic_send_closure_arg(callee_name: &str) -> Option<usize> {
        match callee_name {
            "__concTaskSpawn" | "__concActorSpawn" => Some(0),
            "__concActorCall" | "__concActorSend" => Some(1),
            _ => None,
        }
    }

    fn collect_local_closures_and_aliases(
        term: &TaggedTerm,
        closures: &mut HashMap<Id, Closure>,
        aliases: &mut HashMap<Id, Id>,
    ) {
        match term.body() {
            Term::MakeCls(VarDecl(vid, _), cls, body) => {
                closures.insert(*vid, (**cls).clone());
                collect_local_closures_and_aliases(body, closures, aliases);
            }
            Term::Let(VarDecl(vid, _), val, body) => {
                if let Term::Var(rhs_id) = val.body() {
                    aliases.insert(*vid, *rhs_id);
                }
                collect_local_closures_and_aliases(val, closures, aliases);
                collect_local_closures_and_aliases(body, closures, aliases);
            }
            Term::Tuple(es) | Term::Block(es) => {
                for e in es {
                    collect_local_closures_and_aliases(e, closures, aliases);
                }
            }
            Term::ApplyDir(_, args) => {
                for a in args {
                    collect_local_closures_and_aliases(a, closures, aliases);
                }
            }
            Term::ApplyCls(c, args) => {
                collect_local_closures_and_aliases(c, closures, aliases);
                for a in args {
                    collect_local_closures_and_aliases(a, closures, aliases);
                }
            }
            Term::Binary(_, l, r) => {
                collect_local_closures_and_aliases(l, closures, aliases);
                collect_local_closures_and_aliases(r, closures, aliases);
            }
            Term::Unary(_, e) | Term::GetTag(e) | Term::GetField(e, _) => {
                collect_local_closures_and_aliases(e, closures, aliases);
            }
            Term::If(c, t, f) => {
                collect_local_closures_and_aliases(c, closures, aliases);
                collect_local_closures_and_aliases(t, closures, aliases);
                collect_local_closures_and_aliases(f, closures, aliases);
            }
            Term::While(c, b) => {
                collect_local_closures_and_aliases(c, closures, aliases);
                collect_local_closures_and_aliases(b, closures, aliases);
            }
            Term::Match(s, arms) => {
                collect_local_closures_and_aliases(s, closures, aliases);
                for (_, g, b) in arms {
                    if let Some(gt) = g {
                        collect_local_closures_and_aliases(gt, closures, aliases);
                    }
                    collect_local_closures_and_aliases(b, closures, aliases);
                }
            }
            Term::MakeData { fields, .. } => {
                for f in fields {
                    collect_local_closures_and_aliases(f, closures, aliases);
                }
            }
            Term::Lit(_) | Term::Var(_) | Term::GlobalVar(_) => {}
        }
    }

    fn resolve_alias(mut id: Id, aliases: &HashMap<Id, Id>) -> Id {
        let mut hops = 0;
        while let Some(&next) = aliases.get(&id) {
            if next == id || hops > 32 {
                break;
            }
            id = next;
            hops += 1;
        }
        id
    }

    #[derive(Clone)]
    enum ResolvedClosureRef {
        KnownClosure(Closure),
        ParamIdx(usize),
        Opaque(Type),
    }

    fn resolve_closure_arg(
        term: &TaggedTerm,
        param_map: &HashMap<Id, usize>,
        local_closures: &HashMap<Id, Closure>,
        aliases: &HashMap<Id, Id>,
    ) -> ResolvedClosureRef {
        match term.body() {
            Term::MakeCls(VarDecl(vid, _), cls, body) => {
                if let Term::Var(ret_id) = body.body() {
                    if ret_id == vid {
                        return ResolvedClosureRef::KnownClosure((**cls).clone());
                    }
                }
                resolve_closure_arg(body, param_map, local_closures, aliases)
            }
            Term::Let(_, _, body) => resolve_closure_arg(body, param_map, local_closures, aliases),
            Term::Block(es) if !es.is_empty() => {
                resolve_closure_arg(es.last().unwrap(), param_map, local_closures, aliases)
            }
            Term::Var(id) => {
                let root = resolve_alias(*id, aliases);
                if let Some(&pidx) = param_map.get(&root) {
                    ResolvedClosureRef::ParamIdx(pidx)
                } else if let Some(cls) = local_closures.get(&root) {
                    ResolvedClosureRef::KnownClosure(cls.clone())
                } else {
                    ResolvedClosureRef::Opaque(term.ref_scheme().body().clone())
                }
            }
            _ => ResolvedClosureRef::Opaque(term.ref_scheme().body().clone()),
        }
    }

    fn find_sink_args_in_term<'a>(
        term: &'a TaggedTerm,
        interner: &Interner,
        req_send_params: &HashMap<Id, HashSet<usize>>,
        out: &mut Vec<&'a TaggedTerm>,
    ) {
        match term.body() {
            Term::ApplyDir(VarDecl(callee_id, _), args) => {
                let cname = interner.trace(*callee_id);
                if let Some(idx) = intrinsic_send_closure_arg(cname) {
                    if idx < args.len() {
                        out.push(&args[idx]);
                    }
                }
                if let Some(indices) = req_send_params.get(callee_id) {
                    for &idx in indices {
                        if idx < args.len() {
                            out.push(&args[idx]);
                        }
                    }
                }
                for a in args {
                    find_sink_args_in_term(a, interner, req_send_params, out);
                }
            }
            Term::ApplyCls(c, args) => {
                find_sink_args_in_term(c, interner, req_send_params, out);
                for a in args {
                    find_sink_args_in_term(a, interner, req_send_params, out);
                }
            }
            Term::MakeCls(_, _, body) => {
                find_sink_args_in_term(body, interner, req_send_params, out);
            }
            Term::Let(_, val, body) => {
                find_sink_args_in_term(val, interner, req_send_params, out);
                find_sink_args_in_term(body, interner, req_send_params, out);
            }
            Term::Tuple(es) | Term::Block(es) => {
                for e in es {
                    find_sink_args_in_term(e, interner, req_send_params, out);
                }
            }
            Term::Binary(_, l, r) => {
                find_sink_args_in_term(l, interner, req_send_params, out);
                find_sink_args_in_term(r, interner, req_send_params, out);
            }
            Term::Unary(_, e) | Term::GetTag(e) | Term::GetField(e, _) => {
                find_sink_args_in_term(e, interner, req_send_params, out);
            }
            Term::If(c, t, f) => {
                find_sink_args_in_term(c, interner, req_send_params, out);
                find_sink_args_in_term(t, interner, req_send_params, out);
                find_sink_args_in_term(f, interner, req_send_params, out);
            }
            Term::While(c, b) => {
                find_sink_args_in_term(c, interner, req_send_params, out);
                find_sink_args_in_term(b, interner, req_send_params, out);
            }
            Term::Match(s, arms) => {
                find_sink_args_in_term(s, interner, req_send_params, out);
                for (_, g, b) in arms {
                    if let Some(gt) = g {
                        find_sink_args_in_term(gt, interner, req_send_params, out);
                    }
                    find_sink_args_in_term(b, interner, req_send_params, out);
                }
            }
            Term::MakeData { fields, .. } => {
                for f in fields {
                    find_sink_args_in_term(f, interner, req_send_params, out);
                }
            }
            Term::Lit(_) | Term::Var(_) | Term::GlobalVar(_) => {}
        }
    }

    // Precompute per-function local closure maps, alias maps, and parameter index maps
    let mut fn_local_closures: HashMap<Id, HashMap<Id, Closure>> = HashMap::new();
    let mut fn_aliases: HashMap<Id, HashMap<Id, Id>> = HashMap::new();
    let mut fn_param_maps: HashMap<Id, HashMap<Id, usize>> = HashMap::new();

    for (&fid, fdef) in &k_out.functions {
        let mut closures = HashMap::new();
        let mut aliases = HashMap::new();
        collect_local_closures_and_aliases(fdef.body(), &mut closures, &mut aliases);
        let pmap: HashMap<Id, usize> = fdef
            .parameters()
            .iter()
            .enumerate()
            .map(|(i, VarDecl(pid, _))| (*pid, i))
            .collect();
        fn_local_closures.insert(fid, closures);
        fn_aliases.insert(fid, aliases);
        fn_param_maps.insert(fid, pmap);
    }

    // 1. Fixed-point propagation of `RequiresSendClosure` across higher-order parameters
    let mut req_send_params: HashMap<Id, HashSet<usize>> = HashMap::new();
    let mut changed = true;
    let mut iter = 0;
    while changed && iter < 64 {
        changed = false;
        iter += 1;
        for (&fid, fdef) in &k_out.functions {
            let mut sink_args = Vec::new();
            find_sink_args_in_term(fdef.body(), interner, &req_send_params, &mut sink_args);
            let pmap = &fn_param_maps[&fid];
            let lcls = &fn_local_closures[&fid];
            let aliases = &fn_aliases[&fid];

            let mut cls_worklist: Vec<Closure> = Vec::new();
            for arg in sink_args {
                match resolve_closure_arg(arg, pmap, lcls, aliases) {
                    ResolvedClosureRef::ParamIdx(pidx) => {
                        if req_send_params.entry(fid).or_default().insert(pidx) {
                            changed = true;
                        }
                    }
                    ResolvedClosureRef::KnownClosure(cls) => cls_worklist.push(cls),
                    ResolvedClosureRef::Opaque(_) => {}
                }
            }

            let mut seen_entries = HashSet::new();
            while let Some(cls) = cls_worklist.pop() {
                if !seen_entries.insert(cls.entry()) {
                    continue;
                }
                for fv_id in cls.fv() {
                    let root = resolve_alias(fv_id, aliases);
                    if let Some(&pidx) = pmap.get(&root) {
                        let p_ty = fdef.parameters()[pidx].1.body();
                        if matches!(p_ty, Type::Arr(..)) {
                            if req_send_params.entry(fid).or_default().insert(pidx) {
                                changed = true;
                            }
                        }
                    } else if let Some(nested_cls) = lcls.get(&root) {
                        cls_worklist.push(nested_cls.clone());
                    }
                }
            }
        }
    }

    // 2. Validate all closures flowing into `SendClosure` sinks (both in functions and top-level global inits)
    let global_types: HashMap<Id, Type> = k_out
        .globals
        .iter()
        .map(|g| (g.name, g.ty.clone()))
        .collect();

    let mut verified_send_entries: HashSet<Id> = HashSet::new();

    for (&fid, fdef) in &k_out.functions {
        let mut sink_args = Vec::new();
        find_sink_args_in_term(fdef.body(), interner, &req_send_params, &mut sink_args);
        let pmap = &fn_param_maps[&fid];
        let lcls = &fn_local_closures[&fid];
        let aliases = &fn_aliases[&fid];
        let allowed_fn_params = req_send_params.get(&fid);

        let mut cls_worklist: Vec<Closure> = Vec::new();
        for arg in sink_args {
            match resolve_closure_arg(arg, pmap, lcls, aliases) {
                ResolvedClosureRef::ParamIdx(_) => {}
                ResolvedClosureRef::KnownClosure(cls) => cls_worklist.push(cls),
                ResolvedClosureRef::Opaque(ty) => {
                    return Err(TypeError::NoInstance {
                        concept: "Send".to_string(),
                        ty,
                        span: Span::default(),
                    });
                }
            }
        }

        let mut seen_local = HashSet::new();
        while let Some(cls) = cls_worklist.pop() {
            if !seen_local.insert(cls.entry()) {
                continue;
            }
            verified_send_entries.insert(cls.entry());
            let Some(entry_def) = k_out.functions.get(&cls.entry()) else {
                continue;
            };
            for (actual_id, VarDecl(_, formal_scm)) in cls.fv().into_iter().zip(entry_def.fv().iter())
            {
                let root = resolve_alias(actual_id, aliases);
                if let Some(&pidx) = pmap.get(&root) {
                    if allowed_fn_params.map_or(false, |s| s.contains(&pidx)) {
                        continue;
                    }
                }
                if let Some(nested_cls) = lcls.get(&root) {
                    cls_worklist.push(nested_cls.clone());
                    continue;
                }
                let fv_ty = formal_scm.body();
                if !Infer::is_concrete_type_send(fv_ty, adt_registry) {
                    return Err(TypeError::NoInstance {
                        concept: "Send".to_string(),
                        ty: fv_ty.clone(),
                        span: Span::default(),
                    });
                }
            }
        }
    }

    // 3. Ensure no spawned closure transitively reads a `!Send` top-level global (`def g = cellNew(0)`)
    fn check_globals_in_term(
        term: &TaggedTerm,
        k_out: &KOutput,
        global_types: &HashMap<Id, Type>,
        adt_registry: &AdtRegistry,
        visited_fns: &mut HashSet<Id>,
    ) -> Result<(), TypeError> {
        match term.body() {
            Term::Var(id) | Term::GlobalVar(id) => {
                if let Some(gty) = global_types.get(id) {
                    if !Infer::is_concrete_type_send(gty, adt_registry) {
                        return Err(TypeError::NoInstance {
                            concept: "Send".to_string(),
                            ty: gty.clone(),
                            span: Span::default(),
                        });
                    }
                }
                Ok(())
            }
            Term::MakeCls(_, cls, body) => {
                if visited_fns.insert(cls.entry()) {
                    if let Some(edef) = k_out.functions.get(&cls.entry()) {
                        check_globals_in_term(
                            edef.body(),
                            k_out,
                            global_types,
                            adt_registry,
                            visited_fns,
                        )?;
                    }
                }
                check_globals_in_term(body, k_out, global_types, adt_registry, visited_fns)
            }
            Term::ApplyDir(VarDecl(callee_id, _), args) => {
                if visited_fns.insert(*callee_id) {
                    if let Some(cdef) = k_out.functions.get(callee_id) {
                        check_globals_in_term(
                            cdef.body(),
                            k_out,
                            global_types,
                            adt_registry,
                            visited_fns,
                        )?;
                    }
                }
                for a in args {
                    check_globals_in_term(a, k_out, global_types, adt_registry, visited_fns)?;
                }
                Ok(())
            }
            Term::ApplyCls(c, args) => {
                check_globals_in_term(c, k_out, global_types, adt_registry, visited_fns)?;
                for a in args {
                    check_globals_in_term(a, k_out, global_types, adt_registry, visited_fns)?;
                }
                Ok(())
            }
            Term::Let(_, v, b) => {
                check_globals_in_term(v, k_out, global_types, adt_registry, visited_fns)?;
                check_globals_in_term(b, k_out, global_types, adt_registry, visited_fns)
            }
            Term::Tuple(es) | Term::Block(es) => {
                for e in es {
                    check_globals_in_term(e, k_out, global_types, adt_registry, visited_fns)?;
                }
                Ok(())
            }
            Term::Binary(_, l, r) => {
                check_globals_in_term(l, k_out, global_types, adt_registry, visited_fns)?;
                check_globals_in_term(r, k_out, global_types, adt_registry, visited_fns)
            }
            Term::Unary(_, e) | Term::GetTag(e) | Term::GetField(e, _) => {
                check_globals_in_term(e, k_out, global_types, adt_registry, visited_fns)
            }
            Term::If(c, t, f) => {
                check_globals_in_term(c, k_out, global_types, adt_registry, visited_fns)?;
                check_globals_in_term(t, k_out, global_types, adt_registry, visited_fns)?;
                check_globals_in_term(f, k_out, global_types, adt_registry, visited_fns)
            }
            Term::While(c, b) => {
                check_globals_in_term(c, k_out, global_types, adt_registry, visited_fns)?;
                check_globals_in_term(b, k_out, global_types, adt_registry, visited_fns)
            }
            Term::Match(s, arms) => {
                check_globals_in_term(s, k_out, global_types, adt_registry, visited_fns)?;
                for (_, g, b) in arms {
                    if let Some(gt) = g {
                        check_globals_in_term(gt, k_out, global_types, adt_registry, visited_fns)?;
                    }
                    check_globals_in_term(b, k_out, global_types, adt_registry, visited_fns)?;
                }
                Ok(())
            }
            Term::MakeData { fields, .. } => {
                for f in fields {
                    check_globals_in_term(f, k_out, global_types, adt_registry, visited_fns)?;
                }
                Ok(())
            }
            Term::Lit(_) => Ok(()),
        }
    }

    let mut visited_fns = HashSet::new();
    for entry_id in verified_send_entries {
        if visited_fns.insert(entry_id) {
            if let Some(edef) = k_out.functions.get(&entry_id) {
                check_globals_in_term(
                    edef.body(),
                    k_out,
                    &global_types,
                    adt_registry,
                    &mut visited_fns,
                )?;
            }
        }
    }

    Ok(())
}

struct ModuleAstRewriter<'a> {
    current_prefix: &'a str,
    val_map: &'a HashMap<Id, Id>,
    ty_map: &'a HashMap<String, String>,
    ctor_map: &'a HashMap<String, String>,
    available_names: &'a HashMap<String, NameInfo>,
    interner: &'a RefCell<Interner>,
    errors: &'a mut Vec<String>,
}

impl<'a> ModuleAstRewriter<'a> {
    fn rewrite_def(&mut self, mut def: Def) -> Def {
        if let Some(&new_id) = self.val_map.get(&def.ident) {
            def.ident = new_id;
        }
        match &mut def.node {
            Item::Form(form) => {
                let mut bound = HashSet::new();
                self.rewrite_form(form, &mut bound);
            }
            Item::Extern { ty, .. } => {
                *ty = self.rewrite_scheme(ty);
            }
            Item::Alias(_, scm) => {
                *scm = Box::new(self.rewrite_scheme(scm));
            }
            Item::Alg(_, variants) => {
                for v in variants {
                    if let Some(q_ctor) = self.ctor_map.get(&v.name) {
                        v.name = q_ctor.clone();
                    }
                    match &mut v.body {
                        VariantBody::Unit => {}
                        VariantBody::Tuple(fs) | VariantBody::Struct(fs) => {
                            for f in fs {
                                *f.ty = self.rewrite_type(&f.ty);
                            }
                        }
                    }
                }
            }
            Item::Concept {
                superclasses,
                methods,
                ..
            } => {
                for sc in superclasses {
                    if let Some(qc) = self.ty_map.get(&sc.concept) {
                        sc.concept = qc.clone();
                    }
                }
                for m in methods {
                    if let Some(&qid) = self.val_map.get(&m.name) {
                        m.name = qid;
                    }
                    m.ty = self.rewrite_scheme(&m.ty);
                }
            }
            Item::Instance {
                concept_name,
                type_args,
                constraints,
                methods,
            } => {
                if let Some(qc) = self.ty_map.get(concept_name) {
                    *concept_name = qc.clone();
                }
                for ta in type_args {
                    *ta = self.rewrite_type(ta);
                }
                for c in constraints {
                    if let Some(qc) = self.ty_map.get(&c.concept) {
                        c.concept = qc.clone();
                    }
                }
                for mi in methods {
                    let mut bound = HashSet::new();
                    self.rewrite_form(&mut mi.body, &mut bound);
                }
            }
        }
        def
    }

    fn rewrite_type(&self, ty: &Type) -> Type {
        match ty {
            Type::Con(n) => {
                if let Some(qn) = self.ty_map.get(n) {
                    Type::Con(qn.clone())
                } else {
                    ty.clone()
                }
            }
            Type::Arr(ps, r) => Type::Arr(
                ps.iter().map(|p| self.rewrite_type(p)).collect(),
                Box::new(self.rewrite_type(r)),
            ),
            Type::Tuple(es) => Type::Tuple(es.iter().map(|e| self.rewrite_type(e)).collect()),
            Type::Comp(b, a) => {
                Type::Comp(Box::new(self.rewrite_type(b)), Box::new(self.rewrite_type(a)))
            }
            Type::Void | Type::Var(_) => ty.clone(),
        }
    }

    fn rewrite_scheme(&self, scm: &Scheme) -> Scheme {
        match scm {
            Scheme::Mono(t) => Scheme::Mono(self.rewrite_type(t)),
            Scheme::Poly(vs, t) => Scheme::Poly(vs.clone(), self.rewrite_type(t)),
            Scheme::PolyConstrained(vs, cs, t) => {
                let new_cs = cs
                    .iter()
                    .map(|c| SchemeConstraint {
                        concept: self.ty_map.get(&c.concept).cloned().unwrap_or_else(|| c.concept.clone()),
                        type_arg: c.type_arg.clone(),
                    })
                    .collect();
                Scheme::PolyConstrained(vs.clone(), new_cs, self.rewrite_type(t))
            }
            Scheme::Slot => Scheme::Slot,
        }
    }

    fn rewrite_pattern(&self, pat: &mut Pattern, bound: &mut HashSet<Id>) {
        match pat {
            Pattern::Var(id) => {
                bound.insert(*id);
            }
            Pattern::Wildcard | Pattern::Lit(_) => {}
            Pattern::Tuple(ps) => {
                for p in ps {
                    self.rewrite_pattern(p, bound);
                }
            }
            Pattern::Constructor(cname, ps) => {
                if let Some(qc) = self.ctor_map.get(cname) {
                    *cname = qc.clone();
                }
                for p in ps {
                    self.rewrite_pattern(p, bound);
                }
            }
            Pattern::StructConstructor(cname, fs) => {
                if let Some(qc) = self.ctor_map.get(cname) {
                    *cname = qc.clone();
                }
                for (_, p) in fs {
                    self.rewrite_pattern(p, bound);
                }
            }
        }
    }

    fn rewrite_form(&mut self, form: &mut Form, bound: &mut HashSet<Id>) {
        if let Some(ref ann) = form.tag.annotate {
            form.tag.annotate = Some(self.rewrite_scheme(ann));
        }

        match &mut form.node {
            Expr::Lit(_) => {}
            Expr::Var(ref mut id) => {
                if !bound.contains(id) {
                    if let Some(&qid) = self.val_map.get(id) {
                        *id = qid;
                    }
                }
            }
            Expr::QualifiedVar(path) => {
                let raw_str = path.to_string_with(&self.interner.borrow());
                let rel_cand = if self.current_prefix.is_empty() {
                    raw_str.clone()
                } else {
                    format!("{}.{}", self.current_prefix, raw_str)
                };
                let resolved_info = self
                    .available_names
                    .get(&rel_cand)
                    .or_else(|| self.available_names.get(&raw_str))
                    .cloned();

                if let Some(info) = resolved_info {
                    if !info.is_public && info.defined_in_module != self.current_prefix {
                        let item_name = path
                            .name()
                            .map(|id| self.interner.borrow().trace(id).to_string())
                            .unwrap_or(raw_str);
                        self.errors.push(format!(
                            "cannot access private item '{}' from module '{}'",
                            item_name, info.defined_in_module
                        ));
                    }
                    form.node = Expr::Var(info.qualified_id);
                }
            }
            Expr::StructInit(ref mut cname, ref mut fs) => {
                if let Some(qc) = self.ctor_map.get(cname) {
                    *cname = qc.clone();
                }
                for (_, val) in fs {
                    self.rewrite_form(val, bound);
                }
            }
            Expr::Tuple(es) | Expr::List(es) | Expr::Block(es) => {
                for e in es {
                    self.rewrite_form(e, bound);
                }
            }
            Expr::Apply(callee, args) => {
                self.rewrite_form(callee, bound);
                for a in args {
                    self.rewrite_form(a, bound);
                }
            }
            Expr::Binary(_, l, r) => {
                self.rewrite_form(l, bound);
                self.rewrite_form(r, bound);
            }
            Expr::Unary(_, e) => self.rewrite_form(e, bound),
            Expr::Field(ref mut base, ref fname) => {
                // Check if `root.f1.f2...` (or a prefix of it) refers to a module-qualified item!
                fn flatten_chain(f: &Form) -> Option<(Id, Vec<(Span, String)>)> {
                    match &f.node {
                        Expr::Var(id) => Some((*id, vec![])),
                        Expr::Field(inner, name) => {
                            let (root, mut segs) = flatten_chain(inner)?;
                            segs.push((f.tag.pos, name.clone()));
                            Some((root, segs))
                        }
                        _ => None,
                    }
                }

                if let Some((root_id, mut segs)) = flatten_chain(base) {
                    segs.push((form.tag.pos, fname.clone()));
                    if !bound.contains(&root_id) && !self.val_map.contains_key(&root_id) {
                        let root_str = self.interner.borrow().trace(root_id).to_string();
                        if !self.available_names.contains_key(&root_str) {
                            // Try longest prefix `root.s0...sk` in `available_names`
                            let mut matched_k: Option<(usize, NameInfo)> = None;
                            for k in (0..segs.len()).rev() {
                                let suffix: Vec<&str> =
                                    segs[..=k].iter().map(|(_, s)| s.as_str()).collect();
                                let raw_str = format!("{}.{}", root_str, suffix.join("."));
                                let rel_cand = if self.current_prefix.is_empty() {
                                    raw_str.clone()
                                } else {
                                    format!("{}.{}", self.current_prefix, raw_str)
                                };
                                if let Some(info) = self
                                    .available_names
                                    .get(&rel_cand)
                                    .or_else(|| self.available_names.get(&raw_str))
                                    .cloned()
                                {
                                    matched_k = Some((k, info));
                                    break;
                                }
                            }

                            if let Some((k, info)) = matched_k {
                                if !info.is_public && info.defined_in_module != self.current_prefix
                                {
                                    self.errors.push(format!(
                                        "cannot access private item '{}' from module '{}'",
                                        segs[k].1, info.defined_in_module
                                    ));
                                }
                                let mut rebuilt =
                                    Form::new(segs[k].0, Expr::Var(info.qualified_id));
                                for (sp, rem_field) in segs.into_iter().skip(k + 1) {
                                    rebuilt =
                                        Form::new(sp, Expr::Field(Box::new(rebuilt), rem_field));
                                }
                                form.node = rebuilt.node;
                                return;
                            }

                            // Check if `root.s0...s_{last-1}` is a known module prefix (missing item error)
                            let mod_parts: Vec<&str> = std::iter::once(root_str.as_str())
                                .chain(segs[..segs.len() - 1].iter().map(|(_, s)| s.as_str()))
                                .collect();
                            let mod_cand = mod_parts.join(".");
                            let mod_dot = format!("{}.", mod_cand);
                            if self.available_names.keys().any(|k| k.starts_with(&mod_dot)) {
                                self.errors.push(format!(
                                    "item '{}' not found in module '{}'",
                                    segs.last().unwrap().1,
                                    mod_cand
                                ));
                                return;
                            }
                        }
                    }
                }

                self.rewrite_form(base, bound);
            }
            Expr::Try(e) => self.rewrite_form(e, bound),
            Expr::If(c, t, f) => {
                self.rewrite_form(c, bound);
                self.rewrite_form(t, bound);
                self.rewrite_form(f, bound);
            }
            Expr::While(c, b) => {
                self.rewrite_form(c, bound);
                self.rewrite_form(b, bound);
            }
            Expr::Abs(fun) => {
                let mut inner_bound = bound.clone();
                for VarDecl(pid, pscm) in &mut fun.param {
                    *pscm = self.rewrite_scheme(pscm);
                    inner_bound.insert(*pid);
                }
                if let Some(ref mut rt) = fun.ret_ty {
                    *rt = self.rewrite_type(rt);
                }
                self.rewrite_form(&mut fun.body, &mut inner_bound);
            }
            Expr::Let(VarDecl(vid, vscm), val, body) => {
                *vscm = self.rewrite_scheme(vscm);
                if matches!(val.node, Expr::Abs(..)) {
                    let mut rec_bound = bound.clone();
                    rec_bound.insert(*vid);
                    self.rewrite_form(val, &mut rec_bound);
                } else {
                    self.rewrite_form(val, bound);
                }
                let mut body_bound = bound.clone();
                body_bound.insert(*vid);
                self.rewrite_form(body, &mut body_bound);
            }
            Expr::Match(scrutinee, arms) => {
                self.rewrite_form(scrutinee, bound);
                for arm in arms {
                    let mut arm_bound = bound.clone();
                    self.rewrite_pattern(&mut arm.pattern, &mut arm_bound);
                    if let Some(ref mut g) = arm.guard {
                        self.rewrite_form(g, &mut arm_bound);
                    }
                    self.rewrite_form(&mut arm.body, &mut arm_bound);
                }
            }
        }
    }
}
