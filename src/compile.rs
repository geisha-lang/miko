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

        let mut rewriter = ModuleAstRewriter {
            current_prefix: &prefix,
            val_map: &combined_val_map,
            ty_map: &combined_ty_map,
            ctor_map: &combined_ctor_map,
            available_names: &available_names,
            interner: &interner,
            errors: &mut import_errors,
        };

        for (_, def) in &m.defs {
            let rewritten = rewriter.rewrite_def(def.clone());
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
            .map_err(|te| CompileError::TypeError(te, single_source))?;
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
