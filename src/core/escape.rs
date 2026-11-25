/// Interprocedural + fixed-point intraprocedural escape analysis for safe stack allocation.

use std::collections::{HashMap, HashSet};
use std::ops::Deref;

use crate::core::term::*;
use crate::internal::VarDecl;
use crate::types::{compute_recursive_adts, AdtRegistry};
use crate::utils::{Id, P};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AllocationStrategy {
    Stack,
    Heap,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum ParamEscape {
    NoEscape,
    EscapesReturn,
    EscapesCapture,
    EscapesStore,
}

#[derive(Clone, Debug)]
pub struct FunctionEscapeInfo {
    pub param_escapes: Vec<ParamEscape>,
    pub allocations: HashMap<Id, AllocationStrategy>,
}

impl FunctionEscapeInfo {
    fn new(param_count: usize) -> Self {
        FunctionEscapeInfo {
            param_escapes: vec![ParamEscape::NoEscape; param_count],
            allocations: HashMap::new(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct EscapeAnalysis {
    pub functions: HashMap<Id, FunctionEscapeInfo>,
    recursive_adts: HashSet<String>,
    #[allow(dead_code)]
    size_threshold: usize,
}

impl EscapeAnalysis {
    pub fn get_strategy(&self, func_id: &Id, binding_id: &Id) -> AllocationStrategy {
        self.functions
            .get(func_id)
            .and_then(|info| info.allocations.get(binding_id))
            .copied()
            .unwrap_or(AllocationStrategy::Heap)
    }

    pub fn is_recursive_adt(&self, adt_name: &str) -> bool {
        self.recursive_adts.contains(adt_name)
    }

    pub fn has_stack_allocations(&self, func_id: &Id) -> bool {
        self.functions
            .get(func_id)
            .map(|info| {
                info.allocations
                    .values()
                    .any(|&s| s == AllocationStrategy::Stack)
            })
            .unwrap_or(false)
    }
}

pub fn analyze_all(
    functions: &HashMap<Id, P<FunDef>>,
    adt_registry: &AdtRegistry,
    size_threshold: usize,
) -> EscapeAnalysis {
    // Detect both self-recursive and mutually recursive ADTs via graph reachability
    let recursive_adts = compute_recursive_adts(adt_registry);

    // Phase 1: Interprocedural parameter escape fixed-point
    let mut param_escapes: HashMap<Id, Vec<ParamEscape>> = HashMap::new();
    for (func_id, func_def) in functions {
        param_escapes.insert(
            *func_id,
            vec![ParamEscape::NoEscape; func_def.parameters().len()],
        );
    }

    let mut changed = true;
    let mut iterations = 0;
    while changed && iterations < 100 {
        changed = false;
        iterations += 1;
        for (func_id, func_def) in functions {
            let old = param_escapes.get(func_id).cloned().unwrap_or_default();
            let next = analyze_param_escapes(func_def, &param_escapes, &recursive_adts);
            if next != old {
                changed = true;
                param_escapes.insert(*func_id, next);
            }
        }
    }

    // Phase 2: Intraprocedural fixed-point allocation analysis
    let mut analysis = EscapeAnalysis {
        functions: HashMap::new(),
        recursive_adts,
        size_threshold,
    };

    for (func_id, func_def) in functions {
        let f_pes = param_escapes.get(func_id).cloned().unwrap_or_default();
        let info = analyze_local_allocations(
            func_def,
            &f_pes,
            &param_escapes,
            &analysis.recursive_adts,
        );
        analysis.functions.insert(*func_id, info);
    }

    analysis
}

fn collect_vars_in_expr(term: &TaggedTerm, out: &mut HashSet<Id>) {
    match term.body() {
        Term::Var(id) => {
            out.insert(*id);
        }
        Term::Tuple(es) | Term::Block(es) | Term::ApplyDir(_, es) => {
            for e in es {
                collect_vars_in_expr(e, out);
            }
        }
        Term::ApplyCls(c, args) => {
            collect_vars_in_expr(c, out);
            for a in args {
                collect_vars_in_expr(a, out);
            }
        }
        Term::MakeData { fields, .. } => {
            for f in fields {
                collect_vars_in_expr(f, out);
            }
        }
        Term::If(c, t, f) => {
            collect_vars_in_expr(c, out);
            collect_vars_in_expr(t, out);
            collect_vars_in_expr(f, out);
        }
        Term::While(c, b) => {
            collect_vars_in_expr(c, out);
            collect_vars_in_expr(b, out);
        }
        Term::Match(s, arms) => {
            collect_vars_in_expr(s, out);
            for (_, g, b) in arms {
                if let Some(gt) = g {
                    collect_vars_in_expr(gt, out);
                }
                collect_vars_in_expr(b, out);
            }
        }
        Term::Let(_, v, b) => {
            collect_vars_in_expr(v, out);
            collect_vars_in_expr(b, out);
        }
        Term::MakeCls(_, cls, b) => {
            for fv in cls.fv() {
                out.insert(fv);
            }
            collect_vars_in_expr(b, out);
        }
        Term::Unary(_, e) | Term::GetTag(e) | Term::GetField(e, _) => {
            collect_vars_in_expr(e, out);
        }
        Term::Binary(_, l, r) => {
            collect_vars_in_expr(l, out);
            collect_vars_in_expr(r, out);
        }
        _ => {}
    }
}

fn collect_pattern_vars(pat: & crate::syntax::form::Pattern, out: &mut Vec<Id>) {
    use crate::syntax::form::Pattern;
    match pat {
        Pattern::Var(id) => out.push(*id),
        Pattern::Wildcard | Pattern::Lit(_) => {}
        Pattern::Tuple(ps) | Pattern::Constructor(_, ps) => {
            for p in ps {
                collect_pattern_vars(p, out);
            }
        }
        Pattern::StructConstructor(_, fs) => {
            for (_, p) in fs {
                collect_pattern_vars(p, out);
            }
        }
    }
}

fn analyze_param_escapes(
    func_def: &FunDef,
    known_escapes: &HashMap<Id, Vec<ParamEscape>>,
    recursive_adts: &HashSet<String>,
) -> Vec<ParamEscape> {
    let params = func_def.parameters();
    let mut escapes = vec![ParamEscape::NoEscape; params.len()];
    let mut param_taints: HashMap<Id, HashSet<usize>> = HashMap::new();
    for (i, VarDecl(id, _)) in params.iter().enumerate() {
        let mut s = HashSet::new();
        s.insert(i);
        param_taints.insert(*id, s);
    }

    analyze_param_escapes_in_term(
        func_def.body(),
        &mut param_taints,
        &mut escapes,
        known_escapes,
        recursive_adts,
        true,
    );
    escapes
}

fn taint_union_for_vars(
    vars: &HashSet<Id>,
    param_taints: &HashMap<Id, HashSet<usize>>,
) -> HashSet<usize> {
    let mut u = HashSet::new();
    for v in vars {
        if let Some(ts) = param_taints.get(v) {
            u.extend(ts.iter().copied());
        }
    }
    u
}

fn analyze_param_escapes_in_term(
    term: &TaggedTerm,
    param_taints: &mut HashMap<Id, HashSet<usize>>,
    escapes: &mut Vec<ParamEscape>,
    known_escapes: &HashMap<Id, Vec<ParamEscape>>,
    recursive_adts: &HashSet<String>,
    in_tail: bool,
) {
    match term.body() {
        Term::Var(id) => {
            if in_tail {
                if let Some(tset) = param_taints.get(id) {
                    for &idx in tset {
                        escapes[idx] = escapes[idx].max(ParamEscape::EscapesReturn);
                    }
                }
            }
        }
        Term::Tuple(elems) => {
            for el in elems {
                analyze_param_escapes_in_term(
                    el.deref(),
                    param_taints,
                    escapes,
                    known_escapes,
                    recursive_adts,
                    in_tail,
                );
            }
        }
        Term::MakeCls(VarDecl(cls_var_id, _), cls, body) => {
            let mut cls_taint = HashSet::new();
            for fv_id in cls.fv() {
                if let Some(tset) = param_taints.get(&fv_id) {
                    for &idx in tset {
                        escapes[idx] = escapes[idx].max(ParamEscape::EscapesCapture);
                        cls_taint.insert(idx);
                    }
                }
            }
            if !cls_taint.is_empty() {
                param_taints.insert(*cls_var_id, cls_taint);
            }
            analyze_param_escapes_in_term(
                body.deref(),
                param_taints,
                escapes,
                known_escapes,
                recursive_adts,
                in_tail,
            );
        }
        Term::Let(VarDecl(lhs_id, _), val, body) => {
            let mut rhs_vars = HashSet::new();
            collect_vars_in_expr(val, &mut rhs_vars);
            let rhs_taint = taint_union_for_vars(&rhs_vars, param_taints);
            if !rhs_taint.is_empty() {
                param_taints.insert(*lhs_id, rhs_taint);
            }
            analyze_param_escapes_in_term(
                val.deref(),
                param_taints,
                escapes,
                known_escapes,
                recursive_adts,
                false,
            );
            analyze_param_escapes_in_term(
                body.deref(),
                param_taints,
                escapes,
                known_escapes,
                recursive_adts,
                in_tail,
            );
        }
        Term::ApplyDir(VarDecl(callee_id, _), args) => {
            if let Some(callee_escapes) = known_escapes.get(callee_id) {
                for (i, arg) in args.iter().enumerate() {
                    let mut arg_vars = HashSet::new();
                    collect_vars_in_expr(arg, &mut arg_vars);
                    let arg_taints = taint_union_for_vars(&arg_vars, param_taints);
                    for param_idx in arg_taints {
                        if i < callee_escapes.len() {
                            match callee_escapes[i] {
                                ParamEscape::EscapesCapture | ParamEscape::EscapesStore => {
                                    escapes[param_idx] =
                                        escapes[param_idx].max(ParamEscape::EscapesStore);
                                }
                                ParamEscape::EscapesReturn if in_tail => {
                                    escapes[param_idx] =
                                        escapes[param_idx].max(ParamEscape::EscapesReturn);
                                }
                                _ => {}
                            }
                        }
                    }
                    analyze_param_escapes_in_term(
                        arg.deref(),
                        param_taints,
                        escapes,
                        known_escapes,
                        recursive_adts,
                        false,
                    );
                }
            } else {
                // External / intrinsic function (e.g. `cellNew`, `cellSet`, `arrayNew`, `arraySet`):
                // conservatively treat any passed parameter (or local derived from it) as escaping into heap storage!
                for arg in args {
                    let mut arg_vars = HashSet::new();
                    collect_vars_in_expr(arg, &mut arg_vars);
                    let arg_taints = taint_union_for_vars(&arg_vars, param_taints);
                    for param_idx in arg_taints {
                        escapes[param_idx] = ParamEscape::EscapesStore;
                    }
                    analyze_param_escapes_in_term(
                        arg.deref(),
                        param_taints,
                        escapes,
                        known_escapes,
                        recursive_adts,
                        true,
                    );
                }
            }
        }
        Term::ApplyCls(callee, args) => {
            analyze_param_escapes_in_term(
                callee.deref(),
                param_taints,
                escapes,
                known_escapes,
                recursive_adts,
                false,
            );
            for arg in args {
                let mut arg_vars = HashSet::new();
                collect_vars_in_expr(arg, &mut arg_vars);
                for param_idx in taint_union_for_vars(&arg_vars, param_taints) {
                    escapes[param_idx] = ParamEscape::EscapesStore;
                }
                analyze_param_escapes_in_term(
                    arg.deref(),
                    param_taints,
                    escapes,
                    known_escapes,
                    recursive_adts,
                    true,
                );
            }
        }
        Term::If(c, t, f) => {
            analyze_param_escapes_in_term(
                c.deref(),
                param_taints,
                escapes,
                known_escapes,
                recursive_adts,
                false,
            );
            analyze_param_escapes_in_term(
                t.deref(),
                param_taints,
                escapes,
                known_escapes,
                recursive_adts,
                in_tail,
            );
            analyze_param_escapes_in_term(
                f.deref(),
                param_taints,
                escapes,
                known_escapes,
                recursive_adts,
                in_tail,
            );
        }
        Term::While(c, b) => {
            analyze_param_escapes_in_term(
                c.deref(),
                param_taints,
                escapes,
                known_escapes,
                recursive_adts,
                false,
            );
            analyze_param_escapes_in_term(
                b.deref(),
                param_taints,
                escapes,
                known_escapes,
                recursive_adts,
                false,
            );
        }
        Term::Binary(_, l, r) => {
            analyze_param_escapes_in_term(
                l.deref(),
                param_taints,
                escapes,
                known_escapes,
                recursive_adts,
                false,
            );
            analyze_param_escapes_in_term(
                r.deref(),
                param_taints,
                escapes,
                known_escapes,
                recursive_adts,
                false,
            );
        }
        Term::Unary(_, e) | Term::GetTag(e) | Term::GetField(e, _) => {
            analyze_param_escapes_in_term(
                e.deref(),
                param_taints,
                escapes,
                known_escapes,
                recursive_adts,
                false,
            );
        }
        Term::Match(scrutinee, arms) => {
            let mut svars = HashSet::new();
            collect_vars_in_expr(scrutinee, &mut svars);
            let scrut_taints = taint_union_for_vars(&svars, param_taints);
            if in_tail {
                for &idx in &scrut_taints {
                    escapes[idx] = escapes[idx].max(ParamEscape::EscapesReturn);
                }
            }
            analyze_param_escapes_in_term(
                scrutinee.deref(),
                param_taints,
                escapes,
                known_escapes,
                recursive_adts,
                false,
            );
            for (pat, guard, body) in arms {
                if !scrut_taints.is_empty() {
                    let mut pvars = Vec::new();
                    collect_pattern_vars(pat, &mut pvars);
                    for pv in pvars {
                        param_taints
                            .entry(pv)
                            .or_default()
                            .extend(scrut_taints.iter().copied());
                    }
                }
                if let Some(ref g) = guard {
                    analyze_param_escapes_in_term(
                        g.deref(),
                        param_taints,
                        escapes,
                        known_escapes,
                        recursive_adts,
                        false,
                    );
                }
                analyze_param_escapes_in_term(
                    body.deref(),
                    param_taints,
                    escapes,
                    known_escapes,
                    recursive_adts,
                    in_tail,
                );
            }
        }
        Term::MakeData {
            type_name, fields, ..
        } => {
            let is_rec = recursive_adts.contains(type_name);
            for field in fields {
                analyze_param_escapes_in_term(
                    field.deref(),
                    param_taints,
                    escapes,
                    known_escapes,
                    recursive_adts,
                    is_rec || in_tail,
                );
            }
        }
        Term::Block(items) => {
            for (i, item) in items.iter().enumerate() {
                let is_last = i + 1 == items.len();
                analyze_param_escapes_in_term(
                    item.deref(),
                    param_taints,
                    escapes,
                    known_escapes,
                    recursive_adts,
                    in_tail && is_last,
                );
            }
        }
        Term::Lit(_) | Term::GlobalVar(_) => {}
    }
}

#[derive(Clone, Debug)]
struct AllocInfo {
    is_recursive_adt: bool,
    /// Local bindings directly stored inside this allocation (fields of `MakeData` or captured vars of `MakeCls`)
    contained_vars: Vec<Id>,
}

fn analyze_local_allocations(
    func_def: &FunDef,
    param_escapes: &[ParamEscape],
    all_param_escapes: &HashMap<Id, Vec<ParamEscape>>,
    recursive_adts: &HashSet<String>,
) -> FunctionEscapeInfo {
    let mut info = FunctionEscapeInfo::new(func_def.parameters().len());
    info.param_escapes = param_escapes.to_vec();

    let mut allocations: HashMap<Id, AllocInfo> = HashMap::new();
    collect_allocations(func_def.body(), &mut allocations, recursive_adts);

    let mut escaping: HashSet<Id> = HashSet::new();
    for (id, ainfo) in &allocations {
        if ainfo.is_recursive_adt {
            escaping.insert(*id);
        }
    }

    // Initial escape seed pass
    analyze_allocation_escapes(
        func_def.body(),
        &allocations,
        &mut escaping,
        all_param_escapes,
        recursive_adts,
        true,
    );

    // Fixed-point containment propagation:
    // If outer allocation `outer_id` escapes, EVERY allocation `inner_id` stored in its fields or captured in its closure MUST ALSO escape!
    loop {
        let prev_len = escaping.len();
        let current_escaping: Vec<Id> = escaping.iter().copied().collect();
        for outer_id in current_escaping {
            if let Some(ainfo) = allocations.get(&outer_id) {
                for &inner_id in &ainfo.contained_vars {
                    if allocations.contains_key(&inner_id) {
                        escaping.insert(inner_id);
                    }
                }
            }
        }
        if escaping.len() == prev_len {
            break;
        }
    }

    for (id, _) in &allocations {
        let strategy = if escaping.contains(id) {
            AllocationStrategy::Heap
        } else {
            AllocationStrategy::Stack
        };
        info.allocations.insert(*id, strategy);
    }

    info
}

fn collect_allocations(
    term: &TaggedTerm,
    allocations: &mut HashMap<Id, AllocInfo>,
    recursive_adts: &HashSet<String>,
) {
    match term.body() {
        Term::MakeCls(VarDecl(id, _), cls, body) => {
            allocations.insert(
                *id,
                AllocInfo {
                    is_recursive_adt: false,
                    contained_vars: cls.fv(),
                },
            );
            collect_allocations(body.deref(), allocations, recursive_adts);
        }
        Term::Let(VarDecl(id, _), val, body) => {
            if let Term::MakeData {
                type_name, fields, ..
            } = val.body()
            {
                let mut contained = HashSet::new();
                for f in fields {
                    collect_vars_in_expr(f, &mut contained);
                }
                allocations.insert(
                    *id,
                    AllocInfo {
                        is_recursive_adt: recursive_adts.contains(type_name),
                        contained_vars: contained.into_iter().collect(),
                    },
                );
            }
            collect_allocations(val.deref(), allocations, recursive_adts);
            collect_allocations(body.deref(), allocations, recursive_adts);
        }
        Term::If(c, t, f) => {
            collect_allocations(c.deref(), allocations, recursive_adts);
            collect_allocations(t.deref(), allocations, recursive_adts);
            collect_allocations(f.deref(), allocations, recursive_adts);
        }
        Term::While(c, b) => {
            collect_allocations(c.deref(), allocations, recursive_adts);
            collect_allocations(b.deref(), allocations, recursive_adts);
        }
        Term::Binary(_, l, r) => {
            collect_allocations(l.deref(), allocations, recursive_adts);
            collect_allocations(r.deref(), allocations, recursive_adts);
        }
        Term::Unary(_, e) | Term::GetTag(e) | Term::GetField(e, _) => {
            collect_allocations(e.deref(), allocations, recursive_adts);
        }
        Term::Match(scrutinee, arms) => {
            collect_allocations(scrutinee.deref(), allocations, recursive_adts);
            for (_, guard, body) in arms {
                if let Some(ref g) = guard {
                    collect_allocations(g.deref(), allocations, recursive_adts);
                }
                collect_allocations(body.deref(), allocations, recursive_adts);
            }
        }
        Term::ApplyDir(_, args) => {
            for arg in args {
                collect_allocations(arg.deref(), allocations, recursive_adts);
            }
        }
        Term::ApplyCls(callee, args) => {
            collect_allocations(callee.deref(), allocations, recursive_adts);
            for arg in args {
                collect_allocations(arg.deref(), allocations, recursive_adts);
            }
        }
        Term::MakeData { fields, .. } | Term::Tuple(fields) | Term::Block(fields) => {
            for f in fields {
                collect_allocations(f.deref(), allocations, recursive_adts);
            }
        }
        Term::Lit(_) | Term::Var(_) | Term::GlobalVar(_) => {}
    }
}

fn analyze_allocation_escapes(
    term: &TaggedTerm,
    allocations: &HashMap<Id, AllocInfo>,
    escaping: &mut HashSet<Id>,
    all_param_escapes: &HashMap<Id, Vec<ParamEscape>>,
    recursive_adts: &HashSet<String>,
    in_tail: bool,
) {
    match term.body() {
        Term::Var(id) => {
            if in_tail && allocations.contains_key(id) {
                escaping.insert(*id);
            }
        }
        Term::Tuple(elems) => {
            for el in elems {
                analyze_allocation_escapes(
                    el.deref(),
                    allocations,
                    escaping,
                    all_param_escapes,
                    recursive_adts,
                    in_tail,
                );
            }
        }
        Term::MakeCls(_, cls, body) => {
            for fv_id in cls.fv() {
                if allocations.contains_key(&fv_id) {
                    escaping.insert(fv_id);
                }
            }
            analyze_allocation_escapes(
                body.deref(),
                allocations,
                escaping,
                all_param_escapes,
                recursive_adts,
                in_tail,
            );
        }
        Term::Let(VarDecl(var_id, _), val, body) => {
            // Note: if `val` is a `MakeData` directly bound to `var_id`, its fields only escape if `var_id` escapes
            // (handled by fixed-point propagation over `contained_vars`!).
            // Any anonymous `MakeData` nested inside sub-expressions of `val` is heap-allocated, so its fields DO escape!
            if let Term::MakeData { fields, .. } = val.body() {
                for f in fields {
                    analyze_allocation_escapes(
                        f.deref(),
                        allocations,
                        escaping,
                        all_param_escapes,
                        recursive_adts,
                        false,
                    );
                }
            } else {
                // If `val` aliases or extracts an existing allocation (e.g. `let b = a`), conservatively mark if `b` is used
                let mut aliased = HashSet::new();
                collect_vars_in_expr(val, &mut aliased);
                for aid in aliased {
                    if allocations.contains_key(&aid) {
                        escaping.insert(aid);
                    }
                }
                analyze_allocation_escapes(
                    val.deref(),
                    allocations,
                    escaping,
                    all_param_escapes,
                    recursive_adts,
                    false,
                );
            }
            let _ = var_id;
            analyze_allocation_escapes(
                body.deref(),
                allocations,
                escaping,
                all_param_escapes,
                recursive_adts,
                in_tail,
            );
        }
        Term::ApplyDir(VarDecl(callee_id, _), args) => {
            if let Some(callee_escapes) = all_param_escapes.get(callee_id) {
                for (i, arg) in args.iter().enumerate() {
                    let arg_escapes = if i < callee_escapes.len() {
                        match callee_escapes[i] {
                            ParamEscape::EscapesCapture | ParamEscape::EscapesStore => true,
                            ParamEscape::EscapesReturn => in_tail,
                            ParamEscape::NoEscape => false,
                        }
                    } else {
                        true
                    };
                    if arg_escapes {
                        let mut avars = HashSet::new();
                        collect_vars_in_expr(arg, &mut avars);
                        for av in avars {
                            if allocations.contains_key(&av) {
                                escaping.insert(av);
                            }
                        }
                    }
                    analyze_allocation_escapes(
                        arg.deref(),
                        allocations,
                        escaping,
                        all_param_escapes,
                        recursive_adts,
                        arg_escapes,
                    );
                }
            } else {
                // Unknown external/runtime/intrinsic function (e.g. `cellNew`, `cellSet`, `arrayNew`, `arraySet`):
                // mark any passed allocations (including nested tuple/ADT vars) as escaping!
                for arg in args {
                    let mut avars = HashSet::new();
                    collect_vars_in_expr(arg, &mut avars);
                    for av in avars {
                        if allocations.contains_key(&av) {
                            escaping.insert(av);
                        }
                    }
                    analyze_allocation_escapes(
                        arg.deref(),
                        allocations,
                        escaping,
                        all_param_escapes,
                        recursive_adts,
                        true,
                    );
                }
            }
        }
        Term::ApplyCls(callee, args) => {
            analyze_allocation_escapes(
                callee.deref(),
                allocations,
                escaping,
                all_param_escapes,
                recursive_adts,
                false,
            );
            for arg in args {
                analyze_allocation_escapes(
                    arg.deref(),
                    allocations,
                    escaping,
                    all_param_escapes,
                    recursive_adts,
                    true,
                );
            }
        }
        Term::If(c, t, f) => {
            analyze_allocation_escapes(
                c.deref(),
                allocations,
                escaping,
                all_param_escapes,
                recursive_adts,
                false,
            );
            analyze_allocation_escapes(
                t.deref(),
                allocations,
                escaping,
                all_param_escapes,
                recursive_adts,
                in_tail,
            );
            analyze_allocation_escapes(
                f.deref(),
                allocations,
                escaping,
                all_param_escapes,
                recursive_adts,
                in_tail,
            );
        }
        Term::While(c, b) => {
            analyze_allocation_escapes(
                c.deref(),
                allocations,
                escaping,
                all_param_escapes,
                recursive_adts,
                false,
            );
            analyze_allocation_escapes(
                b.deref(),
                allocations,
                escaping,
                all_param_escapes,
                recursive_adts,
                false,
            );
        }
        Term::Binary(_, l, r) => {
            analyze_allocation_escapes(
                l.deref(),
                allocations,
                escaping,
                all_param_escapes,
                recursive_adts,
                false,
            );
            analyze_allocation_escapes(
                r.deref(),
                allocations,
                escaping,
                all_param_escapes,
                recursive_adts,
                false,
            );
        }
        Term::Unary(_, e) | Term::GetTag(e) | Term::GetField(e, _) => {
            analyze_allocation_escapes(
                e.deref(),
                allocations,
                escaping,
                all_param_escapes,
                recursive_adts,
                false,
            );
        }
        Term::Match(scrutinee, arms) => {
            // Any pattern-bound variables extracted from `scrutinee` may outlive `scrutinee` (or be stored/returned);
            // if `scrutinee` is NOT a direct `Term::Var(sid)` of a single local allocation with wildcards-only,
            // conservatively mark allocations referenced in `scrutinee` (and their contained inner allocations when bound or in_tail)!
            let has_pat_bindings = arms.iter().any(|(pat, _, _)| {
                let mut pvs = Vec::new();
                collect_pattern_vars(pat, &mut pvs);
                !pvs.is_empty()
            });
            if in_tail || has_pat_bindings {
                if let Term::Var(sid) = scrutinee.body() {
                    if let Some(ainfo) = allocations.get(sid) {
                        for &inner in &ainfo.contained_vars {
                            if allocations.contains_key(&inner) {
                                escaping.insert(inner);
                            }
                        }
                    }
                } else {
                    let mut svars = HashSet::new();
                    collect_vars_in_expr(scrutinee, &mut svars);
                    for sv in svars {
                        if allocations.contains_key(&sv) {
                            escaping.insert(sv);
                        }
                    }
                }
            }
            analyze_allocation_escapes(
                scrutinee.deref(),
                allocations,
                escaping,
                all_param_escapes,
                recursive_adts,
                false,
            );
            for (_, guard, body) in arms {
                if let Some(ref g) = guard {
                    analyze_allocation_escapes(
                        g.deref(),
                        allocations,
                        escaping,
                        all_param_escapes,
                        recursive_adts,
                        false,
                    );
                }
                analyze_allocation_escapes(
                    body.deref(),
                    allocations,
                    escaping,
                    all_param_escapes,
                    recursive_adts,
                    in_tail,
                );
            }
        }
        Term::MakeData { fields, .. } => {
            // Anonymous MakeData (not directly bound to a Let) is always Heap-allocated,
            // so any local allocation stored into its fields escapes onto the heap!
            for field in fields {
                analyze_allocation_escapes(
                    field.deref(),
                    allocations,
                    escaping,
                    all_param_escapes,
                    recursive_adts,
                    true,
                );
            }
        }
        Term::Block(items) => {
            for (i, item) in items.iter().enumerate() {
                let is_last = i + 1 == items.len();
                analyze_allocation_escapes(
                    item.deref(),
                    allocations,
                    escaping,
                    all_param_escapes,
                    recursive_adts,
                    in_tail && is_last,
                );
            }
        }
        Term::Lit(_) | Term::GlobalVar(_) => {}
    }
}
