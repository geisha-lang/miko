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
        Term::Tuple(es) | Term::Block(es) => {
            for e in es {
                collect_vars_in_expr(e, out);
            }
        }
        Term::MakeData { fields, .. } => {
            for f in fields {
                collect_vars_in_expr(f, out);
            }
        }
        Term::If(_, t, f) => {
            collect_vars_in_expr(t, out);
            collect_vars_in_expr(f, out);
        }
        Term::While(c, b) => {
            collect_vars_in_expr(c, out);
            collect_vars_in_expr(b, out);
        }
        Term::Match(_, arms) => {
            for (_, _, b) in arms {
                collect_vars_in_expr(b, out);
            }
        }
        Term::Let(_, _, b) | Term::MakeCls(_, _, b) => {
            collect_vars_in_expr(b, out);
        }
        _ => {}
    }
}

fn analyze_param_escapes(
    func_def: &FunDef,
    known_escapes: &HashMap<Id, Vec<ParamEscape>>,
    recursive_adts: &HashSet<String>,
) -> Vec<ParamEscape> {
    let params = func_def.parameters();
    let mut escapes = vec![ParamEscape::NoEscape; params.len()];
    let param_ids: HashMap<Id, usize> = params
        .iter()
        .enumerate()
        .map(|(i, VarDecl(id, _))| (*id, i))
        .collect();

    analyze_param_escapes_in_term(
        func_def.body(),
        &param_ids,
        &mut escapes,
        known_escapes,
        recursive_adts,
        true,
    );
    escapes
}

fn analyze_param_escapes_in_term(
    term: &TaggedTerm,
    param_ids: &HashMap<Id, usize>,
    escapes: &mut Vec<ParamEscape>,
    known_escapes: &HashMap<Id, Vec<ParamEscape>>,
    recursive_adts: &HashSet<String>,
    in_tail: bool,
) {
    match term.body() {
        Term::Var(id) => {
            if in_tail {
                if let Some(&idx) = param_ids.get(id) {
                    escapes[idx] = escapes[idx].max(ParamEscape::EscapesReturn);
                }
            }
        }
        Term::Tuple(elems) => {
            for el in elems {
                analyze_param_escapes_in_term(
                    el.deref(),
                    param_ids,
                    escapes,
                    known_escapes,
                    recursive_adts,
                    in_tail,
                );
            }
        }
        Term::MakeCls(_, cls, body) => {
            for fv_id in cls.fv() {
                if let Some(&idx) = param_ids.get(&fv_id) {
                    escapes[idx] = escapes[idx].max(ParamEscape::EscapesCapture);
                }
            }
            analyze_param_escapes_in_term(
                body.deref(),
                param_ids,
                escapes,
                known_escapes,
                recursive_adts,
                in_tail,
            );
        }
        Term::Let(_, val, body) => {
            analyze_param_escapes_in_term(
                val.deref(),
                param_ids,
                escapes,
                known_escapes,
                recursive_adts,
                false,
            );
            analyze_param_escapes_in_term(
                body.deref(),
                param_ids,
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
                    for avid in arg_vars {
                        if let Some(&param_idx) = param_ids.get(&avid) {
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
                    }
                    analyze_param_escapes_in_term(
                        arg.deref(),
                        param_ids,
                        escapes,
                        known_escapes,
                        recursive_adts,
                        false,
                    );
                }
            } else {
                // External / intrinsic function (e.g. `cellNew`, `cellSet`, `arrayNew`, `arraySet`):
                // conservatively treat any passed parameter as escaping into heap storage!
                for arg in args {
                    let mut arg_vars = HashSet::new();
                    collect_vars_in_expr(arg, &mut arg_vars);
                    for avid in arg_vars {
                        if let Some(&param_idx) = param_ids.get(&avid) {
                            escapes[param_idx] = ParamEscape::EscapesStore;
                        }
                    }
                    analyze_param_escapes_in_term(
                        arg.deref(),
                        param_ids,
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
                param_ids,
                escapes,
                known_escapes,
                recursive_adts,
                false,
            );
            for arg in args {
                // Unknown closure may return or store its argument
                analyze_param_escapes_in_term(
                    arg.deref(),
                    param_ids,
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
                param_ids,
                escapes,
                known_escapes,
                recursive_adts,
                false,
            );
            analyze_param_escapes_in_term(
                t.deref(),
                param_ids,
                escapes,
                known_escapes,
                recursive_adts,
                in_tail,
            );
            analyze_param_escapes_in_term(
                f.deref(),
                param_ids,
                escapes,
                known_escapes,
                recursive_adts,
                in_tail,
            );
        }
        Term::While(c, b) => {
            analyze_param_escapes_in_term(
                c.deref(),
                param_ids,
                escapes,
                known_escapes,
                recursive_adts,
                false,
            );
            analyze_param_escapes_in_term(
                b.deref(),
                param_ids,
                escapes,
                known_escapes,
                recursive_adts,
                false,
            );
        }
        Term::Binary(_, l, r) => {
            analyze_param_escapes_in_term(
                l.deref(),
                param_ids,
                escapes,
                known_escapes,
                recursive_adts,
                false,
            );
            analyze_param_escapes_in_term(
                r.deref(),
                param_ids,
                escapes,
                known_escapes,
                recursive_adts,
                false,
            );
        }
        Term::Unary(_, e) | Term::GetTag(e) | Term::GetField(e, _) => {
            analyze_param_escapes_in_term(
                e.deref(),
                param_ids,
                escapes,
                known_escapes,
                recursive_adts,
                false,
            );
        }
        Term::Match(scrutinee, arms) => {
            // Note: matching an ADT and returning one of its extracted fields means a component of scrutinee may escape!
            if in_tail {
                let mut svars = HashSet::new();
                collect_vars_in_expr(scrutinee, &mut svars);
                for sv in svars {
                    if let Some(&idx) = param_ids.get(&sv) {
                        if escapes[idx] == ParamEscape::NoEscape {
                            escapes[idx] = ParamEscape::EscapesReturn;
                        }
                    }
                }
            }
            analyze_param_escapes_in_term(
                scrutinee.deref(),
                param_ids,
                escapes,
                known_escapes,
                recursive_adts,
                false,
            );
            for (_, guard, body) in arms {
                if let Some(ref g) = guard {
                    analyze_param_escapes_in_term(
                        g.deref(),
                        param_ids,
                        escapes,
                        known_escapes,
                        recursive_adts,
                        false,
                    );
                }
                analyze_param_escapes_in_term(
                    body.deref(),
                    param_ids,
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
                    param_ids,
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
                    param_ids,
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
            // If match is in tail position, an extracted field from `scrutinee` might be returned;
            // any inner allocation stored in `scrutinee` that returns must be on heap if `scrutinee` is destructured to return an inner pointer.
            if in_tail {
                if let Term::Var(sid) = scrutinee.body() {
                    if let Some(ainfo) = allocations.get(sid) {
                        for &inner in &ainfo.contained_vars {
                            if allocations.contains_key(&inner) {
                                escaping.insert(inner);
                            }
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
