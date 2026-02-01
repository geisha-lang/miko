/// Escape analysis for stack allocation optimization
///
/// This module performs interprocedural escape analysis to determine which
/// allocations (closures and ADTs) can safely be stack-allocated instead of
/// using gc_alloc().
///
/// A value escapes (must be heap-allocated) if:
/// 1. Captured by a closure (pointer captured, pointee must survive)
/// 2. Returned from function (in tail position)
/// 3. Stored in an escaping ADT
/// 4. Passed to an escaping parameter of a called function
/// 5. Is a recursive ADT (can grow unboundedly)
/// 6. Size > threshold

use std::collections::{HashMap, HashSet};
use std::ops::Deref;

use crate::core::term::*;
use crate::internal::VarDecl;
use crate::types::{AdtRegistry, Type, Scheme};
use crate::utils::{Id, P};

/// Allocation strategy for a value
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AllocationStrategy {
    /// Allocate on stack (fast, no GC overhead)
    Stack,
    /// Allocate on heap via gc_alloc (required for escaping values)
    Heap,
}

/// How a parameter escapes from its function
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ParamEscape {
    /// Parameter does not escape
    NoEscape,
    /// Parameter is captured by a closure created in this function
    EscapesCapture,
    /// Parameter is returned from the function
    EscapesReturn,
    /// Parameter is stored in a heap-allocated ADT
    EscapesStore,
}

impl ParamEscape {
    fn escapes(&self) -> bool {
        !matches!(self, ParamEscape::NoEscape)
    }
}

/// Escape information for a single function
#[derive(Clone, Debug)]
pub struct FunctionEscapeInfo {
    /// How each parameter escapes (indexed by parameter position)
    pub param_escapes: Vec<ParamEscape>,
    /// Allocation strategy for local bindings (by binding Id)
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

/// Global escape analysis result
#[derive(Clone, Debug)]
pub struct EscapeAnalysis {
    /// Per-function escape information
    pub functions: HashMap<Id, FunctionEscapeInfo>,
    /// Set of recursive ADT names (always heap-allocated)
    recursive_adts: HashSet<String>,
    /// Size threshold for stack allocation (bytes)
    size_threshold: usize,
}

impl EscapeAnalysis {
    /// Get allocation strategy for a binding in a function
    pub fn get_strategy(&self, func_id: &Id, binding_id: &Id) -> AllocationStrategy {
        self.functions
            .get(func_id)
            .and_then(|info| info.allocations.get(binding_id))
            .copied()
            .unwrap_or(AllocationStrategy::Heap) // Default to heap if unknown
    }

    /// Check if an ADT is recursive
    pub fn is_recursive_adt(&self, adt_name: &str) -> bool {
        self.recursive_adts.contains(adt_name)
    }
}

/// Perform escape analysis on all functions
pub fn analyze_all(
    functions: &HashMap<Id, P<FunDef>>,
    adt_registry: &AdtRegistry,
    size_threshold: usize,
) -> EscapeAnalysis {
    // Build set of recursive ADTs
    let recursive_adts: HashSet<String> = adt_registry
        .iter()
        .filter(|(_, info)| info.is_recursive())
        .map(|(name, _)| name.clone())
        .collect();

    // Build call graph for interprocedural analysis
    let call_graph = build_call_graph(functions);

    // Phase 1: Compute parameter escape properties (bottom-up)
    let mut param_escapes: HashMap<Id, Vec<ParamEscape>> = HashMap::new();

    // Initialize all functions with NoEscape for all parameters
    for (func_id, func_def) in functions {
        let param_count = func_def.parameters().len();
        param_escapes.insert(*func_id, vec![ParamEscape::NoEscape; param_count]);
    }

    // Iterate until fixed point (simple worklist algorithm)
    let mut changed = true;
    let mut iterations = 0;
    const MAX_ITERATIONS: usize = 100;

    while changed && iterations < MAX_ITERATIONS {
        changed = false;
        iterations += 1;

        for (func_id, func_def) in functions {
            let old_escapes = param_escapes.get(func_id).cloned().unwrap_or_default();
            let new_escapes = analyze_param_escapes(
                func_def,
                &param_escapes,
                &recursive_adts,
            );

            if new_escapes != old_escapes {
                changed = true;
                param_escapes.insert(*func_id, new_escapes);
            }
        }
    }

    // Phase 2: Determine local allocation strategies
    let mut analysis = EscapeAnalysis {
        functions: HashMap::new(),
        recursive_adts,
        size_threshold,
    };

    for (func_id, func_def) in functions {
        let func_param_escapes = param_escapes.get(func_id).cloned().unwrap_or_default();
        let func_info = analyze_local_allocations(
            func_def,
            &func_param_escapes,
            &param_escapes,
            &analysis.recursive_adts,
            size_threshold,
        );
        analysis.functions.insert(*func_id, func_info);
    }

    analysis
}

/// Build a simple call graph: function -> set of called functions
fn build_call_graph(functions: &HashMap<Id, P<FunDef>>) -> HashMap<Id, HashSet<Id>> {
    let mut graph: HashMap<Id, HashSet<Id>> = HashMap::new();

    for (func_id, func_def) in functions {
        let mut callees = HashSet::new();
        collect_callees(func_def.body(), &mut callees);
        graph.insert(*func_id, callees);
    }

    graph
}

/// Collect all function Ids called from a term
fn collect_callees(term: &TaggedTerm, callees: &mut HashSet<Id>) {
    match term.body() {
        Term::ApplyDir(VarDecl(id, _), args) => {
            callees.insert(*id);
            for arg in args {
                collect_callees(arg.deref(), callees);
            }
        }
        Term::ApplyCls(callee, args) => {
            collect_callees(callee.deref(), callees);
            for arg in args {
                collect_callees(arg.deref(), callees);
            }
        }
        Term::Let(_, val, body) => {
            collect_callees(val.deref(), callees);
            collect_callees(body.deref(), callees);
        }
        Term::MakeCls(_, _, body) => {
            collect_callees(body.deref(), callees);
        }
        Term::If(c, t, f) => {
            collect_callees(c.deref(), callees);
            collect_callees(t.deref(), callees);
            collect_callees(f.deref(), callees);
        }
        Term::Binary(_, l, r) => {
            collect_callees(l.deref(), callees);
            collect_callees(r.deref(), callees);
        }
        Term::Unary(_, e) => {
            collect_callees(e.deref(), callees);
        }
        Term::Match(scrutinee, arms) => {
            collect_callees(scrutinee.deref(), callees);
            for (_, _, body) in arms {
                collect_callees(body.deref(), callees);
            }
        }
        Term::MakeData { fields, .. } => {
            for f in fields {
                collect_callees(f.deref(), callees);
            }
        }
        Term::GetTag(node) | Term::GetField(node, _) => {
            collect_callees(node.deref(), callees);
        }
        Term::Block(items) | Term::List(items) => {
            for item in items {
                collect_callees(item.deref(), callees);
            }
        }
        Term::Lit(_) | Term::Var(_) => {}
    }
}

/// Analyze how parameters escape from a function
fn analyze_param_escapes(
    func_def: &FunDef,
    known_escapes: &HashMap<Id, Vec<ParamEscape>>,
    recursive_adts: &HashSet<String>,
) -> Vec<ParamEscape> {
    let params = func_def.parameters();
    let mut escapes = vec![ParamEscape::NoEscape; params.len()];

    // Build param id -> index mapping
    let param_ids: HashMap<Id, usize> = params
        .iter()
        .enumerate()
        .map(|(i, VarDecl(id, _))| (*id, i))
        .collect();

    // Analyze the body
    analyze_param_escapes_in_term(
        func_def.body(),
        &param_ids,
        &mut escapes,
        known_escapes,
        recursive_adts,
        true, // Initial position is tail
    );

    escapes
}

/// Analyze parameter escapes in a term
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
            // If a parameter is in tail position, it escapes via return
            if in_tail {
                if let Some(&idx) = param_ids.get(id) {
                    escapes[idx] = ParamEscape::EscapesReturn;
                }
            }
        }
        Term::MakeCls(_, cls, body) => {
            // Parameters captured by closure escape
            for fv_id in cls.fv() {
                if let Some(&idx) = param_ids.get(&fv_id) {
                    // Check if the parameter is a pointer type that would escape
                    // For now, conservatively mark all captured params as escaping
                    if escapes[idx] == ParamEscape::NoEscape {
                        escapes[idx] = ParamEscape::EscapesCapture;
                    }
                }
            }
            analyze_param_escapes_in_term(body.deref(), param_ids, escapes, known_escapes, recursive_adts, in_tail);
        }
        Term::Let(_, val, body) => {
            // Value is not in tail position
            analyze_param_escapes_in_term(val.deref(), param_ids, escapes, known_escapes, recursive_adts, false);
            // Body inherits tail position
            analyze_param_escapes_in_term(body.deref(), param_ids, escapes, known_escapes, recursive_adts, in_tail);
        }
        Term::ApplyDir(VarDecl(callee_id, _), args) => {
            // Check if arguments are passed to escaping parameters
            if let Some(callee_escapes) = known_escapes.get(callee_id) {
                for (i, arg) in args.iter().enumerate() {
                    if let Term::Var(arg_id) = arg.body() {
                        if let Some(&param_idx) = param_ids.get(arg_id) {
                            if i < callee_escapes.len() {
                                match callee_escapes[i] {
                                    ParamEscape::EscapesCapture | ParamEscape::EscapesStore => {
                                        // Arg escapes unconditionally
                                        if escapes[param_idx] == ParamEscape::NoEscape {
                                            escapes[param_idx] = ParamEscape::EscapesStore;
                                        }
                                    }
                                    ParamEscape::EscapesReturn => {
                                        // Arg escapes only if call result escapes
                                        if in_tail && escapes[param_idx] == ParamEscape::NoEscape {
                                            escapes[param_idx] = ParamEscape::EscapesReturn;
                                        }
                                    }
                                    ParamEscape::NoEscape => {}
                                }
                            }
                        }
                    }
                    // Recurse into non-variable args
                    analyze_param_escapes_in_term(arg.deref(), param_ids, escapes, known_escapes, recursive_adts, false);
                }
            } else {
                // Unknown callee - conservatively analyze args
                for arg in args {
                    analyze_param_escapes_in_term(arg.deref(), param_ids, escapes, known_escapes, recursive_adts, false);
                }
            }
        }
        Term::ApplyCls(callee, args) => {
            // Closure call - can't know param escapes, analyze args conservatively
            analyze_param_escapes_in_term(callee.deref(), param_ids, escapes, known_escapes, recursive_adts, false);
            for arg in args {
                analyze_param_escapes_in_term(arg.deref(), param_ids, escapes, known_escapes, recursive_adts, false);
            }
        }
        Term::If(c, t, f) => {
            analyze_param_escapes_in_term(c.deref(), param_ids, escapes, known_escapes, recursive_adts, false);
            // Both branches inherit tail position
            analyze_param_escapes_in_term(t.deref(), param_ids, escapes, known_escapes, recursive_adts, in_tail);
            analyze_param_escapes_in_term(f.deref(), param_ids, escapes, known_escapes, recursive_adts, in_tail);
        }
        Term::Binary(_, l, r) => {
            analyze_param_escapes_in_term(l.deref(), param_ids, escapes, known_escapes, recursive_adts, false);
            analyze_param_escapes_in_term(r.deref(), param_ids, escapes, known_escapes, recursive_adts, false);
        }
        Term::Unary(_, e) => {
            analyze_param_escapes_in_term(e.deref(), param_ids, escapes, known_escapes, recursive_adts, false);
        }
        Term::Match(scrutinee, arms) => {
            analyze_param_escapes_in_term(scrutinee.deref(), param_ids, escapes, known_escapes, recursive_adts, false);
            for (_, _, body) in arms {
                // All arms inherit tail position
                analyze_param_escapes_in_term(body.deref(), param_ids, escapes, known_escapes, recursive_adts, in_tail);
            }
        }
        Term::MakeData { type_name, fields, .. } => {
            let is_recursive = recursive_adts.contains(type_name);
            for field in fields {
                if let Term::Var(fid) = field.body() {
                    if let Some(&idx) = param_ids.get(fid) {
                        // If stored in recursive ADT or in tail position, escapes
                        if is_recursive || in_tail {
                            if escapes[idx] == ParamEscape::NoEscape {
                                escapes[idx] = ParamEscape::EscapesStore;
                            }
                        }
                    }
                }
                analyze_param_escapes_in_term(field.deref(), param_ids, escapes, known_escapes, recursive_adts, false);
            }
        }
        Term::GetTag(node) | Term::GetField(node, _) => {
            analyze_param_escapes_in_term(node.deref(), param_ids, escapes, known_escapes, recursive_adts, false);
        }
        Term::Block(items) => {
            for (i, item) in items.iter().enumerate() {
                let is_last = i == items.len() - 1;
                analyze_param_escapes_in_term(item.deref(), param_ids, escapes, known_escapes, recursive_adts, in_tail && is_last);
            }
        }
        Term::List(items) => {
            for item in items {
                analyze_param_escapes_in_term(item.deref(), param_ids, escapes, known_escapes, recursive_adts, false);
            }
        }
        Term::Lit(_) => {}
    }
}

/// Analyze local allocations in a function to determine stack vs heap
fn analyze_local_allocations(
    func_def: &FunDef,
    param_escapes: &[ParamEscape],
    all_param_escapes: &HashMap<Id, Vec<ParamEscape>>,
    recursive_adts: &HashSet<String>,
    size_threshold: usize,
) -> FunctionEscapeInfo {
    let param_count = func_def.parameters().len();
    let mut info = FunctionEscapeInfo::new(param_count);
    info.param_escapes = param_escapes.to_vec();

    // Collect all local allocations (MakeCls, MakeData bindings)
    let mut allocations: HashMap<Id, AllocInfo> = HashMap::new();
    collect_allocations(func_def.body(), &mut allocations, recursive_adts);

    // Track which allocations escape
    let mut escaping: HashSet<Id> = HashSet::new();

    // Analyze escaping
    analyze_allocation_escapes(
        func_def.body(),
        &allocations,
        &mut escaping,
        all_param_escapes,
        recursive_adts,
        true, // start in tail position
    );

    // Determine strategies
    for (id, alloc_info) in &allocations {
        let strategy = if escaping.contains(id) {
            AllocationStrategy::Heap
        } else if alloc_info.is_recursive_adt {
            AllocationStrategy::Heap // Recursive ADTs always on heap
        } else {
            // Could be stack-allocated
            AllocationStrategy::Stack
        };
        info.allocations.insert(*id, strategy);
    }

    info
}

/// Information about a local allocation
#[derive(Clone, Debug)]
struct AllocInfo {
    /// Is this a MakeCls (closure)?
    is_closure: bool,
    /// Is this a recursive ADT?
    is_recursive_adt: bool,
    /// Free variables (for closures)
    free_vars: Vec<Id>,
}

/// Collect all allocation sites in a term
fn collect_allocations(
    term: &TaggedTerm,
    allocations: &mut HashMap<Id, AllocInfo>,
    recursive_adts: &HashSet<String>,
) {
    match term.body() {
        Term::MakeCls(VarDecl(id, _), cls, body) => {
            allocations.insert(*id, AllocInfo {
                is_closure: true,
                is_recursive_adt: false,
                free_vars: cls.fv(),
            });
            collect_allocations(body.deref(), allocations, recursive_adts);
        }
        Term::Let(VarDecl(id, _), val, body) => {
            // Check if val is MakeData
            if let Term::MakeData { type_name, .. } = val.body() {
                let is_recursive = recursive_adts.contains(type_name);
                allocations.insert(*id, AllocInfo {
                    is_closure: false,
                    is_recursive_adt: is_recursive,
                    free_vars: vec![],
                });
            }
            collect_allocations(val.deref(), allocations, recursive_adts);
            collect_allocations(body.deref(), allocations, recursive_adts);
        }
        Term::If(c, t, f) => {
            collect_allocations(c.deref(), allocations, recursive_adts);
            collect_allocations(t.deref(), allocations, recursive_adts);
            collect_allocations(f.deref(), allocations, recursive_adts);
        }
        Term::Binary(_, l, r) => {
            collect_allocations(l.deref(), allocations, recursive_adts);
            collect_allocations(r.deref(), allocations, recursive_adts);
        }
        Term::Unary(_, e) => {
            collect_allocations(e.deref(), allocations, recursive_adts);
        }
        Term::Match(scrutinee, arms) => {
            collect_allocations(scrutinee.deref(), allocations, recursive_adts);
            for (_, _, body) in arms {
                collect_allocations(body.deref(), allocations, recursive_adts);
            }
        }
        Term::ApplyDir(_, args) | Term::ApplyCls(_, args) => {
            if let Term::ApplyCls(callee, _) = term.body() {
                collect_allocations(callee.deref(), allocations, recursive_adts);
            }
            for arg in args {
                collect_allocations(arg.deref(), allocations, recursive_adts);
            }
        }
        Term::MakeData { fields, .. } => {
            for f in fields {
                collect_allocations(f.deref(), allocations, recursive_adts);
            }
        }
        Term::GetTag(node) | Term::GetField(node, _) => {
            collect_allocations(node.deref(), allocations, recursive_adts);
        }
        Term::Block(items) | Term::List(items) => {
            for item in items {
                collect_allocations(item.deref(), allocations, recursive_adts);
            }
        }
        Term::Lit(_) | Term::Var(_) => {}
    }
}

/// Analyze which allocations escape
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
            // If an allocation is in tail position, it escapes
            if in_tail && allocations.contains_key(id) {
                escaping.insert(*id);
            }
        }
        Term::MakeCls(VarDecl(cls_id, _), cls, body) => {
            // Check if the closure itself escapes (in tail position of body)
            // Values captured by the closure must be on heap if they're pointer types
            for fv_id in cls.fv() {
                if allocations.contains_key(&fv_id) {
                    // Captured allocation escapes
                    escaping.insert(fv_id);
                }
            }
            analyze_allocation_escapes(body.deref(), allocations, escaping, all_param_escapes, recursive_adts, in_tail);
        }
        Term::Let(VarDecl(var_id, _), val, body) => {
            // Check if this binding is for a MakeData
            if let Term::MakeData { type_name, fields, .. } = val.body() {
                let is_recursive = recursive_adts.contains(type_name);
                if is_recursive {
                    // Recursive ADT always escapes
                    escaping.insert(*var_id);
                }
                // Check if any field is an escaping allocation
                for field in fields {
                    if let Term::Var(fid) = field.body() {
                        if escaping.contains(fid) {
                            // This ADT contains an escaping value, so it must escape too
                            escaping.insert(*var_id);
                        }
                    }
                }
            }
            analyze_allocation_escapes(val.deref(), allocations, escaping, all_param_escapes, recursive_adts, false);
            analyze_allocation_escapes(body.deref(), allocations, escaping, all_param_escapes, recursive_adts, in_tail);
        }
        Term::ApplyDir(VarDecl(callee_id, _), args) => {
            // Check if arguments passed to escaping parameters
            if let Some(callee_escapes) = all_param_escapes.get(callee_id) {
                for (i, arg) in args.iter().enumerate() {
                    if let Term::Var(arg_id) = arg.body() {
                        if allocations.contains_key(arg_id) && i < callee_escapes.len() {
                            match callee_escapes[i] {
                                ParamEscape::EscapesCapture | ParamEscape::EscapesStore => {
                                    escaping.insert(*arg_id);
                                }
                                ParamEscape::EscapesReturn => {
                                    // Only escapes if call result escapes (i.e., in tail)
                                    if in_tail {
                                        escaping.insert(*arg_id);
                                    }
                                }
                                ParamEscape::NoEscape => {}
                            }
                        }
                    }
                    analyze_allocation_escapes(arg.deref(), allocations, escaping, all_param_escapes, recursive_adts, false);
                }
            } else {
                // Unknown callee, analyze args conservatively
                for arg in args {
                    analyze_allocation_escapes(arg.deref(), allocations, escaping, all_param_escapes, recursive_adts, false);
                }
            }
        }
        Term::ApplyCls(callee, args) => {
            // Closure call - can't know param escapes, but analyze subterms
            analyze_allocation_escapes(callee.deref(), allocations, escaping, all_param_escapes, recursive_adts, false);
            for arg in args {
                // Conservative: assume args to unknown closure might escape
                if let Term::Var(arg_id) = arg.body() {
                    if allocations.contains_key(arg_id) {
                        escaping.insert(*arg_id);
                    }
                }
                analyze_allocation_escapes(arg.deref(), allocations, escaping, all_param_escapes, recursive_adts, false);
            }
        }
        Term::If(c, t, f) => {
            analyze_allocation_escapes(c.deref(), allocations, escaping, all_param_escapes, recursive_adts, false);
            analyze_allocation_escapes(t.deref(), allocations, escaping, all_param_escapes, recursive_adts, in_tail);
            analyze_allocation_escapes(f.deref(), allocations, escaping, all_param_escapes, recursive_adts, in_tail);
        }
        Term::Binary(_, l, r) => {
            analyze_allocation_escapes(l.deref(), allocations, escaping, all_param_escapes, recursive_adts, false);
            analyze_allocation_escapes(r.deref(), allocations, escaping, all_param_escapes, recursive_adts, false);
        }
        Term::Unary(_, e) => {
            analyze_allocation_escapes(e.deref(), allocations, escaping, all_param_escapes, recursive_adts, false);
        }
        Term::Match(scrutinee, arms) => {
            analyze_allocation_escapes(scrutinee.deref(), allocations, escaping, all_param_escapes, recursive_adts, false);
            for (_, _, body) in arms {
                analyze_allocation_escapes(body.deref(), allocations, escaping, all_param_escapes, recursive_adts, in_tail);
            }
        }
        Term::MakeData { type_name, fields, .. } => {
            // If this MakeData is in tail position (not bound to a variable),
            // it will be heap allocated anyway
            let is_recursive = recursive_adts.contains(type_name);
            for field in fields {
                if let Term::Var(fid) = field.body() {
                    if allocations.contains_key(fid) {
                        if is_recursive || in_tail {
                            // Stored in escaping ADT
                            escaping.insert(*fid);
                        }
                    }
                }
                analyze_allocation_escapes(field.deref(), allocations, escaping, all_param_escapes, recursive_adts, false);
            }
        }
        Term::GetTag(node) | Term::GetField(node, _) => {
            analyze_allocation_escapes(node.deref(), allocations, escaping, all_param_escapes, recursive_adts, false);
        }
        Term::Block(items) => {
            for (i, item) in items.iter().enumerate() {
                let is_last = i == items.len() - 1;
                analyze_allocation_escapes(item.deref(), allocations, escaping, all_param_escapes, recursive_adts, in_tail && is_last);
            }
        }
        Term::List(items) => {
            for item in items {
                analyze_allocation_escapes(item.deref(), allocations, escaping, all_param_escapes, recursive_adts, false);
            }
        }
        Term::Lit(_) => {}
    }
}
