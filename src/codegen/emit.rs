use std::collections::HashMap;
use std::ops::Deref;

use crate::codegen::llvm::*;
use crate::core::escape::{AllocationStrategy, EscapeAnalysis};
use crate::core::*;
use crate::internal::*;
use crate::syntax::form::Pattern;
use crate::types::*;
use crate::utils::*;
use llvm_sys::analysis::LLVMVerifierFailureAction;

/// Explicit variable slot tracking both pointer `LLVMValue` and stored element `LLVMType`
/// (eliminates all `LLVMGetElementType` calls for LLVM 20 opaque pointers).
#[derive(Clone, Copy, Debug)]
pub struct VarSlot {
    pub ptr: LLVMValue,
    pub ty: LLVMType,
}

pub type VarEnv<'a> = SymTable<'a, Id, VarSlot>;

pub trait EmitProvider {
    fn gen_module<T>(&mut self, module: T)
    where
        T: IntoIterator<Item = FunDef>;
}

pub struct LLVMEmit<'i> {
    pub generator: LLVMCodegen,
    interner: &'i mut Interner,
    escape_info: EscapeAnalysis,
    current_func: Option<Id>,
    /// Set ONLY for the immediate RHS of a `Let` binding; consumed immediately via `.take()` so it never leaks to subexpressions!
    pending_let_binding: Option<Id>,
    global_slots: HashMap<Id, (VarSlot, Type)>,
    extern_fns: HashMap<Id, (Name, Type)>,
    cli_globals: Option<(LLVMValue, LLVMValue)>,
}

impl<'i> LLVMEmit<'i> {
    pub fn new(
        name: &str,
        interner: &'i mut Interner,
        adt_registry: AdtRegistry,
        escape_info: EscapeAnalysis,
    ) -> Self {
        LLVMEmit {
            generator: LLVMCodegen::new(name, adt_registry),
            interner,
            escape_info,
            current_func: None,
            pending_let_binding: None,
            global_slots: HashMap::new(),
            extern_fns: HashMap::new(),
            cli_globals: None,
        }
    }

    pub fn register_externs(&mut self, externs: &HashMap<Id, (Name, Type)>) {
        self.extern_fns = externs.clone();
    }

    fn get_or_declare_cli_globals(&mut self) -> (LLVMValue, LLVMValue) {
        if let Some(pair) = self.cli_globals {
            return pair;
        }
        let i32_ty = self.context().get_int32_type();
        let ptr_ty = self.context().get_ptr_type();
        let argc_g = self.generator.module.add_global(&i32_ty, "__gs_argc");
        let argv_g = self.generator.module.add_global(&ptr_ty, "__gs_argv");
        self.cli_globals = Some((argc_g, argv_g));
        (argc_g, argv_g)
    }

    fn get_allocation_strategy(&self, binding_id: &Id) -> AllocationStrategy {
        if let Some(func_id) = &self.current_func {
            self.escape_info.get_strategy(func_id, binding_id)
        } else {
            AllocationStrategy::Heap
        }
    }

    pub fn dump(&mut self) {
        self.generator.module.dump();
    }

    /// Declare LLVM globals for all top-level value definitions (`def pi = 3.14`) before emitting functions.
    pub fn declare_globals(&mut self, globals: &[GlobalValDef]) {
        for g in globals {
            let gname = format!("__gval_{}", self.interner.trace(g.name));
            let val_llvm_ty = self.generator.get_val_llvm_type(&g.ty);
            let global_ptr = self.generator.module.add_global(&val_llvm_ty, &gname);
            self.global_slots.insert(
                g.name,
                (
                    VarSlot {
                        ptr: global_ptr,
                        ty: val_llvm_ty,
                    },
                    g.ty.clone(),
                ),
            );
        }
    }

    pub fn gen_top_level(&mut self, def: &FunDef, prelude: &VarEnv) {
        self.current_func = Some(def.name());
        self.pending_let_binding = None;

        let fun_type = def.ref_type();
        let void_ret = match fun_type {
            Type::Arr(_, ret) => {
                matches!(ret.as_ref(), Type::Void)
                    || matches!(ret.as_ref(), Type::Con(n) if n == "Void")
            }
            _ => false,
        };

        let def_name = self.interner.trace(def.name()).to_owned();
        let fun = self.generator.get_or_add_function(&def_name, fun_type);
        let arg_count = fun.count_params();

        if fun.count_basic_blocks() != 0 {
            panic!("Redefined function: {}", def_name);
        }
        let entry_blk = self.context().append_basic_block(&fun, "entry");
        self.builder().set_position_at_end(&entry_blk);

        let mut symtbl = prelude.sub_env();

        // Bind regular parameters cleanly (1-to-1 with explicit arity!)
        let param_defs = def.parameters();
        for (i, VarDecl(pname, pscm)) in param_defs.iter().enumerate() {
            let p_llvm_ty = self.generator.get_val_llvm_type(pscm.body());
            let trace_name = self.interner.trace(*pname).to_owned();
            let arg = fun.get_param(i);
            arg.set_name(&trace_name);
            let slot_ptr = self
                .generator
                .create_entry_block_alloca(&fun, &trace_name, &p_llvm_ty);
            self.builder().store(&arg, &slot_ptr);
            symtbl.insert(
                *pname,
                VarSlot {
                    ptr: slot_ptr,
                    ty: p_llvm_ty,
                },
            );
        }

        // Unpack closure environment from trailing `ptr %fvs` parameter
        let p_fvs = fun.get_param(arg_count - 1);
        p_fvs.set_name("fvs");

        let formal_fvs = def.fv();
        if !formal_fvs.is_empty() {
            let fv_llvm_tys: Vec<LLVMType> = formal_fvs
                .iter()
                .map(|VarDecl(_, scm)| self.generator.get_val_llvm_type(scm.body()))
                .collect();
            let fv_struct_ty = self.context().get_struct_type(&fv_llvm_tys, false);

            for (i, VarDecl(fv_id, _)) in formal_fvs.iter().enumerate() {
                let elem_ty = fv_llvm_tys[i];
                let fv_name = self.interner.trace(*fv_id).to_owned();
                let slot_ptr = self
                    .generator
                    .create_entry_block_alloca(&fun, &fv_name, &elem_ty);
                let field_ptr =
                    self.builder()
                        .struct_field_ptr(&fv_struct_ty, &p_fvs, i, "fv.gep");
                let fv_val = self.builder().load(&elem_ty, &field_ptr, &fv_name);
                self.builder().store(&fv_val, &slot_ptr);
                symtbl.insert(
                    *fv_id,
                    VarSlot {
                        ptr: slot_ptr,
                        ty: elem_ty,
                    },
                );
            }
        }

        let allow_tail = !self.escape_info.has_stack_allocations(&def.name());
        self.emit_gc_safepoint_poll();
        let fun_body = self.gen_expr_in_pos(def.body(), &mut symtbl, allow_tail);
        if !self.builder().current_block_has_terminator() {
            if void_ret {
                self.builder().ret_void();
            } else {
                self.builder().ret(&fun_body);
            }
        }

        if !fun.verify(LLVMVerifierFailureAction::LLVMPrintMessageAction) {
            self.generator.module.dump();
            panic!("LLVM verification failed for function '{}'", def_name);
        }

        self.current_func = None;
    }

    pub fn gen_main(&mut self, def: &FunDef, globals: &[GlobalValDef], prelude: &VarEnv) {
        // 1. Emit `@__geisha_main` containing global initializers and the user's `main` body so its entire stack frame
        //    (all allocas, register spills, and saved callee-saved registers) sits strictly below `@main`'s stack anchor!
        self.current_func = Some(def.name());
        self.pending_let_binding = None;

        let void_ty = self.context().get_void_type();
        let user_main_ty = LLVMContext::get_function_type(&void_ty, &[], false);
        let user_main_fn = self.module().add_function("__geisha_main", &user_main_ty);

        let user_entry = self.context().append_basic_block(&user_main_fn, "entry");
        self.builder().set_position_at_end(&user_entry);

        let mut symtbl = prelude.sub_env();
        let gc_reg_fn = self.generator.declare_gc_register_root();
        for g in globals {
            let (slot, g_ty) = self.global_slots[&g.name].clone();
            if is_gc_pointer_type(&g_ty) {
                self.builder().call(&gc_reg_fn, &mut [slot.ptr], "");
            }
            self.pending_let_binding = None;
            let init_val = self.gen_expr(&g.init, &mut symtbl);
            self.builder().store(&init_val, &slot.ptr);
        }

        self.gen_expr(def.body(), &mut symtbl);

        if !self.builder().current_block_has_terminator() {
            self.builder().ret_void();
        }

        if !user_main_fn.verify(LLVMVerifierFailureAction::LLVMPrintMessageAction) {
            self.generator.module.dump();
            panic!("LLVM verification failed for '__geisha_main'");
        }

        self.current_func = None;

        // 2. Emit C entry trampoline `@main(i32 %argc, ptr %argv)` with zero GC pointers of its own
        let (argc_global, argv_global) = self.get_or_declare_cli_globals();
        let main_ty = self.generator.get_main_type();
        let fun = self.module().add_function("main", &main_ty);
        let block = self.context().append_basic_block(&fun, "entry");
        self.builder().set_position_at_end(&block);

        let argc_param = fun.get_param(0);
        let argv_param = fun.get_param(1);
        self.builder().store(&argc_param, &argc_global);
        self.builder().store(&argv_param, &argv_global);

        let i64_ty = self.context().get_int64_type();
        let stack_anchor = self.builder().alloca(&i64_ty, "gc.stack.anchor");
        let zero64 = self.context().get_int64_const(0);
        self.builder().store(&zero64, &stack_anchor);

        let gc_init_fn = self.generator.declare_gc_init_with_stack();
        let heap_size = self.context().get_int64_const(131072);
        self.builder()
            .call(&gc_init_fn, &mut [heap_size, stack_anchor], "");
        self.builder().call(&user_main_fn, &mut [], "");

        let zero = self.context().get_int32_const(0);
        self.builder().ret(&zero);

        if !fun.verify(LLVMVerifierFailureAction::LLVMPrintMessageAction) {
            self.generator.module.dump();
            panic!("LLVM verification failed for 'main'");
        }
    }

    fn gen_expr<'a: 'b, 'b>(
        &mut self,
        term: &'a TaggedTerm,
        symbols: &mut VarEnv<'b>,
    ) -> LLVMValue {
        self.gen_expr_in_pos(term, symbols, false)
    }

    fn gen_expr_in_pos<'a: 'b, 'b>(
        &mut self,
        term: &'a TaggedTerm,
        symbols: &mut VarEnv<'b>,
        in_tail: bool,
    ) -> LLVMValue {
        use self::Term::*;

        // Take `pending_let_binding` immediately so nested sub-expressions NEVER inherit an outer `let`'s Stack strategy!
        let direct_let_binding = self.pending_let_binding.take();

        match *term.body() {
            Lit(ref lit) => self.generator.gen_lit(lit),

            Var(vn) => {
                let var_name = self.interner.trace(vn).to_owned();
                if let Some(slot) = symbols.lookup(&vn).copied() {
                    self.builder().load(&slot.ty, &slot.ptr, &var_name)
                } else if let Some(slot) = self.global_slots.get(&vn).map(|(s, _)| *s) {
                    self.builder().load(&slot.ty, &slot.ptr, &var_name)
                } else {
                    panic!("Variable '{}' not found in codegen symbol table", var_name);
                }
            }

            GlobalVar(vn) => {
                let var_name = self.interner.trace(vn).to_owned();
                let slot = self
                    .global_slots
                    .get(&vn)
                    .map(|(s, _)| *s)
                    .unwrap_or_else(|| panic!("Global value '{}' not declared", var_name));
                self.builder().load(&slot.ty, &slot.ptr, &var_name)
            }

            Tuple(ref elems) => {
                let tuple_llvm_ty = self.generator.get_val_llvm_type(term.ref_scheme().body());
                let mut agg = tuple_llvm_ty.get_undef();
                for (i, el) in elems.iter().enumerate() {
                    let v = self.gen_expr(el, symbols);
                    agg = self.builder().insert_value(&agg, &v, i, "tup.ins");
                }
                agg
            }

            Unary(op, ref operand) => {
                let val = self.gen_expr(operand, symbols);
                match op {
                    UnOp::Not => {
                        let one = self.context().get_int1_const(1);
                        self.builder().xor(&val, &one, "not")
                    }
                    UnOp::Neg => match operand.ref_scheme().body() {
                        Type::Con(n) if n == "Float" => self.builder().fneg(&val, "fneg"),
                        _ => self.builder().neg(&val, "neg"),
                    },
                }
            }

            // Short-circuit `&&`!
            Binary(BinOp::And, ref lhs, ref rhs) => {
                let lval = self.gen_expr(lhs, symbols);
                let lhs_end_blk = self.builder().get_insert_block();
                let parent = lhs_end_blk.get_parent();

                let rhs_blk = self.context().append_basic_block(&parent, "and.rhs");
                let merge_blk = self.context().append_basic_block(&parent, "and.end");

                self.builder().cond_br(&lval, &rhs_blk, &merge_blk);

                self.builder().set_position_at_end(&rhs_blk);
                let rval = self.gen_expr(rhs, symbols);
                let rhs_end_blk = self.builder().get_insert_block();
                self.builder().br(&merge_blk);

                self.builder().set_position_at_end(&merge_blk);
                let false_val = self.context().get_int1_const(0);
                let i1_ty = self.context().get_int1_type();
                self.builder().phi_node(
                    &i1_ty,
                    &[(&false_val, &lhs_end_blk), (&rval, &rhs_end_blk)],
                    "and.res",
                )
            }

            // Short-circuit `||`!
            Binary(BinOp::Or, ref lhs, ref rhs) => {
                let lval = self.gen_expr(lhs, symbols);
                let lhs_end_blk = self.builder().get_insert_block();
                let parent = lhs_end_blk.get_parent();

                let rhs_blk = self.context().append_basic_block(&parent, "or.rhs");
                let merge_blk = self.context().append_basic_block(&parent, "or.end");

                self.builder().cond_br(&lval, &merge_blk, &rhs_blk);

                self.builder().set_position_at_end(&rhs_blk);
                let rval = self.gen_expr(rhs, symbols);
                let rhs_end_blk = self.builder().get_insert_block();
                self.builder().br(&merge_blk);

                self.builder().set_position_at_end(&merge_blk);
                let true_val = self.context().get_int1_const(1);
                let i1_ty = self.context().get_int1_type();
                self.builder().phi_node(
                    &i1_ty,
                    &[(&true_val, &lhs_end_blk), (&rval, &rhs_end_blk)],
                    "or.res",
                )
            }

            Binary(op, ref lhs, ref rhs) => {
                let lval = self.gen_expr(lhs, symbols);
                let rval = self.gen_expr(rhs, symbols);
                self.generator
                    .bin_operator(op, lval, rval, lhs.ref_scheme().body())
            }

            Let(VarDecl(var, ref tyvar), ref val, ref exp) => {
                let fun = self.builder().get_insert_block().get_parent();
                self.pending_let_binding = Some(var);
                let init = self.gen_expr(val, symbols);
                self.pending_let_binding = None;

                let var_name = self.interner.trace(var).to_owned();
                let val_llvm_ty = self.generator.get_val_llvm_type(tyvar.body());
                let alloca = self
                    .generator
                    .create_entry_block_alloca(&fun, &var_name, &val_llvm_ty);
                self.builder().store(&init, &alloca);

                symbols.with_var(
                    var,
                    VarSlot {
                        ptr: alloca,
                        ty: val_llvm_ty,
                    },
                    |sym| self.gen_expr_in_pos(exp.deref(), sym, in_tail),
                )
            }

            ApplyCls(ref callee, ref args) => {
                let callee_ptr = self.gen_expr(callee, symbols);
                let mut argsv: Vec<LLVMValue> =
                    args.iter().map(|arg| self.gen_expr(arg, symbols)).collect();

                let cls_hdr_ty = self.generator.get_closure_header_type();
                let ptr_ty = self.context().get_ptr_type();

                // Field 1 of `{ ptr fn, env_struct }` starts at byte offset 8, identical to field 1 of `{ ptr, ptr }`!
                let fvs_ptr =
                    self.builder()
                        .struct_field_ptr(&cls_hdr_ty, &callee_ptr, 1, "cls.fv");
                argsv.push(fvs_ptr);

                let fn_entry_ptr =
                    self.builder()
                        .struct_field_ptr(&cls_hdr_ty, &callee_ptr, 0, "cls.fn");
                let fn_entry = self.builder().load(&ptr_ty, &fn_entry_ptr, "cls.fn.actual");

                let callee_fn_ty = self.generator.get_fn_llvm_type(callee.ref_scheme().body());
                let call_val =
                    self.builder()
                        .call_value(&callee_fn_ty, &fn_entry, &mut argsv, "callcls");
                if in_tail {
                    call_val.set_tail_call(true);
                }
                if callee_fn_ty.get_return_type().is_void() {
                    self.context().get_uint8_const(0)
                } else {
                    call_val
                }
            }

            ApplyDir(VarDecl(fun_id, ref fun_ty), ref args) => {
                let callee_name = self.interner.trace(fun_id).to_owned();
                if let Some(intrinsic_val) =
                    self.try_emit_container_intrinsic(&callee_name, args, term.ref_scheme().body(), symbols)
                {
                    return intrinsic_val;
                }

                // Platform C ABI `extern` call (NO trailing `ptr %fvs` closure argument!)
                // Wrapped in `_setjmp` + `gc_enter_blocking` / `gc_leave_blocking` so blocking syscalls never stall STW GC!
                if let Some((c_sym, c_fty)) = self.extern_fns.get(&fun_id).cloned() {
                    let c_callee = self.generator.get_or_add_c_function(&c_sym, &c_fty);
                    let c_llvm_fty = c_callee.get_function_type();
                    let mut argsv: Vec<LLVMValue> =
                        args.iter().map(|arg| self.gen_expr(arg, symbols)).collect();

                    let parent = self.builder().get_insert_block().get_parent();
                    let jb_ty = LLVMContext::get_array_type(&self.context().get_int64_type(), 64);
                    let jb_ptr = self
                        .generator
                        .create_entry_block_alloca(&parent, "ffi.jb", &jb_ty);
                    let setjmp_fn = self.generator.declare_posix_setjmp();
                    let enter_fn = self.generator.declare_gc_enter_blocking();
                    let leave_fn = self.generator.declare_gc_leave_blocking();

                    self.builder().call(&setjmp_fn, &mut [jb_ptr], "");
                    self.builder().call(&enter_fn, &mut [jb_ptr], "");
                    let call_val = self.builder().call(&c_callee, &mut argsv, "callc");
                    self.builder().call(&leave_fn, &mut [], "");

                    return if c_llvm_fty.get_return_type().is_void() {
                        self.context().get_uint8_const(0)
                    } else {
                        call_val
                    };
                }

                let callee = self.generator.get_or_add_function(&callee_name, fun_ty.body());
                let fn_ty = callee.get_function_type();

                let mut argsv: Vec<LLVMValue> =
                    args.iter().map(|arg| self.gen_expr(arg, symbols)).collect();
                let null_fv = self.context().get_ptr_type().get_null();
                argsv.push(null_fv);

                let call_val = self.builder().call(&callee, &mut argsv, "calldir");
                if in_tail && !matches!(callee_name.as_str(), "printLn" | "putNumber" | "putFloat" | "putChar") {
                    call_val.set_tail_call(true);
                }
                if fn_ty.get_return_type().is_void() {
                    self.context().get_uint8_const(0)
                } else {
                    call_val
                }
            }

            Block(ref fs) => {
                let mut ret = self.context().get_uint8_const(0);
                let len = fs.len();
                for (i, n) in fs.iter().enumerate() {
                    ret = self.gen_expr_in_pos(n, symbols, in_tail && (i + 1 == len));
                }
                ret
            }

            MakeCls(ref var_decl, ref cls, ref exp) => {
                let VarDecl(var, ref tyvar) = *var_decl;
                let alloc_strategy = self.get_allocation_strategy(&var);
                let ptr_ty = self.context().get_ptr_type();
                let fun = self.builder().get_insert_block().get_parent();

                let cls_slot_ptr =
                    self.generator
                        .create_entry_block_alloca(&fun, "cls.slot", &ptr_ty);
                let cls_slot = VarSlot {
                    ptr: cls_slot_ptr,
                    ty: ptr_ty,
                };

                let fv_tys: Vec<LLVMType> = symbols.with_var(var, cls_slot, |sym| {
                    cls.fv()
                        .into_iter()
                        .map(|fv_name| {
                            sym.lookup(&fv_name)
                                .map(|s| s.ty)
                                .or_else(|| self.global_slots.get(&fv_name).map(|(s, _)| s.ty))
                                .unwrap_or_else(|| {
                                    panic!(
                                        "Unexpected free var '{}' in closure",
                                        self.interner.trace(fv_name)
                                    )
                                })
                        })
                        .collect()
                });

                let cls_ty_actual = self.generator.get_actual_cls_type(&fv_tys);
                let cls_obj_ptr = match alloc_strategy {
                    AllocationStrategy::Stack => self
                        .generator
                        .create_entry_block_alloca(&fun, "cls.stack", &cls_ty_actual),
                    AllocationStrategy::Heap => {
                        let gc_alloc_fn = self.generator.declare_gc_alloc();
                        let cls_size = self.generator.get_type_size(&cls_ty_actual);
                        let size_val = self.context().get_int64_const(cls_size as i64);
                        self.builder()
                            .call(&gc_alloc_fn, &mut [size_val], "cls.heap")
                    }
                };

                self.builder().store(&cls_obj_ptr, &cls_slot_ptr);

                let cls_fun_field =
                    self.builder()
                        .struct_field_ptr(&cls_ty_actual, &cls_obj_ptr, 0, "cls.fn");
                let ent_name = self.interner.trace_string(cls.entry());
                let fn_ent = self
                    .generator
                    .get_or_add_function(&ent_name, tyvar.body())
                    .into_value();
                self.builder().store(&fn_ent, &cls_fun_field);

                symbols.with_var(var, cls_slot, |sym| {
                    if !fv_tys.is_empty() {
                        let env_struct_ty = self.context().get_struct_type(&fv_tys, false);
                        let cls_fv_ptr = self.builder().struct_field_ptr(
                            &cls_ty_actual,
                            &cls_obj_ptr,
                            1,
                            "cls.fv",
                        );
                        for (i, val_id) in cls.fv().into_iter().enumerate() {
                            let val_slot = sym
                                .lookup(&val_id)
                                .copied()
                                .or_else(|| self.global_slots.get(&val_id).map(|(s, _)| *s))
                                .expect("Free var must exist in scope");
                            let val = self.builder().load(&val_slot.ty, &val_slot.ptr, "fv.load");
                            let fv_field = self.builder().struct_field_ptr(
                                &env_struct_ty,
                                &cls_fv_ptr,
                                i,
                                "fv.slot",
                            );
                            self.builder().store(&val, &fv_field);
                        }
                    }
                    self.gen_expr_in_pos(exp, sym, in_tail && alloc_strategy == AllocationStrategy::Heap)
                })
            }

            If(ref c, ref t, ref f) => {
                let cond = self.gen_expr(c, symbols);
                let parent = self.builder().get_insert_block().get_parent();
                let then_blk = self.context().append_basic_block(&parent, "if.then");
                let else_blk = self.context().append_basic_block(&parent, "if.else");
                let cont_blk = self.context().append_basic_block(&parent, "if.cont");

                self.builder().cond_br(&cond, &then_blk, &else_blk);

                self.builder().set_position_at_end(&then_blk);
                let then_val = self.gen_expr_in_pos(t, symbols, in_tail);
                let then_end = self.builder().get_insert_block();
                self.builder().br(&cont_blk);

                self.builder().set_position_at_end(&else_blk);
                let else_val = self.gen_expr_in_pos(f, symbols, in_tail);
                let else_end = self.builder().get_insert_block();
                self.builder().br(&cont_blk);

                self.builder().set_position_at_end(&cont_blk);
                let res_ty = self.generator.get_val_llvm_type(term.ref_scheme().body());
                self.builder().phi_node(
                    &res_ty,
                    &[(&then_val, &then_end), (&else_val, &else_end)],
                    "if.res",
                )
            }

            While(ref cond, ref body) => {
                let parent = self.builder().get_insert_block().get_parent();
                let cond_blk = self.context().append_basic_block(&parent, "while.cond");
                let body_blk = self.context().append_basic_block(&parent, "while.body");
                let end_blk = self.context().append_basic_block(&parent, "while.end");

                self.builder().br(&cond_blk);

                self.builder().set_position_at_end(&cond_blk);
                self.emit_gc_safepoint_poll();
                let cval = self.gen_expr(cond, symbols);
                self.builder().cond_br(&cval, &body_blk, &end_blk);

                self.builder().set_position_at_end(&body_blk);
                let _ = self.gen_expr(body, symbols);
                if !self.builder().current_block_has_terminator() {
                    self.builder().br(&cond_blk);
                }

                self.builder().set_position_at_end(&end_blk);
                self.context().get_uint8_const(0)
            }

            MakeData {
                ref type_name,
                tag,
                ref fields,
                ref field_types,
                ref type_args,
            } => {
                let field_vals: Vec<LLVMValue> =
                    fields.iter().map(|f| self.gen_expr(f, symbols)).collect();

                let adt_storage_ty = self
                    .generator
                    .get_adt_storage_llvm_type(type_name, type_args);

                let is_recursive = self.escape_info.is_recursive_adt(type_name);
                let alloc_strategy = if is_recursive {
                    AllocationStrategy::Heap
                } else if let Some(bind_id) = direct_let_binding {
                    self.get_allocation_strategy(&bind_id)
                } else {
                    AllocationStrategy::Heap
                };

                let fun = self.builder().get_insert_block().get_parent();
                let adt_ptr = match alloc_strategy {
                    AllocationStrategy::Stack => self
                        .generator
                        .create_entry_block_alloca(&fun, "adt.stack", &adt_storage_ty),
                    AllocationStrategy::Heap => {
                        let gc_alloc_fn = self.generator.declare_gc_alloc();
                        let adt_size = self.generator.get_type_size(&adt_storage_ty);
                        let size_val = self.context().get_int64_const(adt_size as i64);
                        self.builder()
                            .call(&gc_alloc_fn, &mut [size_val], "adt.heap")
                    }
                };

                let tag_val = self.context().get_int32_const(tag as i32);
                let tag_ptr =
                    self.builder()
                        .struct_field_ptr(&adt_storage_ty, &adt_ptr, 0, "adt.tag.ptr");
                self.builder().store(&tag_val, &tag_ptr);

                if !field_vals.is_empty() {
                    let payload_ptr = self.builder().struct_field_ptr(
                        &adt_storage_ty,
                        &adt_ptr,
                        1,
                        "adt.payload.ptr",
                    );
                    let variant_field_llvm_tys: Vec<LLVMType> = field_types
                        .iter()
                        .map(|ft| self.generator.get_val_llvm_type(ft))
                        .collect();
                    let variant_payload_ty =
                        self.context().get_struct_type(&variant_field_llvm_tys, false);

                    for (i, fv) in field_vals.iter().enumerate() {
                        let field_ptr = self.builder().struct_field_ptr(
                            &variant_payload_ty,
                            &payload_ptr,
                            i,
                            "adt.field.ptr",
                        );
                        self.builder().store(fv, &field_ptr);
                    }
                }
                adt_ptr
            }

            GetTag(ref node) => {
                let adt_ptr = self.gen_expr(node, symbols);
                let i32_ty = self.context().get_int32_type();
                let hdr_ty = self.context().get_struct_type(&[i32_ty], false);
                let tag_ptr = self
                    .builder()
                    .struct_field_ptr(&hdr_ty, &adt_ptr, 0, "adt.tag.ptr");
                self.builder().load(&i32_ty, &tag_ptr, "adt.tag")
            }

            GetField(_, _) => {
                unreachable!("GetField is superseded by structured pattern matching")
            }

            Match(ref scrutinee, ref arms) => {
                let scrutinee_val = self.gen_expr(scrutinee, symbols);
                let scrutinee_ty = scrutinee.ref_scheme().body().clone();
                let parent = self.builder().get_insert_block().get_parent();

                let cont_blk = self.context().append_basic_block(&parent, "match.cont");
                let fail_blk = self.context().append_basic_block(&parent, "match.fail");

                let arm_test_blks: Vec<LLVMBasicBlock> = (0..arms.len())
                    .map(|i| {
                        self.context()
                            .append_basic_block(&parent, &format!("match.test.{}", i))
                    })
                    .collect();

                if arm_test_blks.is_empty() {
                    self.builder().br(&fail_blk);
                } else {
                    self.builder().br(&arm_test_blks[0]);
                }

                let mut incoming_results: Vec<(LLVMValue, LLVMBasicBlock)> = Vec::new();

                for (i, (pattern, guard_opt, body)) in arms.iter().enumerate() {
                    let next_fail_blk = if i + 1 < arm_test_blks.len() {
                        &arm_test_blks[i + 1]
                    } else {
                        &fail_blk
                    };

                    self.builder().set_position_at_end(&arm_test_blks[i]);
                    let mut arm_symbols = symbols.sub_env();

                    // Recursively emit pattern checks & bindings (handles arbitrary nesting, tuples, literals, wildcards, and real variant tags!)
                    self.emit_pattern_check(
                        &parent,
                        &scrutinee_val,
                        &scrutinee_ty,
                        pattern,
                        &mut arm_symbols,
                        next_fail_blk,
                    );

                    // Optional match guard `if (cond)`
                    if let Some(ref guard_term) = guard_opt {
                        let guard_val = self.gen_expr(guard_term, &mut arm_symbols);
                        let guard_pass_blk = self
                            .context()
                            .append_basic_block(&parent, &format!("match.body.{}", i));
                        self.builder()
                            .cond_br(&guard_val, &guard_pass_blk, next_fail_blk);
                        self.builder().set_position_at_end(&guard_pass_blk);
                    }

                    let body_val = self.gen_expr_in_pos(body, &mut arm_symbols, in_tail);
                    let body_end_blk = self.builder().get_insert_block();
                    self.builder().br(&cont_blk);
                    incoming_results.push((body_val, body_end_blk));
                }

                // Populate `match.fail` block cleanly with runtime diagnostic abort (no UB!)
                self.builder().set_position_at_end(&fail_blk);
                let fail_fn = self.generator.declare_gs_match_fail();
                self.builder().call(&fail_fn, &mut [], "");
                self.builder().unreachable();

                self.builder().set_position_at_end(&cont_blk);
                let res_llvm_ty = self.generator.get_val_llvm_type(term.ref_scheme().body());
                let refs: Vec<(&LLVMValue, &LLVMBasicBlock)> =
                    incoming_results.iter().map(|(v, b)| (v, b)).collect();
                self.builder().phi_node(&res_llvm_ty, &refs, "match.res")
            }
        }
    }

    /// Emit inline LLVM 20 IR for built-in mutable container primitives (`Cell a` & `MutArray a`).
    fn try_emit_container_intrinsic<'a: 'b, 'b>(
        &mut self,
        name: &str,
        args: &'a [Node],
        ret_ty: &Type,
        symbols: &mut VarEnv<'b>,
    ) -> Option<LLVMValue> {
        match name {
            "cellNew" => {
                let init_val = self.gen_expr(&args[0], symbols);
                let elem_llvm_ty = self.generator.get_val_llvm_type(args[0].ref_scheme().body());
                let byte_size = self.generator.get_type_size(&elem_llvm_ty).max(8);
                let gc_alloc_fn = self.generator.declare_gc_alloc();
                let size_val = self.context().get_int64_const(byte_size as i64);
                let cell_ptr = self
                    .builder()
                    .call(&gc_alloc_fn, &mut [size_val], "cell.ptr");
                self.builder().store(&init_val, &cell_ptr);
                Some(cell_ptr)
            }
            "cellGet" => {
                let cell_ptr = self.gen_expr(&args[0], symbols);
                let elem_llvm_ty = self.generator.get_val_llvm_type(ret_ty);
                Some(self.builder().load(&elem_llvm_ty, &cell_ptr, "cell.val"))
            }
            "cellSet" => {
                let cell_ptr = self.gen_expr(&args[0], symbols);
                let new_val = self.gen_expr(&args[1], symbols);
                self.builder().store(&new_val, &cell_ptr);
                Some(self.context().get_uint8_const(0))
            }
            "arrayNew" => {
                let len32 = self.gen_expr(&args[0], symbols);
                let init_val = self.gen_expr(&args[1], symbols);
                let elem_llvm_ty = self.generator.get_val_llvm_type(args[1].ref_scheme().body());
                let elem_size = self.generator.get_type_size(&elem_llvm_ty).max(1);

                let parent = self.builder().get_insert_block().get_parent();
                let bad_blk = self.context().append_basic_block(&parent, "arr.bad_len");
                let alloc_blk = self.context().append_basic_block(&parent, "arr.alloc");
                let init_cond_blk = self.context().append_basic_block(&parent, "arr.init.cond");
                let init_body_blk = self.context().append_basic_block(&parent, "arr.init.body");
                let init_done_blk = self.context().append_basic_block(&parent, "arr.init.done");

                let zero32 = self.context().get_int32_const(0);
                let is_neg = self.builder().icmp(
                    libllvm::LLVMIntPredicate::LLVMIntSLT,
                    &len32,
                    &zero32,
                    "arr.is_neg",
                );
                self.builder().cond_br(&is_neg, &bad_blk, &alloc_blk);

                self.builder().set_position_at_end(&bad_blk);
                let fail_fn = self.generator.declare_gs_bounds_fail();
                self.builder().call(&fail_fn, &mut [len32, zero32], "");
                self.builder().unreachable();

                self.builder().set_position_at_end(&alloc_blk);
                let i64_ty = self.context().get_int64_type();
                let i8_ty = self.context().get_int8_type();
                let len64 = self.builder().sext(&len32, &i64_ty, "arr.len64");
                let stride_const = self.context().get_int64_const(elem_size as i64);
                let data_bytes = self.builder().mul(&len64, &stride_const, "arr.data_bytes");
                let hdr_bytes = self.context().get_int64_const(8);
                let total_bytes = self.builder().add(&hdr_bytes, &data_bytes, "arr.total_bytes");

                let gc_alloc_fn = if is_noscan_elem_type(args[1].ref_scheme().body()) {
                    self.generator.declare_gc_alloc_atomic()
                } else {
                    self.generator.declare_gc_alloc()
                };
                let arr_ptr = self
                    .builder()
                    .call(&gc_alloc_fn, &mut [total_bytes], "arr.ptr");
                // Store length (`i64`) at byte offset 0; elements buffer starts at 8-byte aligned offset +8
                self.builder().store(&len64, &arr_ptr);

                let mut off8 = [hdr_bytes];
                let elems_base = self
                    .builder()
                    .gep(&i8_ty, &arr_ptr, &mut off8, "arr.elems");

                let idx_slot =
                    self.generator
                        .create_entry_block_alloca(&parent, "arr.init.idx", &i64_ty);
                let zero64 = self.context().get_int64_const(0);
                let one64 = self.context().get_int64_const(1);
                self.builder().store(&zero64, &idx_slot);
                self.builder().br(&init_cond_blk);

                self.builder().set_position_at_end(&init_cond_blk);
                let cur_idx = self.builder().load(&i64_ty, &idx_slot, "arr.idx");
                let more = self.builder().icmp(
                    libllvm::LLVMIntPredicate::LLVMIntSLT,
                    &cur_idx,
                    &len64,
                    "arr.more",
                );
                self.builder()
                    .cond_br(&more, &init_body_blk, &init_done_blk);

                self.builder().set_position_at_end(&init_body_blk);
                let mut idx_arr = [cur_idx];
                let slot_ptr =
                    self.builder()
                        .gep(&elem_llvm_ty, &elems_base, &mut idx_arr, "arr.slot");
                self.builder().store(&init_val, &slot_ptr);
                let next_idx = self.builder().add(&cur_idx, &one64, "arr.next_idx");
                self.builder().store(&next_idx, &idx_slot);
                self.builder().br(&init_cond_blk);

                self.builder().set_position_at_end(&init_done_blk);
                Some(arr_ptr)
            }
            "arrayLen" => {
                let arr_ptr = self.gen_expr(&args[0], symbols);
                let i64_ty = self.context().get_int64_type();
                let i32_ty = self.context().get_int32_type();
                let len64 = self.builder().load(&i64_ty, &arr_ptr, "arr.len64");
                Some(self.builder().trunc(&len64, &i32_ty, "arr.len"))
            }
            "arrayGet" => {
                let arr_ptr = self.gen_expr(&args[0], symbols);
                let idx32 = self.gen_expr(&args[1], symbols);
                let elem_llvm_ty = self.generator.get_val_llvm_type(ret_ty);

                let i64_ty = self.context().get_int64_type();
                let i32_ty = self.context().get_int32_type();
                let i8_ty = self.context().get_int8_type();

                let len64 = self.builder().load(&i64_ty, &arr_ptr, "arr.len64");
                let len32 = self.builder().trunc(&len64, &i32_ty, "arr.len32");
                let oob = self.builder().icmp(
                    libllvm::LLVMIntPredicate::LLVMIntUGE,
                    &idx32,
                    &len32,
                    "arr.oob",
                );

                let parent = self.builder().get_insert_block().get_parent();
                let oob_blk = self.context().append_basic_block(&parent, "arr.oob_trap");
                let ok_blk = self.context().append_basic_block(&parent, "arr.get_ok");
                self.builder().cond_br(&oob, &oob_blk, &ok_blk);

                self.builder().set_position_at_end(&oob_blk);
                let fail_fn = self.generator.declare_gs_bounds_fail();
                self.builder().call(&fail_fn, &mut [idx32, len32], "");
                self.builder().unreachable();

                self.builder().set_position_at_end(&ok_blk);
                let idx64 = self.builder().sext(&idx32, &i64_ty, "arr.idx64");
                let mut off8 = [self.context().get_int64_const(8)];
                let elems_base = self
                    .builder()
                    .gep(&i8_ty, &arr_ptr, &mut off8, "arr.elems");
                let mut idx_arr = [idx64];
                let slot_ptr =
                    self.builder()
                        .gep(&elem_llvm_ty, &elems_base, &mut idx_arr, "arr.slot");
                Some(self.builder().load(&elem_llvm_ty, &slot_ptr, "arr.val"))
            }
            "arraySet" => {
                let arr_ptr = self.gen_expr(&args[0], symbols);
                let idx32 = self.gen_expr(&args[1], symbols);
                let new_val = self.gen_expr(&args[2], symbols);
                let elem_llvm_ty = self.generator.get_val_llvm_type(args[2].ref_scheme().body());

                let i64_ty = self.context().get_int64_type();
                let i32_ty = self.context().get_int32_type();
                let i8_ty = self.context().get_int8_type();

                let len64 = self.builder().load(&i64_ty, &arr_ptr, "arr.len64");
                let len32 = self.builder().trunc(&len64, &i32_ty, "arr.len32");
                let oob = self.builder().icmp(
                    libllvm::LLVMIntPredicate::LLVMIntUGE,
                    &idx32,
                    &len32,
                    "arr.oob",
                );

                let parent = self.builder().get_insert_block().get_parent();
                let oob_blk = self.context().append_basic_block(&parent, "arr.oob_trap");
                let ok_blk = self.context().append_basic_block(&parent, "arr.set_ok");
                self.builder().cond_br(&oob, &oob_blk, &ok_blk);

                self.builder().set_position_at_end(&oob_blk);
                let fail_fn = self.generator.declare_gs_bounds_fail();
                self.builder().call(&fail_fn, &mut [idx32, len32], "");
                self.builder().unreachable();

                self.builder().set_position_at_end(&ok_blk);
                let idx64 = self.builder().sext(&idx32, &i64_ty, "arr.idx64");
                let mut off8 = [self.context().get_int64_const(8)];
                let elems_base = self
                    .builder()
                    .gep(&i8_ty, &arr_ptr, &mut off8, "arr.elems");
                let mut idx_arr = [idx64];
                let slot_ptr =
                    self.builder()
                        .gep(&elem_llvm_ty, &elems_base, &mut idx_arr, "arr.slot");
                self.builder().store(&new_val, &slot_ptr);
                Some(self.context().get_uint8_const(0))
            }
            "bitAnd" => {
                let a = self.gen_expr(&args[0], symbols);
                let b = self.gen_expr(&args[1], symbols);
                Some(self.builder().and(&a, &b, "bit.and"))
            }
            "bitOr" => {
                let a = self.gen_expr(&args[0], symbols);
                let b = self.gen_expr(&args[1], symbols);
                Some(self.builder().or(&a, &b, "bit.or"))
            }
            "bitXor" => {
                let a = self.gen_expr(&args[0], symbols);
                let b = self.gen_expr(&args[1], symbols);
                Some(self.builder().xor(&a, &b, "bit.xor"))
            }
            "bitShl" => {
                let a = self.gen_expr(&args[0], symbols);
                let b = self.gen_expr(&args[1], symbols);
                Some(self.builder().shl(&a, &b, "bit.shl"))
            }
            "bitShr" => {
                let a = self.gen_expr(&args[0], symbols);
                let b = self.gen_expr(&args[1], symbols);
                Some(self.builder().lshr(&a, &b, "bit.shr"))
            }
            "charToInt" => {
                let c = self.gen_expr(&args[0], symbols);
                let i32_ty = self.context().get_int32_type();
                Some(self.builder().zext(&c, &i32_ty, "char.i32"))
            }
            "intToChar" => {
                let n = self.gen_expr(&args[0], symbols);
                let i8_ty = self.context().get_int8_type();
                Some(self.builder().trunc(&n, &i8_ty, "i32.char"))
            }
            "intToI64" => {
                let n = self.gen_expr(&args[0], symbols);
                let i64_ty = self.context().get_int64_type();
                Some(self.builder().sext(&n, &i64_ty, "i32.i64"))
            }
            "i64ToInt" => {
                let n = self.gen_expr(&args[0], symbols);
                let i32_ty = self.context().get_int32_type();
                Some(self.builder().trunc(&n, &i32_ty, "i64.i32"))
            }
            "ptrNull" => Some(self.context().get_ptr_type().get_null()),
            "ptrIsNull" => {
                let p = self.gen_expr(&args[0], symbols);
                let null_p = self.context().get_ptr_type().get_null();
                Some(self.builder().icmp(
                    libllvm::LLVMIntPredicate::LLVMIntEQ,
                    &p,
                    &null_p,
                    "ptr.is_null",
                ))
            }
            "ptrAllocBytes" => {
                let n32 = self.gen_expr(&args[0], symbols);
                let i64_ty = self.context().get_int64_type();
                let n64 = self.builder().sext(&n32, &i64_ty, "alloc.n64");
                let gc_alloc_fn = self.generator.declare_gc_alloc_atomic();
                Some(self.builder().call(&gc_alloc_fn, &mut [n64], "raw.ptr"))
            }
            "ptrByteAt" => {
                let p = self.gen_expr(&args[0], symbols);
                let idx32 = self.gen_expr(&args[1], symbols);
                let i64_ty = self.context().get_int64_type();
                let i8_ty = self.context().get_int8_type();
                let i32_ty = self.context().get_int32_type();
                let idx64 = self.builder().sext(&idx32, &i64_ty, "ptr.idx64");
                let mut idx_arr = [idx64];
                let bp = self.builder().gep(&i8_ty, &p, &mut idx_arr, "ptr.byte_ptr");
                let b8 = self.builder().load(&i8_ty, &bp, "ptr.b8");
                Some(self.builder().zext(&b8, &i32_ty, "ptr.b32"))
            }
            "ptrSetByte" => {
                let p = self.gen_expr(&args[0], symbols);
                let idx32 = self.gen_expr(&args[1], symbols);
                let val32 = self.gen_expr(&args[2], symbols);
                let i64_ty = self.context().get_int64_type();
                let i8_ty = self.context().get_int8_type();
                let idx64 = self.builder().sext(&idx32, &i64_ty, "ptr.idx64");
                let val8 = self.builder().trunc(&val32, &i8_ty, "ptr.val8");
                let mut idx_arr = [idx64];
                let bp = self.builder().gep(&i8_ty, &p, &mut idx_arr, "ptr.byte_ptr");
                self.builder().store(&val8, &bp);
                Some(self.context().get_uint8_const(0))
            }
            "ptrToString" => {
                let p = self.gen_expr(&args[0], symbols);
                let strlen_fn = self.generator.declare_libc_strlen();
                let memcpy_fn = self.generator.declare_libc_memcpy();
                let gc_alloc_fn = self.generator.declare_gc_alloc_atomic();
                let i8_ty = self.context().get_int8_type();
                let one64 = self.context().get_int64_const(1);
                let zero8 = self.context().get_uint8_const(0);

                let len64 = self.builder().call(&strlen_fn, &mut [p], "cstr.len");
                let alloc64 = self.builder().add(&len64, &one64, "cstr.alloc");
                let dst = self.builder().call(&gc_alloc_fn, &mut [alloc64], "cstr.dst");
                self.builder().call(&memcpy_fn, &mut [dst, p, len64], "");
                let mut end_idx = [len64];
                let term_ptr = self.builder().gep(&i8_ty, &dst, &mut end_idx, "cstr.term");
                self.builder().store(&zero8, &term_ptr);
                Some(dst)
            }
            "strLen" => {
                let s = self.gen_expr(&args[0], symbols);
                let strlen_fn = self.generator.declare_libc_strlen();
                let i32_ty = self.context().get_int32_type();
                let len64 = self.builder().call(&strlen_fn, &mut [s], "str.len64");
                Some(self.builder().trunc(&len64, &i32_ty, "str.len"))
            }
            "strByteAt" => {
                let s = self.gen_expr(&args[0], symbols);
                let idx32 = self.gen_expr(&args[1], symbols);
                let strlen_fn = self.generator.declare_libc_strlen();
                let i64_ty = self.context().get_int64_type();
                let i32_ty = self.context().get_int32_type();
                let i8_ty = self.context().get_int8_type();

                let len64 = self.builder().call(&strlen_fn, &mut [s], "str.len64");
                let len32 = self.builder().trunc(&len64, &i32_ty, "str.len32");
                let oob = self.builder().icmp(
                    libllvm::LLVMIntPredicate::LLVMIntUGE,
                    &idx32,
                    &len32,
                    "str.oob",
                );

                let parent = self.builder().get_insert_block().get_parent();
                let oob_blk = self.context().append_basic_block(&parent, "str.oob_trap");
                let ok_blk = self.context().append_basic_block(&parent, "str.ok");
                self.builder().cond_br(&oob, &oob_blk, &ok_blk);

                self.builder().set_position_at_end(&oob_blk);
                let fail_fn = self.generator.declare_gs_bounds_fail();
                self.builder().call(&fail_fn, &mut [idx32, len32], "");
                self.builder().unreachable();

                self.builder().set_position_at_end(&ok_blk);
                let idx64 = self.builder().sext(&idx32, &i64_ty, "str.idx64");
                let mut idx_arr = [idx64];
                let bp = self.builder().gep(&i8_ty, &s, &mut idx_arr, "str.bp");
                let b8 = self.builder().load(&i8_ty, &bp, "str.b8");
                Some(self.builder().zext(&b8, &i32_ty, "str.b32"))
            }
            "strSlice" => {
                let s = self.gen_expr(&args[0], symbols);
                let start32 = self.gen_expr(&args[1], symbols);
                let end32 = self.gen_expr(&args[2], symbols);

                let strlen_fn = self.generator.declare_libc_strlen();
                let memcpy_fn = self.generator.declare_libc_memcpy();
                let gc_alloc_fn = self.generator.declare_gc_alloc_atomic();
                let i64_ty = self.context().get_int64_type();
                let i32_ty = self.context().get_int32_type();
                let i8_ty = self.context().get_int8_type();

                let len64 = self.builder().call(&strlen_fn, &mut [s], "sl.len64");
                let len32 = self.builder().trunc(&len64, &i32_ty, "sl.len32");
                let zero32 = self.context().get_int32_const(0);

                let bad1 = self.builder().icmp(
                    libllvm::LLVMIntPredicate::LLVMIntSLT,
                    &start32,
                    &zero32,
                    "sl.bad1",
                );
                let bad2 = self.builder().icmp(
                    libllvm::LLVMIntPredicate::LLVMIntSLT,
                    &end32,
                    &start32,
                    "sl.bad2",
                );
                let bad3 = self.builder().icmp(
                    libllvm::LLVMIntPredicate::LLVMIntSGT,
                    &end32,
                    &len32,
                    "sl.bad3",
                );
                let bad12 = self.builder().or(&bad1, &bad2, "sl.bad12");
                let bad = self.builder().or(&bad12, &bad3, "sl.bad");

                let parent = self.builder().get_insert_block().get_parent();
                let oob_blk = self.context().append_basic_block(&parent, "sl.oob");
                let ok_blk = self.context().append_basic_block(&parent, "sl.ok");
                self.builder().cond_br(&bad, &oob_blk, &ok_blk);

                self.builder().set_position_at_end(&oob_blk);
                let fail_fn = self.generator.declare_gs_bounds_fail();
                self.builder().call(&fail_fn, &mut [end32, len32], "");
                self.builder().unreachable();

                self.builder().set_position_at_end(&ok_blk);
                let start64 = self.builder().sext(&start32, &i64_ty, "sl.start64");
                let end64 = self.builder().sext(&end32, &i64_ty, "sl.end64");
                let sub_len = self.builder().sub(&end64, &start64, "sl.sublen");
                let one64 = self.context().get_int64_const(1);
                let alloc_sz = self.builder().add(&sub_len, &one64, "sl.alloc");

                let dst = self.builder().call(&gc_alloc_fn, &mut [alloc_sz], "sl.dst");
                let mut src_off = [start64];
                let src_ptr = self.builder().gep(&i8_ty, &s, &mut src_off, "sl.src");
                self.builder()
                    .call(&memcpy_fn, &mut [dst, src_ptr, sub_len], "");
                let mut term_off = [sub_len];
                let term_ptr = self.builder().gep(&i8_ty, &dst, &mut term_off, "sl.term");
                let zero8 = self.context().get_uint8_const(0);
                self.builder().store(&zero8, &term_ptr);
                Some(dst)
            }
            "strConcat" => {
                let a = self.gen_expr(&args[0], symbols);
                let b = self.gen_expr(&args[1], symbols);
                let strlen_fn = self.generator.declare_libc_strlen();
                let memcpy_fn = self.generator.declare_libc_memcpy();
                let gc_alloc_fn = self.generator.declare_gc_alloc_atomic();
                let i8_ty = self.context().get_int8_type();
                let one64 = self.context().get_int64_const(1);
                let zero8 = self.context().get_uint8_const(0);

                let la = self.builder().call(&strlen_fn, &mut [a], "cat.la");
                let lb = self.builder().call(&strlen_fn, &mut [b], "cat.lb");
                let lab = self.builder().add(&la, &lb, "cat.lab");
                let alloc_sz = self.builder().add(&lab, &one64, "cat.alloc");

                let dst = self.builder().call(&gc_alloc_fn, &mut [alloc_sz], "cat.dst");
                self.builder().call(&memcpy_fn, &mut [dst, a, la], "");
                let mut off_a = [la];
                let dst_b = self.builder().gep(&i8_ty, &dst, &mut off_a, "cat.dst_b");
                self.builder().call(&memcpy_fn, &mut [dst_b, b, lb], "");
                let mut off_end = [lab];
                let term_ptr = self.builder().gep(&i8_ty, &dst, &mut off_end, "cat.term");
                self.builder().store(&zero8, &term_ptr);
                Some(dst)
            }
            "strFromBytes" => {
                let arr_ptr = self.gen_expr(&args[0], symbols);
                let count32 = self.gen_expr(&args[1], symbols);

                let i64_ty = self.context().get_int64_type();
                let i32_ty = self.context().get_int32_type();
                let i8_ty = self.context().get_int8_type();

                let cap64 = self.builder().load(&i64_ty, &arr_ptr, "sfb.cap64");
                let cap32 = self.builder().trunc(&cap64, &i32_ty, "sfb.cap32");
                let zero32 = self.context().get_int32_const(0);

                let neg = self.builder().icmp(
                    libllvm::LLVMIntPredicate::LLVMIntSLT,
                    &count32,
                    &zero32,
                    "sfb.neg",
                );
                let gt = self.builder().icmp(
                    libllvm::LLVMIntPredicate::LLVMIntSGT,
                    &count32,
                    &cap32,
                    "sfb.gt",
                );
                let bad = self.builder().or(&neg, &gt, "sfb.bad");

                let parent = self.builder().get_insert_block().get_parent();
                let oob_blk = self.context().append_basic_block(&parent, "sfb.oob");
                let alloc_blk = self.context().append_basic_block(&parent, "sfb.alloc");
                let cond_blk = self.context().append_basic_block(&parent, "sfb.cond");
                let body_blk = self.context().append_basic_block(&parent, "sfb.body");
                let done_blk = self.context().append_basic_block(&parent, "sfb.done");
                self.builder().cond_br(&bad, &oob_blk, &alloc_blk);

                self.builder().set_position_at_end(&oob_blk);
                let fail_fn = self.generator.declare_gs_bounds_fail();
                self.builder().call(&fail_fn, &mut [count32, cap32], "");
                self.builder().unreachable();

                self.builder().set_position_at_end(&alloc_blk);
                let count64 = self.builder().sext(&count32, &i64_ty, "sfb.cnt64");
                let one64 = self.context().get_int64_const(1);
                let alloc_sz = self.builder().add(&count64, &one64, "sfb.alloc_sz");
                let gc_alloc_fn = self.generator.declare_gc_alloc_atomic();
                let dst = self.builder().call(&gc_alloc_fn, &mut [alloc_sz], "sfb.dst");

                let mut off8 = [self.context().get_int64_const(8)];
                let elems_base = self
                    .builder()
                    .gep(&i8_ty, &arr_ptr, &mut off8, "sfb.elems");

                let idx_slot =
                    self.generator
                        .create_entry_block_alloca(&parent, "sfb.idx", &i64_ty);
                self.builder()
                    .store(&self.context().get_int64_const(0), &idx_slot);
                self.builder().br(&cond_blk);

                self.builder().set_position_at_end(&cond_blk);
                let cur = self.builder().load(&i64_ty, &idx_slot, "sfb.cur");
                let more = self.builder().icmp(
                    libllvm::LLVMIntPredicate::LLVMIntSLT,
                    &cur,
                    &count64,
                    "sfb.more",
                );
                self.builder().cond_br(&more, &body_blk, &done_blk);

                self.builder().set_position_at_end(&body_blk);
                let mut i_arr1 = [cur];
                let src_p = self
                    .builder()
                    .gep(&i32_ty, &elems_base, &mut i_arr1, "sfb.src_p");
                let v32 = self.builder().load(&i32_ty, &src_p, "sfb.v32");
                let v8 = self.builder().trunc(&v32, &i8_ty, "sfb.v8");
                let mut i_arr2 = [cur];
                let dst_p = self.builder().gep(&i8_ty, &dst, &mut i_arr2, "sfb.dst_p");
                self.builder().store(&v8, &dst_p);
                let next = self.builder().add(&cur, &one64, "sfb.next");
                self.builder().store(&next, &idx_slot);
                self.builder().br(&cond_blk);

                self.builder().set_position_at_end(&done_blk);
                let mut end_arr = [count64];
                let term_p = self.builder().gep(&i8_ty, &dst, &mut end_arr, "sfb.term");
                self.builder()
                    .store(&self.context().get_uint8_const(0), &term_p);
                Some(dst)
            }
            "sysArgc" => {
                let (argc_g, _) = self.get_or_declare_cli_globals();
                let i32_ty = self.context().get_int32_type();
                Some(self.builder().load(&i32_ty, &argc_g, "cli.argc"))
            }
            "sysArgv" => {
                let idx32 = self.gen_expr(&args[0], symbols);
                let (argc_g, argv_g) = self.get_or_declare_cli_globals();
                let i32_ty = self.context().get_int32_type();
                let i64_ty = self.context().get_int64_type();
                let ptr_ty = self.context().get_ptr_type();

                let argc = self.builder().load(&i32_ty, &argc_g, "cli.argc");
                let oob = self.builder().icmp(
                    libllvm::LLVMIntPredicate::LLVMIntUGE,
                    &idx32,
                    &argc,
                    "cli.oob",
                );

                let parent = self.builder().get_insert_block().get_parent();
                let oob_blk = self.context().append_basic_block(&parent, "cli.oob_trap");
                let ok_blk = self.context().append_basic_block(&parent, "cli.ok");
                self.builder().cond_br(&oob, &oob_blk, &ok_blk);

                self.builder().set_position_at_end(&oob_blk);
                let fail_fn = self.generator.declare_gs_bounds_fail();
                self.builder().call(&fail_fn, &mut [idx32, argc], "");
                self.builder().unreachable();

                self.builder().set_position_at_end(&ok_blk);
                let argv = self.builder().load(&ptr_ty, &argv_g, "cli.argv");
                let idx64 = self.builder().sext(&idx32, &i64_ty, "cli.idx64");
                let mut idx_arr = [idx64];
                let elem_p = self.builder().gep(&ptr_ty, &argv, &mut idx_arr, "cli.argp");
                Some(self.builder().load(&ptr_ty, &elem_p, "cli.arg"))
            }
            _ => self.try_emit_conc_intrinsic(name, args, ret_ty, symbols),
        }
    }

    fn emit_gc_safepoint_poll(&mut self) {
        let req_global = self.generator.declare_gc_stw_requested_global();
        let i32_ty = self.context().get_int32_type();
        let flag = self
            .builder()
            .load_atomic_monotonic_i32(&i32_ty, &req_global, "gc.req");
        let zero = self.context().get_int32_const(0);
        let need_sp =
            self.builder()
                .icmp(LLVMIntPredicate::LLVMIntNE, &flag, &zero, "gc.need_sp");

        let parent = self.builder().get_insert_block().get_parent();
        let slow_blk = self.context().append_basic_block(&parent, "gc.sp.slow");
        let cont_blk = self.context().append_basic_block(&parent, "gc.sp.cont");
        self.builder().cond_br(&need_sp, &slow_blk, &cont_blk);

        self.builder().set_position_at_end(&slow_blk);
        let sp_fn = self.generator.declare_gc_safepoint_slow();
        self.builder().call(&sp_fn, &mut [], "");
        self.builder().br(&cont_blk);

        self.builder().set_position_at_end(&cont_blk);
    }

    fn is_void_geisha_type(ty: &Type) -> bool {
        matches!(ty, Type::Void) || matches!(ty, Type::Con(n) if n == "Void")
    }

    fn mangle_conc_type_sym(ty: &Type) -> String {
        match ty {
            Type::Void => "Void".to_string(),
            Type::Con(n) => n.replace('.', "_"),
            Type::Var(v) => format!("V_{}", v),
            Type::Tuple(es) => {
                let parts: Vec<String> = es.iter().map(Self::mangle_conc_type_sym).collect();
                format!("Tup{}_{}", es.len(), parts.join("_"))
            }
            Type::Arr(ps, r) => {
                let parts: Vec<String> = ps.iter().map(Self::mangle_conc_type_sym).collect();
                format!(
                    "Fn{}_{}_to_{}",
                    ps.len(),
                    parts.join("_"),
                    Self::mangle_conc_type_sym(r)
                )
            }
            Type::Comp(b, a) => format!(
                "{}_of_{}",
                Self::mangle_conc_type_sym(b),
                Self::mangle_conc_type_sym(a)
            ),
        }
    }

    fn box_geisha_value(&mut self, val: LLVMValue, ty: &Type) -> LLVMValue {
        if Self::is_void_geisha_type(ty) {
            return self.context().get_ptr_type().get_null();
        }
        let elem_llvm_ty = self.generator.get_val_llvm_type(ty);
        let byte_size = self.generator.get_type_size(&elem_llvm_ty).max(8);
        let alloc_fn = if is_noscan_elem_type(ty) {
            self.generator.declare_gc_alloc_atomic()
        } else {
            self.generator.declare_gc_alloc()
        };
        let sz_val = self.context().get_int64_const(byte_size as i64);
        let box_ptr = self.builder().call(&alloc_fn, &mut [sz_val], "conc.box");
        self.builder().store(&val, &box_ptr);
        box_ptr
    }

    fn unbox_geisha_value(&mut self, box_ptr: LLVMValue, ty: &Type) -> LLVMValue {
        if Self::is_void_geisha_type(ty) {
            return self.context().get_uint8_const(0);
        }
        let elem_llvm_ty = self.generator.get_val_llvm_type(ty);
        self.builder().load(&elem_llvm_ty, &box_ptr, "conc.unbox")
    }

    fn emit_nullable_box_to_pair(
        &mut self,
        box_ptr: LLVMValue,
        pair_ret_ty: &Type,
    ) -> LLVMValue {
        let elem_ty = match pair_ret_ty {
            Type::Tuple(es) if es.len() == 2 => es[1].clone(),
            _ => panic!("Expected (Bool, T) return type, got {}", pair_ret_ty),
        };
        let pair_llvm_ty = self.generator.get_val_llvm_type(pair_ret_ty);
        let elem_llvm_ty = self.generator.get_val_llvm_type(&elem_ty);
        let null_ptr = self.context().get_ptr_type().get_null();
        let has_val = self.builder().icmp(
            LLVMIntPredicate::LLVMIntNE,
            &box_ptr,
            &null_ptr,
            "conc.has_val",
        );

        let parent = self.builder().get_insert_block().get_parent();
        let some_blk = self.context().append_basic_block(&parent, "conc.some");
        let none_blk = self.context().append_basic_block(&parent, "conc.none");
        let merge_blk = self.context().append_basic_block(&parent, "conc.merge");
        self.builder().cond_br(&has_val, &some_blk, &none_blk);

        self.builder().set_position_at_end(&some_blk);
        let loaded_val = if Self::is_void_geisha_type(&elem_ty) {
            self.context().get_uint8_const(0)
        } else {
            self.builder().load(&elem_llvm_ty, &box_ptr, "conc.some.v")
        };
        let some_end = self.builder().get_insert_block();
        self.builder().br(&merge_blk);

        self.builder().set_position_at_end(&none_blk);
        let zero_val = elem_llvm_ty.get_null();
        let none_end = self.builder().get_insert_block();
        self.builder().br(&merge_blk);

        self.builder().set_position_at_end(&merge_blk);
        let val_phi = self.builder().phi_node(
            &elem_llvm_ty,
            &[(&loaded_val, &some_end), (&zero_val, &none_end)],
            "conc.pair.v",
        );
        let mut agg = pair_llvm_ty.get_undef();
        agg = self.builder().insert_value(&agg, &has_val, 0, "conc.pair.0");
        self.builder().insert_value(&agg, &val_phi, 1, "conc.pair.1")
    }

    /// Synthesize `(ptr %cls_ptr) -> ptr` trampoline invoking a Geisha `() -> T` closure and boxing `T`.
    fn get_or_emit_thunk_runner(&mut self, ret_ty: &Type) -> LLVMFunction {
        let sym = format!("__gs_thunk_runner__{}", Self::mangle_conc_type_sym(ret_ty));
        if let Some(existing) = self.module().get_function(&sym) {
            return existing;
        }

        let saved_blk = self.builder().get_insert_block();
        let ptr_ty = self.context().get_ptr_type();
        let runner_fty = LLVMContext::get_function_type(&ptr_ty, &[ptr_ty], false);
        let runner_fn = self.module().add_function(&sym, &runner_fty);
        let entry = self.context().append_basic_block(&runner_fn, "entry");
        self.builder().set_position_at_end(&entry);

        let cls_ptr = runner_fn.get_param(0);
        let cls_hdr_ty = self.generator.get_closure_header_type();
        let fvs_ptr = self
            .builder()
            .struct_field_ptr(&cls_hdr_ty, &cls_ptr, 1, "tr.fv");
        let fn_slot = self
            .builder()
            .struct_field_ptr(&cls_hdr_ty, &cls_ptr, 0, "tr.fn.slot");
        let fn_entry = self.builder().load(&ptr_ty, &fn_slot, "tr.fn");

        let geisha_fty = Type::Arr(vec![], Box::new(ret_ty.clone()));
        let llvm_fty = self.generator.get_fn_llvm_type(&geisha_fty);
        let call_val = self
            .builder()
            .call_value(&llvm_fty, &fn_entry, &mut [fvs_ptr], "tr.call");

        let boxed = if Self::is_void_geisha_type(ret_ty) {
            // Allocate a tiny 1-word marker box so `gs_mvar`/`gs_chan` distinguish `Some(())` (`non-null`) from `None` (`null`)!
            let alloc_fn = self.generator.declare_gc_alloc_atomic();
            let eight = self.context().get_int64_const(8);
            self.builder().call(&alloc_fn, &mut [eight], "tr.void.box")
        } else {
            self.box_geisha_value(call_val, ret_ty)
        };
        self.builder().ret(&boxed);

        if !runner_fn.verify(LLVMVerifierFailureAction::LLVMPrintMessageAction) {
            self.generator.module.dump();
            panic!("LLVM verification failed for '{}'", sym);
        }

        self.builder().set_position_at_end(&saved_blk);
        runner_fn
    }

    /// Synthesize `(ptr %state_box, ptr %cls_ptr) -> ptr` trampoline invoking an Actor handler `S -> R`.
    fn get_or_emit_actor_runner(&mut self, state_ty: &Type, ret_ty: &Type) -> LLVMFunction {
        let sym = format!(
            "__gs_actor_runner__{}__{}",
            Self::mangle_conc_type_sym(state_ty),
            Self::mangle_conc_type_sym(ret_ty)
        );
        if let Some(existing) = self.module().get_function(&sym) {
            return existing;
        }

        let saved_blk = self.builder().get_insert_block();
        let ptr_ty = self.context().get_ptr_type();
        let runner_fty = LLVMContext::get_function_type(&ptr_ty, &[ptr_ty, ptr_ty], false);
        let runner_fn = self.module().add_function(&sym, &runner_fty);
        let entry = self.context().append_basic_block(&runner_fn, "entry");
        self.builder().set_position_at_end(&entry);

        let state_box = runner_fn.get_param(0);
        let cls_ptr = runner_fn.get_param(1);

        let st_val = self.unbox_geisha_value(state_box, state_ty);
        let cls_hdr_ty = self.generator.get_closure_header_type();
        let fvs_ptr = self
            .builder()
            .struct_field_ptr(&cls_hdr_ty, &cls_ptr, 1, "ar.fv");
        let fn_slot = self
            .builder()
            .struct_field_ptr(&cls_hdr_ty, &cls_ptr, 0, "ar.fn.slot");
        let fn_entry = self.builder().load(&ptr_ty, &fn_slot, "ar.fn");

        let geisha_fty = Type::Arr(vec![state_ty.clone()], Box::new(ret_ty.clone()));
        let llvm_fty = self.generator.get_fn_llvm_type(&geisha_fty);
        let call_val =
            self.builder()
                .call_value(&llvm_fty, &fn_entry, &mut [st_val, fvs_ptr], "ar.call");

        let boxed = if Self::is_void_geisha_type(ret_ty) {
            ptr_ty.get_null()
        } else {
            self.box_geisha_value(call_val, ret_ty)
        };
        self.builder().ret(&boxed);

        if !runner_fn.verify(LLVMVerifierFailureAction::LLVMPrintMessageAction) {
            self.generator.module.dump();
            panic!("LLVM verification failed for '{}'", sym);
        }

        self.builder().set_position_at_end(&saved_blk);
        runner_fn
    }

    /// Emit inline LLVM IR for concurrency runtime intrinsics (`__conc*`).
    fn try_emit_conc_intrinsic<'a: 'b, 'b>(
        &mut self,
        name: &str,
        args: &'a [Node],
        ret_ty: &Type,
        symbols: &mut VarEnv<'b>,
    ) -> Option<LLVMValue> {
        let ptr_ty = self.context().get_ptr_type();
        let i32_ty = self.context().get_int32_type();
        let void_ty = self.context().get_void_type();

        match name {
            "__concAtomicNew" => {
                let init = self.gen_expr(&args[0], symbols);
                let f = self
                    .generator
                    .declare_c_runtime_fn("gs_atomic_new", &ptr_ty, &[i32_ty]);
                Some(self.builder().call(&f, &mut [init], "atom.new"))
            }
            "__concAtomicLoad" => {
                let a = self.gen_expr(&args[0], symbols);
                let f = self
                    .generator
                    .declare_c_runtime_fn("gs_atomic_load", &i32_ty, &[ptr_ty]);
                Some(self.builder().call(&f, &mut [a], "atom.load"))
            }
            "__concAtomicStore" => {
                let a = self.gen_expr(&args[0], symbols);
                let v = self.gen_expr(&args[1], symbols);
                let f = self
                    .generator
                    .declare_c_runtime_fn("gs_atomic_store", &void_ty, &[ptr_ty, i32_ty]);
                self.builder().call(&f, &mut [a, v], "");
                Some(self.context().get_uint8_const(0))
            }
            "__concAtomicAdd" => {
                let a = self.gen_expr(&args[0], symbols);
                let d = self.gen_expr(&args[1], symbols);
                let f = self
                    .generator
                    .declare_c_runtime_fn("gs_atomic_add", &i32_ty, &[ptr_ty, i32_ty]);
                Some(self.builder().call(&f, &mut [a, d], "atom.add"))
            }
            "__concAtomicCas" => {
                let a = self.gen_expr(&args[0], symbols);
                let exp = self.gen_expr(&args[1], symbols);
                let des = self.gen_expr(&args[2], symbols);
                let f = self.generator.declare_c_runtime_fn(
                    "gs_atomic_cas",
                    &i32_ty,
                    &[ptr_ty, i32_ty, i32_ty],
                );
                let r = self.builder().call(&f, &mut [a, exp, des], "atom.cas");
                let zero = self.context().get_int32_const(0);
                Some(
                    self.builder()
                        .icmp(LLVMIntPredicate::LLVMIntNE, &r, &zero, "atom.cas.b"),
                )
            }
            "__concMVarNew" => {
                let val_ty = args[0].ref_scheme().body().clone();
                let val = self.gen_expr(&args[0], symbols);
                let boxed = if Self::is_void_geisha_type(&val_ty) {
                    let alloc_fn = self.generator.declare_gc_alloc_atomic();
                    let eight = self.context().get_int64_const(8);
                    self.builder().call(&alloc_fn, &mut [eight], "mvar.void")
                } else {
                    self.box_geisha_value(val, &val_ty)
                };
                let one = self.context().get_int32_const(1);
                let f = self
                    .generator
                    .declare_c_runtime_fn("gs_mvar_new", &ptr_ty, &[ptr_ty, i32_ty]);
                Some(self.builder().call(&f, &mut [boxed, one], "mvar.new"))
            }
            "__concMVarEmpty" => {
                let null_p = ptr_ty.get_null();
                let zero = self.context().get_int32_const(0);
                let f = self
                    .generator
                    .declare_c_runtime_fn("gs_mvar_new", &ptr_ty, &[ptr_ty, i32_ty]);
                Some(self.builder().call(&f, &mut [null_p, zero], "mvar.empty"))
            }
            "__concMVarTake" => {
                let m = self.gen_expr(&args[0], symbols);
                let f = self
                    .generator
                    .declare_c_runtime_fn("gs_mvar_take", &ptr_ty, &[ptr_ty]);
                let box_ptr = self.builder().call(&f, &mut [m], "mvar.take");
                Some(self.unbox_geisha_value(box_ptr, ret_ty))
            }
            "__concMVarPut" => {
                let m = self.gen_expr(&args[0], symbols);
                let val_ty = args[1].ref_scheme().body().clone();
                let val = self.gen_expr(&args[1], symbols);
                let boxed = if Self::is_void_geisha_type(&val_ty) {
                    let alloc_fn = self.generator.declare_gc_alloc_atomic();
                    let eight = self.context().get_int64_const(8);
                    self.builder().call(&alloc_fn, &mut [eight], "mvar.void")
                } else {
                    self.box_geisha_value(val, &val_ty)
                };
                let f = self
                    .generator
                    .declare_c_runtime_fn("gs_mvar_put", &void_ty, &[ptr_ty, ptr_ty]);
                self.builder().call(&f, &mut [m, boxed], "");
                Some(self.context().get_uint8_const(0))
            }
            "__concMVarRead" => {
                let m = self.gen_expr(&args[0], symbols);
                let f = self
                    .generator
                    .declare_c_runtime_fn("gs_mvar_read", &ptr_ty, &[ptr_ty]);
                let box_ptr = self.builder().call(&f, &mut [m], "mvar.read");
                Some(self.unbox_geisha_value(box_ptr, ret_ty))
            }
            "__concMVarTryTake" => {
                let m = self.gen_expr(&args[0], symbols);
                let f = self
                    .generator
                    .declare_c_runtime_fn("gs_mvar_try_take", &ptr_ty, &[ptr_ty]);
                let box_ptr = self.builder().call(&f, &mut [m], "mvar.trytake");
                Some(self.emit_nullable_box_to_pair(box_ptr, ret_ty))
            }
            "__concChanNew" => {
                let cap = self.gen_expr(&args[0], symbols);
                let f = self
                    .generator
                    .declare_c_runtime_fn("gs_chan_new", &ptr_ty, &[i32_ty]);
                Some(self.builder().call(&f, &mut [cap], "chan.new"))
            }
            "__concChanSend" => {
                let ch = self.gen_expr(&args[0], symbols);
                let val_ty = args[1].ref_scheme().body().clone();
                let val = self.gen_expr(&args[1], symbols);
                let boxed = if Self::is_void_geisha_type(&val_ty) {
                    let alloc_fn = self.generator.declare_gc_alloc_atomic();
                    let eight = self.context().get_int64_const(8);
                    self.builder().call(&alloc_fn, &mut [eight], "chan.void")
                } else {
                    self.box_geisha_value(val, &val_ty)
                };
                let f = self
                    .generator
                    .declare_c_runtime_fn("gs_chan_send", &i32_ty, &[ptr_ty, ptr_ty]);
                let r = self.builder().call(&f, &mut [ch, boxed], "chan.send");
                let zero = self.context().get_int32_const(0);
                Some(
                    self.builder()
                        .icmp(LLVMIntPredicate::LLVMIntNE, &r, &zero, "chan.send.ok"),
                )
            }
            "__concChanRecv" => {
                let ch = self.gen_expr(&args[0], symbols);
                let f = self
                    .generator
                    .declare_c_runtime_fn("gs_chan_recv", &ptr_ty, &[ptr_ty]);
                let box_ptr = self.builder().call(&f, &mut [ch], "chan.recv");
                Some(self.emit_nullable_box_to_pair(box_ptr, ret_ty))
            }
            "__concChanTryRecv" => {
                let ch = self.gen_expr(&args[0], symbols);
                let f = self
                    .generator
                    .declare_c_runtime_fn("gs_chan_try_recv", &ptr_ty, &[ptr_ty]);
                let box_ptr = self.builder().call(&f, &mut [ch], "chan.tryrecv");
                Some(self.emit_nullable_box_to_pair(box_ptr, ret_ty))
            }
            "__concChanClose" => {
                let ch = self.gen_expr(&args[0], symbols);
                let f = self
                    .generator
                    .declare_c_runtime_fn("gs_chan_close", &void_ty, &[ptr_ty]);
                self.builder().call(&f, &mut [ch], "");
                Some(self.context().get_uint8_const(0))
            }
            "__concChanIsClosed" => {
                let ch = self.gen_expr(&args[0], symbols);
                let f = self
                    .generator
                    .declare_c_runtime_fn("gs_chan_is_closed", &i32_ty, &[ptr_ty]);
                let r = self.builder().call(&f, &mut [ch], "chan.closed");
                let zero = self.context().get_int32_const(0);
                Some(
                    self.builder()
                        .icmp(LLVMIntPredicate::LLVMIntNE, &r, &zero, "chan.closed.b"),
                )
            }
            "__concTaskSpawn" => {
                let inner_ty = match ret_ty {
                    Type::Comp(_, elem) => elem.as_ref().clone(),
                    _ => panic!("Expected Task T return type, got {}", ret_ty),
                };
                let cls_ptr = self.gen_expr(&args[0], symbols);
                let runner = self.get_or_emit_thunk_runner(&inner_ty).into_value();
                let f = self
                    .generator
                    .declare_c_runtime_fn("gs_task_spawn", &ptr_ty, &[ptr_ty, ptr_ty]);
                Some(self.builder().call(&f, &mut [runner, cls_ptr], "task.spawn"))
            }
            "__concTaskAwait" => {
                let t = self.gen_expr(&args[0], symbols);
                let f = self
                    .generator
                    .declare_c_runtime_fn("gs_task_await", &ptr_ty, &[ptr_ty]);
                let box_ptr = self.builder().call(&f, &mut [t], "task.await");
                Some(self.unbox_geisha_value(box_ptr, ret_ty))
            }
            "__concTaskIsReady" => {
                let t = self.gen_expr(&args[0], symbols);
                let f = self
                    .generator
                    .declare_c_runtime_fn("gs_task_is_ready", &i32_ty, &[ptr_ty]);
                let r = self.builder().call(&f, &mut [t], "task.ready");
                let zero = self.context().get_int32_const(0);
                Some(
                    self.builder()
                        .icmp(LLVMIntPredicate::LLVMIntNE, &r, &zero, "task.ready.b"),
                )
            }
            "__concTaskWaitAny" => {
                let arr = self.gen_expr(&args[0], symbols);
                let cnt = self.gen_expr(&args[1], symbols);
                let tmo = self.gen_expr(&args[2], symbols);
                let f = self.generator.declare_c_runtime_fn(
                    "gs_task_wait_any",
                    &void_ty,
                    &[ptr_ty, i32_ty, i32_ty],
                );
                self.builder().call(&f, &mut [arr, cnt, tmo], "");
                Some(self.context().get_uint8_const(0))
            }
            "__concNumCpu" => {
                let f = self
                    .generator
                    .declare_c_runtime_fn("gs_conc_num_cpu", &i32_ty, &[]);
                Some(self.builder().call(&f, &mut [], "conc.ncpu"))
            }
            "__concSleepMs" => {
                let ms = self.gen_expr(&args[0], symbols);
                let f = self
                    .generator
                    .declare_c_runtime_fn("gs_conc_sleep_ms", &void_ty, &[i32_ty]);
                self.builder().call(&f, &mut [ms], "");
                Some(self.context().get_uint8_const(0))
            }
            "__concActorSpawn" => {
                let state_ty = match ret_ty {
                    Type::Comp(_, st) => st.as_ref().clone(),
                    _ => panic!("Expected Actor S return type, got {}", ret_ty),
                };
                let init_cls = self.gen_expr(&args[0], symbols);
                let runner = self.get_or_emit_thunk_runner(&state_ty).into_value();
                let f = self
                    .generator
                    .declare_c_runtime_fn("gs_actor_spawn", &ptr_ty, &[ptr_ty, ptr_ty]);
                Some(
                    self.builder()
                        .call(&f, &mut [runner, init_cls], "actor.spawn"),
                )
            }
            "__concActorCall" => {
                let state_ty = match args[0].ref_scheme().body() {
                    Type::Comp(_, st) => st.as_ref().clone(),
                    other => panic!("Expected Actor S argument, got {}", other),
                };
                let resp_ty = match ret_ty {
                    Type::Comp(_, r) => r.as_ref().clone(),
                    _ => panic!("Expected Task R return type, got {}", ret_ty),
                };
                let actor_h = self.gen_expr(&args[0], symbols);
                let handler_cls = self.gen_expr(&args[1], symbols);
                let runner = self
                    .get_or_emit_actor_runner(&state_ty, &resp_ty)
                    .into_value();
                let f = self.generator.declare_c_runtime_fn(
                    "gs_actor_call",
                    &ptr_ty,
                    &[ptr_ty, ptr_ty, ptr_ty],
                );
                Some(
                    self.builder()
                        .call(&f, &mut [actor_h, runner, handler_cls], "actor.call"),
                )
            }
            "__concActorSend" => {
                let state_ty = match args[0].ref_scheme().body() {
                    Type::Comp(_, st) => st.as_ref().clone(),
                    other => panic!("Expected Actor S argument, got {}", other),
                };
                let actor_h = self.gen_expr(&args[0], symbols);
                let handler_cls = self.gen_expr(&args[1], symbols);
                let runner = self
                    .get_or_emit_actor_runner(&state_ty, &Type::Void)
                    .into_value();
                let f = self.generator.declare_c_runtime_fn(
                    "gs_actor_send",
                    &void_ty,
                    &[ptr_ty, ptr_ty, ptr_ty],
                );
                self.builder()
                    .call(&f, &mut [actor_h, runner, handler_cls], "");
                Some(self.context().get_uint8_const(0))
            }
            "__concActorStop" => {
                let actor_h = self.gen_expr(&args[0], symbols);
                let f = self
                    .generator
                    .declare_c_runtime_fn("gs_actor_stop", &void_ty, &[ptr_ty]);
                self.builder().call(&f, &mut [actor_h], "");
                Some(self.context().get_uint8_const(0))
            }
            _ => None,
        }
    }

    /// Recursively emit control-flow checks and variable bindings for `pattern` matching `val` of `val_ty`.
    /// On mismatch at any depth, branches to `fail_blk`; on full match, leaves builder positioned in the success block!
    fn emit_pattern_check<'b>(
        &mut self,
        fun: &LLVMFunction,
        val: &LLVMValue,
        val_ty: &Type,
        pattern: &Pattern,
        symbols: &mut VarEnv<'b>,
        fail_blk: &LLVMBasicBlock,
    ) {
        match pattern {
            Pattern::Wildcard => {}

            Pattern::Var(id) => {
                let vname = self.interner.trace(*id).to_owned();
                let llvm_ty = self.generator.get_val_llvm_type(val_ty);
                let slot_ptr = self
                    .generator
                    .create_entry_block_alloca(fun, &vname, &llvm_ty);
                self.builder().store(val, &slot_ptr);
                symbols.insert(
                    *id,
                    VarSlot {
                        ptr: slot_ptr,
                        ty: llvm_ty,
                    },
                );
            }

            Pattern::Lit(lit) => {
                if matches!(lit, Lit::Unit) {
                    return;
                }
                let lit_val = self.generator.gen_lit(lit);
                let cmp = match lit {
                    Lit::Int(_) | Lit::Bool(_) | Lit::Char(_) => self.builder().icmp(
                        LLVMIntPredicate::LLVMIntEQ,
                        val,
                        &lit_val,
                        "pat.eq",
                    ),
                    Lit::Float(_) => self.builder().fcmp(
                        LLVMRealPredicate::LLVMRealOEQ,
                        val,
                        &lit_val,
                        "pat.feq",
                    ),
                    Lit::Str(_) => {
                        let cmp_fn = self.generator.declare_gs_str_cmp();
                        let diff = self
                            .builder()
                            .call(&cmp_fn, &mut [*val, lit_val], "pat.strcmp");
                        let zero = self.context().get_int32_const(0);
                        self.builder()
                            .icmp(LLVMIntPredicate::LLVMIntEQ, &diff, &zero, "pat.streq")
                    }
                    Lit::Unit => unreachable!(),
                };
                let ok_blk = self.context().append_basic_block(fun, "pat.lit.ok");
                self.builder().cond_br(&cmp, &ok_blk, fail_blk);
                self.builder().set_position_at_end(&ok_blk);
            }

            Pattern::Tuple(sub_pats) => {
                let elem_tys = match val_ty {
                    Type::Tuple(es) => es.clone(),
                    _ => panic!("Tuple pattern matched against non-tuple type: {}", val_ty),
                };
                for (i, sub_pat) in sub_pats.iter().enumerate() {
                    let elem_val = self.builder().extract_value(val, i, "pat.tup.elem");
                    self.emit_pattern_check(
                        fun,
                        &elem_val,
                        &elem_tys[i],
                        sub_pat,
                        symbols,
                        fail_blk,
                    );
                }
            }

            Pattern::Constructor(ctor_name, sub_pats) => {
                let (adt_name, type_args) = self.extract_adt_name_and_args(val_ty, ctor_name);
                let adt_info = self
                    .generator
                    .adt_registry
                    .get(&adt_name)
                    .cloned()
                    .unwrap_or_else(|| panic!("Unknown ADT '{}' in pattern", adt_name));
                let variant = adt_info
                    .variants
                    .iter()
                    .find(|v| &v.name == ctor_name)
                    .unwrap_or_else(|| panic!("Unknown variant '{}'", ctor_name));

                let adt_storage_ty = self
                    .generator
                    .get_adt_storage_llvm_type(&adt_name, &type_args);

                // 1. Load variant tag from `*val` (`ptr`) and compare against `variant.tag` (NOT arm index!)
                let i32_ty = self.context().get_int32_type();
                let tag_ptr =
                    self.builder()
                        .struct_field_ptr(&adt_storage_ty, val, 0, "pat.tag.ptr");
                let actual_tag = self.builder().load(&i32_ty, &tag_ptr, "pat.tag");
                let expected_tag = self.context().get_int32_const(variant.tag as i32);
                let tag_eq = self.builder().icmp(
                    LLVMIntPredicate::LLVMIntEQ,
                    &actual_tag,
                    &expected_tag,
                    "pat.tag.eq",
                );

                let tag_ok_blk = self.context().append_basic_block(fun, "pat.ctor.ok");
                self.builder().cond_br(&tag_eq, &tag_ok_blk, fail_blk);
                self.builder().set_position_at_end(&tag_ok_blk);

                // 2. Unpack fields and recursively match sub-patterns!
                if !sub_pats.is_empty() {
                    let subst: HashMap<String, Type> = adt_info
                        .type_params
                        .iter()
                        .cloned()
                        .zip(type_args.iter().cloned())
                        .collect();
                    let concrete_field_tys: Vec<Type> = variant
                        .field_types
                        .iter()
                        .map(|ft| self.generator.substitute_type(ft, &subst))
                        .collect();
                    let field_llvm_tys: Vec<LLVMType> = concrete_field_tys
                        .iter()
                        .map(|ft| self.generator.get_val_llvm_type(ft))
                        .collect();
                    let variant_payload_ty =
                        self.context().get_struct_type(&field_llvm_tys, false);

                    let payload_ptr = self.builder().struct_field_ptr(
                        &adt_storage_ty,
                        val,
                        1,
                        "pat.payload.ptr",
                    );

                    for (i, sub_pat) in sub_pats.iter().enumerate() {
                        let field_ptr = self.builder().struct_field_ptr(
                            &variant_payload_ty,
                            &payload_ptr,
                            i,
                            "pat.field.ptr",
                        );
                        let field_val =
                            self.builder()
                                .load(&field_llvm_tys[i], &field_ptr, "pat.field");
                        self.emit_pattern_check(
                            fun,
                            &field_val,
                            &concrete_field_tys[i],
                            sub_pat,
                            symbols,
                            fail_blk,
                        );
                    }
                }
            }

            Pattern::StructConstructor(_, _) => {
                unreachable!("StructConstructor patterns are normalized during K-conversion")
            }
        }
    }

    fn extract_adt_name_and_args(&self, val_ty: &Type, ctor_name: &str) -> (String, Vec<Type>) {
        let mut args = Vec::new();
        let mut curr = val_ty;
        while let Type::Comp(base, arg) = curr {
            args.push(arg.as_ref().clone());
            curr = base.as_ref();
        }
        args.reverse();
        if let Type::Con(name) = curr {
            return (name.clone(), args);
        }
        for (adt_name, info) in &self.generator.adt_registry {
            if info.variants.iter().any(|v| v.name == ctor_name) {
                return (adt_name.clone(), vec![]);
            }
        }
        panic!("Could not determine ADT for constructor '{}'", ctor_name);
    }

    fn builder(&self) -> &LLVMBuilder {
        &self.generator.builder
    }

    fn module(&self) -> &LLVMModule {
        &self.generator.module
    }

    fn context(&self) -> &LLVMContext {
        &self.generator.context
    }
}

impl EmitProvider for LLVMCodegen {
    fn gen_module<T>(&mut self, _module: T)
    where
        T: IntoIterator<Item = FunDef>,
    {
    }
}
