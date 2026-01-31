use crate::core::*;
use crate::types::*;
use crate::internal::*;
use crate::utils::*;
use crate::syntax::form::{Pattern, Form};

use crate::codegen::llvm::*;

use std::ops::Deref;


pub trait EmitProvider {
    fn gen_module<T>(&mut self, module: T) where T: IntoIterator<Item = FunDef>;
}

pub struct LLVMEmit<'i> {
    pub generator: LLVMCodegen,
    interner: &'i mut Interner,
    funpass: bool,
}

pub type VarEnv<'a> = SymTable<'a, Id, LLVMValue>;

//fn with_var<'a, 'b: 'a, F, R>(sym: &mut VarEnv<'a>, var: Id, val: LLVMValue, mut cb: F) -> R
//    where F: FnMut(&mut VarEnv<'a>) -> R
//{
//    let old = sym.insert(var, val);
//    let res = cb(sym);
//    if let Some(o) = old {
//        sym.insert(var, o);
//    } else {
//        sym.remove(&var);
//    }
//    res
//}


impl<'i> LLVMEmit<'i> {
    pub fn new(name: &str, interner: &'i mut Interner, adt_registry: AdtRegistry) -> Self {
        LLVMEmit {
            generator: LLVMCodegen::new(name, adt_registry),
            interner,
            funpass: true
        }
    }
    pub fn dump(&mut self) {
        self.generator.module.dump()
    }
    pub fn close_function_pass(&mut self) {
        self.funpass = false;
    }
    pub fn gen_top_level(&mut self, def: &FunDef, prelude: &VarEnv) {
        // A global function definition
        let fun_type = def.ref_type();
        let void_ret = if let Type::Arr(_, ret) = fun_type {
            matches!(ret.as_ref(), Type::Void)
        } else {
            false
        };
        let def_name = self.interner.trace(def.name()).to_owned();
        let fun = self.generator.get_or_add_function(&def_name, fun_type);
        let arg_count = fun.count_params();

        // Check redefinition

        if fun.count_basic_blocks() != 0 {
            panic!("Redefine function");
        }
        let block = self.context().append_basic_block(&fun, "entry");

        self.builder().set_position_at_end(&block);

        // Create a sub environment for current function generating
        let mut symtbl = prelude.sub_env();

        let param_defs = def.parameters();
        let mut param_count = param_defs.len();
        let param_allocas = self.alloca_for_vars(&fun, param_defs);

        if param_allocas.len() + 1 < arg_count {
            let last_formal = param_defs.last();
            if let Some(p) = last_formal {
                let &VarDecl(ref id, ref ty) = p;
                param_count = param_count - 1;
                // Actual parameters counts more than formal, last formal parameter type is production
                let flatten_count = ty.body().prod_to_vec().len();
                let last_alloca = self.alloca_for_var(&fun, p);
                for i in 0..flatten_count {
                    let idx_name = i.to_string();
                    let arg = fun.get_param(i + param_count);
                    let arg_name = self.trace_id(id.to_owned()).to_owned() + idx_name.as_str();
                    arg.set_name(arg_name.as_str());
                    let field = self.builder().struct_field_ptr(&last_alloca, i, idx_name.as_str());
                    self.builder().store(&arg, &field);
                }
                symtbl.insert(id.to_owned(), last_alloca);
            }
        }
        for i in 0..param_count {
            let var = param_allocas[i];
            let arg = fun.get_param(i);
            let pname = param_defs[i].name();
            arg.set_name(self.trace_id(pname));
            self.builder().store(&arg, &var);
            symtbl.insert(pname, var);
        }


        let p_fvs = fun.get_param(arg_count - 1);
        p_fvs.set_name("fvs");

        let formal_fvs = def.fv();
        let fv_allocas = self.alloca_for_vars(&fun, formal_fvs);
        let fv_tys = fv_allocas.iter().map(|v| v.get_type().get_element()).collect();
        let fv_ty_actual = self.context().get_struct_type(&fv_tys, false);
        let fv_ptr_actual = self.builder().bit_cast(&p_fvs, &fv_ty_actual.get_ptr(0), "fv");

        for (i, fv) in fv_allocas.into_iter().enumerate() {
            let fv_id = formal_fvs[i].name();
            let fv_name = self.interner.trace(fv_id);
            let fv_val_ptr = self.builder().struct_field_ptr(&fv_ptr_actual, i, "tmp");
            let fv_ty_actual = fv.get_type();
            let fv_val = self.builder().load(&fv_val_ptr, fv_name);
            self.builder().store(&fv_val, &fv);
            symtbl.insert(fv_id, fv);
        }


        let fun_body = self.gen_expr(def.body(), &mut symtbl);

        self.builder().ret(&fun_body);

        if !fun.verify(LLVMVerifierFailureAction::LLVMPrintMessageAction) {
            panic!();
        } else {
            if self.funpass { self.generator.passer.run(&fun); }
        }
    }

    pub fn gen_main(&mut self, def: &FunDef, prelude: &VarEnv) {
        let main_ty = self.generator.get_main_type();
        let fun = self.module().add_function("main", &main_ty);

        let block = self.context().append_basic_block(&fun, "entry");
        self.builder().set_position_at_end(&block);

        let mut symtbl = prelude.sub_env();
        self.gen_expr(def.body(), &mut symtbl);

        let zero = self.context().get_int32_const(0);
        self.builder().ret(&zero);

        
        fun.verify(LLVMVerifierFailureAction::LLVMPrintMessageAction);
        if self.funpass { self.generator.passer.run(&fun); }
    }
    /// Long bull shit
    fn gen_expr<'a: 'b, 'b>(&mut self,
                            term: &'a TaggedTerm,
                            symbols: &mut VarEnv<'b>)
                            -> LLVMValue {
        use self::Term::*;
        let ret = match *term.body() {
            Lit(ref lit) => self.generator.gen_lit(lit),
            Var(vn) => {
                let var_name = self.interner.trace(vn);
                match symbols.lookup(&vn) {
                    Some(v) => self.builder().load(v, var_name),
                    _ => {
                        eprintln!("cannot find variable {}", self.interner.trace(vn));
                        unreachable!()
                    }
                }
            }
            Binary(op, ref lhs, ref rhs) => {
                let lval = self.gen_expr(lhs, symbols);
                let rval = self.gen_expr(rhs, symbols);

                self.generator.bin_operator(op, lval, rval, lhs.ref_scheme().body())
            }
            Let(ref var_decl, ref val, ref exp) => {
                let &VarDecl(var, ref tyvar) = var_decl;
                let block = self.builder().get_insert_block();
                let fun = block.get_parent();

                let init = self.gen_expr(val, symbols);
                let alloca = {
                    let var_name = self.interner.trace(var);
                    self.generator.create_entry_block_alloca(&fun, var_name, tyvar.body())
                };
                self.builder().store(&init, &alloca);


                symbols.with_var(var, alloca, |sym| self.gen_expr(exp.deref(), sym))
            }
            ApplyCls(ref callee, ref args) => {
                // get the pointer to closure struct
                let callee_ptr = self.gen_expr(callee, symbols); //.into_function();

                let mut argsv: Vec<_> = args
                    .iter()
                    .map(|arg| self.gen_expr(arg, symbols))
                    .collect();

                // get fvs from closure struct
                // add fv into arguments (cast to i8*)
                let fvs_ptr = self.builder().struct_field_ptr(&callee_ptr, 1, "cls.fv");
                let fvs_ty = self.context().get_int8_type().get_ptr(0);
                let fvs = self.builder().bit_cast(&fvs_ptr, &fvs_ty, "cls.fv.cast");
                argsv.push(fvs);

                // get actual function entry
                let fn_entry_ptr = self.builder().struct_field_ptr(&callee_ptr, 0, "cls.fn");
                // get a void* pointer
                let fn_entry = self.builder().load(&fn_entry_ptr, "cls.fn.actual");
                // cast to function pointer
                let callee_fn_ty = self.generator.get_llvm_type(callee.ref_scheme().body());
                let callee_ptr_ty = callee_fn_ty.get_ptr(0);
                let fun =
                    self.builder().bit_cast(&fn_entry, &callee_ptr_ty, "cls.callee").into_function();

                self.builder().call_with_type(&callee_fn_ty, &fun, &mut argsv, "call")
            }
            ApplyDir(VarDecl(fun, ref fun_ty), ref args) => {
                let empty_fv_ty = self.context().get_int8_type().get_ptr(0);
                let empty_fv_ptr = empty_fv_ty.get_null_ptr();
                let callee_name = self.interner.trace(fun).to_owned();
                let callee = self.generator.get_or_add_function(&callee_name, fun_ty.body());

                // Get actual expected parameter types from the LLVM function
                let fn_ty = callee.get_function_type();

                // Generate argument values with type conversion if needed
                let mut argsv: Vec<_> = Vec::new();
                for (i, arg) in args.iter().enumerate() {
                    let val = self.gen_expr(arg, symbols);
                    // Get expected parameter type from LLVM function
                    let expected_llvm_ty = fn_ty.get_param_type(i);
                    let val_ty = val.get_type();

                    // Check if we need to cast - i8* to specific ADT pointer
                    if val_ty.raw_ptr() != expected_llvm_ty.raw_ptr() {
                        argsv.push(self.builder().bit_cast(&val, &expected_llvm_ty, "arg.cast"));
                    } else {
                        argsv.push(val);
                    }
                }
                argsv.push(empty_fv_ptr);

                self.builder().call(&callee, &mut argsv, "calldirect")
            }
            Block(ref fs) => {
                let mut it = fs.iter();
                if let Some(v) = it.next() {
                    let mut ret = self.gen_expr(v, symbols);
                    for n in it {
                        ret = self.gen_expr(n, symbols);
                    }
                    ret
                } else {
                    panic!("Empty block")
                }
            }
            MakeCls(ref var_decl, ref cls, ref exp) => {
                let cls = cls.as_ref();
                let exp = exp.as_ref();
                let &VarDecl(ref var, ref tyvar) = var_decl;

                // make a alloca of closure pointer
                let cls_ty = self.generator.get_closure_type().get_ptr(0);
                let cls_ptr = {
                    self.builder().alloca(&cls_ty, "cls")
                };

                let fv_tys: Vec<_> = symbols.with_var(*var, cls_ptr, |sym| {
                    cls.fv()
                        .into_iter()
                        .map(|fv_name| {
                            match sym.lookup(&fv_name) {
                                Some(v) => v.get_type().get_element(),
                                None => {
                                    eprintln!("unexpected free var: {}", self.interner.trace(fv_name));
                                    panic!()
                                }
                            }
                        })
                        .collect()
                });


                // make a closure type
                let cls_ty_actual = self.generator.get_actual_cls_type(&fv_tys);
                // allocate for closure
                let cls_value = self.builder().alloca(&cls_ty_actual, "cls.actual");
                let cls_cast = self.builder().bit_cast(&cls_value, &cls_ty, "cls.cast");
                self.builder().store(&cls_cast, &cls_ptr);


                // set function entry
                let cls_fun = self.builder().struct_field_ptr(&cls_value, 0, "cls.fn");
                let fn_ent = {
                    let ent_name = self.interner.trace_string(cls.entry());
                    self.generator.get_or_add_function(&ent_name, tyvar.body()).into_value()
                };
                let fn_ent_ptr = {
                    let ptr_ty = self.context().get_int8_type().get_ptr(0);
                    self.builder().bit_cast(&fn_ent, &ptr_ty, "fn")
                };
                self.builder().store(&fn_ent_ptr, &cls_fun);

                symbols.with_var(var_decl.name(), cls_ptr, |sym| {
                    // store free vars
                    let cls_fv = self.builder().struct_field_ptr(&cls_value, 1, "cls.fv");
                    for (i, val_id) in cls.fv().into_iter().enumerate() {
                        // if variable stored a pointer, it will get (type **)
                        // if stored a value, (type *)
                        let val_ptr = sym.lookup(&val_id).expect("cant found free var in evironment");
                        let val = self.builder().load(&val_ptr, "tmp");
                        let fv = self.builder().struct_field_ptr(&cls_fv, i, "tmp");
                        self.builder().store(&val, &fv);
                    }
                    self.gen_expr(exp, sym)
                })

            }
            If(ref c, ref t, ref f) => {
                let c = c.as_ref();
                let t = t.as_ref();
                let f = f.as_ref();
                //                unimplemented!()
                let cond = self.gen_expr(c, symbols);
                let zero = self.context().get_int1_const(0);

                let blk = self.builder().get_insert_block();
                let parent = blk.get_parent();
                let then_blk = self.context().append_basic_block(&parent, "if.then");
                let else_blk = self.context().append_basic_block(&parent, "if.else");
                let cont_blk = self.context().append_basic_block(&parent, "if.cont");

                self.builder().cond_br(&cond, &then_blk, &else_blk);

                self.builder().set_position_at_end(&then_blk);
                let then = self.gen_expr(t, symbols);
                self.builder().br(&cont_blk);
                let then_end = self.builder().get_insert_block();

                self.builder().set_position_at_end(&else_blk);
                let els = self.gen_expr(f, symbols);
                self.builder().br(&cont_blk);
                let els_end = self.builder().get_insert_block();

                self.builder().set_position_at_end(&cont_blk);
                let ret_ty = then.get_type();
                self.builder().phi_node(&ret_ty, &[(&then, &then_end), (&els, &els_end)], "if.res")
            }
            List(_) => unimplemented!(),
            Unary(_, _) => unimplemented!(),

            // ADT Operations
            MakeData(ref type_name, tag, ref fields) => {
                // Create an ADT value with the given tag and fields
                // ADT is represented as: { i32 tag, payload_struct }
                // Returns a pointer to the allocated ADT struct

                // Get the ADT type from the registry
                let adt_ty = self.generator.gen_user_type(type_name);

                // Get field values
                let field_vals: Vec<LLVMValue> = fields.iter()
                    .map(|f| self.gen_expr(f, symbols))
                    .collect();

                // Allocate on stack
                let adt_ptr = self.builder().alloca(&adt_ty, "adt");

                // Store tag
                let tag_val = self.context().get_int32_const(tag as i32);
                let tag_ptr = self.builder().struct_field_ptr(&adt_ptr, 0, "adt.tag.ptr");
                self.builder().store(&tag_val, &tag_ptr);

                // Store payload fields if any
                if !field_vals.is_empty() {
                    let payload_ptr = self.builder().struct_field_ptr(&adt_ptr, 1, "adt.payload.ptr");
                    for (i, fv) in field_vals.iter().enumerate() {
                        let field_ptr = self.builder().struct_field_ptr(&payload_ptr, i, "adt.field.ptr");
                        let field_ty = field_ptr.get_type().get_element();
                        let val_ty = fv.get_type();
                        let val_to_store = self.convert_to_field_type(fv, &val_ty, &field_ty);
                        self.builder().store(&val_to_store, &field_ptr);
                    }
                }
                adt_ptr
            }

            GetTag(ref node) => {
                // Extract the tag from an ADT value (pointer)
                let adt_ptr = self.gen_expr(node, symbols);
                let tag_ptr = self.builder().struct_field_ptr(&adt_ptr, 0, "adt.tag.ptr");
                self.builder().load(&tag_ptr, "adt.tag")
            }

            GetField(ref node, index) => {
                // Extract a field from an ADT value's payload (pointer)
                let adt_ptr = self.gen_expr(node, symbols);
                let payload_ptr = self.builder().struct_field_ptr(&adt_ptr, 1, "adt.payload.ptr");
                let field_ptr = self.builder().struct_field_ptr(&payload_ptr, index, "adt.field.ptr");
                self.builder().load(&field_ptr, "adt.field")
            }

            Match(ref scrutinee, ref arms) => {
                // Generate code for pattern matching using a switch on the tag
                // For ADT patterns, scrutinee_val is a pointer to the ADT struct
                let scrutinee_val = self.gen_expr(scrutinee, symbols);

                // Get current function and create basic blocks
                let blk = self.builder().get_insert_block();
                let parent = blk.get_parent();

                // Create a continuation block and result phi node
                let cont_blk = self.context().append_basic_block(&parent, "match.cont");

                // Check if arms use ADT patterns or simple patterns
                let has_adt_patterns = arms.iter().any(|(p, _, _)| {
                    matches!(p, Pattern::Constructor(_, _))
                });

                if has_adt_patterns {
                    // ADT pattern matching: load tag via pointer and switch on it
                    let tag_ptr = self.builder().struct_field_ptr(&scrutinee_val, 0, "match.tag.ptr");
                    let tag_val = self.builder().load(&tag_ptr, "match.tag");

                    // Create blocks for each arm
                    let arm_blocks: Vec<LLVMBasicBlock> = arms.iter().enumerate()
                        .map(|(i, _)| self.context().append_basic_block(&parent, &format!("match.arm.{}", i)))
                        .collect();

                    // Create default block - use unreachable for exhaustive matches
                    let default_blk = self.context().append_basic_block(&parent, "match.default");
                    self.builder().set_position_at_end(&default_blk);
                    self.builder().unreachable();

                    // Build switch instruction
                    self.builder().set_position_at_end(&blk);
                    let switch_inst = self.builder().switch(&tag_val, &default_blk, arms.len());

                    // Generate code for each arm
                    let mut results: Vec<(LLVMValue, LLVMBasicBlock)> = Vec::new();

                    for (i, ((pattern, _guard, body), arm_blk)) in arms.iter().zip(arm_blocks.iter()).enumerate() {
                        // Add case to switch
                        if let Pattern::Constructor(_, _) = pattern {
                            let tag_const = self.context().get_int32_const(i as i32);
                            LLVMBuilder::switch_add_case(&switch_inst, &tag_const, arm_blk);
                        }

                        // Generate arm body
                        self.builder().set_position_at_end(arm_blk);

                        // Bind pattern variables (scrutinee_val is a pointer)
                        let mut arm_symbols = symbols.sub_env();
                        self.bind_pattern_vars_ptr(&scrutinee_val, pattern, &mut arm_symbols);

                        let result = self.gen_expr(body, &mut arm_symbols);
                        self.builder().br(&cont_blk);
                        let end_blk = self.builder().get_insert_block();
                        results.push((result, end_blk));
                    }

                    // Build phi node for result
                    self.builder().set_position_at_end(&cont_blk);
                    if !results.is_empty() {
                        let result_ty = results[0].0.get_type();
                        let incoming: Vec<_> = results.iter()
                            .map(|(v, b)| (v, b))
                            .collect();
                        self.builder().phi_node(&result_ty, &incoming, "match.result")
                    } else {
                        // No arms - return undef (shouldn't happen in valid code)
                        self.context().get_int32_const(0)
                    }
                } else {
                    // Simple pattern matching (literals, wildcards, variables)
                    // Generate a chain of if-then-else

                    // For simplicity, treat this as a series of comparisons
                    // This handles literal patterns and wildcards
                    self.gen_simple_match(&scrutinee_val, arms, symbols, &parent, &cont_blk)
                }
            }
        };
        ret
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
    fn alloca_for_var(&mut self, fun: &LLVMFunction, var: &VarDecl) -> LLVMValue {
        let &VarDecl(pname, ref ptype) = var;
        let name = self.interner.trace(pname);
        let ty = ptype.body();
        self.generator.create_entry_block_alloca(&fun, name, ty)
    }

    fn alloca_for_vars(&mut self, fun: &LLVMFunction, vars: &Vec<VarDecl>) -> Vec<LLVMValue> {
        vars.iter().map(|v| self.alloca_for_var(fun, v)).collect()
    }

    fn trace_id(&mut self, id: Id) -> &str {
        self.interner.trace(id)
    }

    fn get_value_ptr(&mut self, val: &LLVMValue) -> LLVMValue {
        let ty = val.get_type();
        let alloca = self.builder().alloca(&ty, "ptr");
        self.builder().store(val, &alloca);
        alloca
    }

    /// Convert a value to match the expected field type in an ADT
    /// Handles polymorphic fields (i64) and pointer type mismatches
    fn convert_to_field_type(&mut self, val: &LLVMValue, val_ty: &LLVMType, field_ty: &LLVMType) -> LLVMValue {
        if val_ty.raw_ptr() == field_ty.raw_ptr() {
            // Types match, no conversion needed
            return val.clone();
        }

        let i64_ty = self.context().get_int64_type();
        let i32_ty = self.context().get_int32_type();
        let i8_ptr_ty = self.context().get_int8_type().get_ptr(0);

        // Check if field is i64 (polymorphic slot)
        if field_ty.raw_ptr() == i64_ty.raw_ptr() {
            // Convert value to i64
            if val_ty.raw_ptr() == i32_ty.raw_ptr() {
                // i32 -> i64 via sext
                return self.builder().sext(val, &i64_ty, "sext.i64");
            } else {
                // Pointer -> i64 via ptrtoint
                return self.builder().ptr_to_int(val, &i64_ty, "ptr.to.i64");
            }
        }

        // Check if field is i8* (opaque pointer for ADT)
        if field_ty.raw_ptr() == i8_ptr_ty.raw_ptr() {
            // Bitcast pointer to i8*
            return self.builder().bit_cast(val, &i8_ptr_ty, "ptr.cast");
        }

        // Fallback: try bitcast
        self.builder().bit_cast(val, field_ty, "field.cast")
    }

    /// Bind pattern variables to their values in the current scope
    fn bind_pattern_vars<'b>(&mut self, scrutinee: &LLVMValue, pattern: &Pattern, symbols: &mut VarEnv<'b>) {
        match pattern {
            Pattern::Var(id) => {
                // Bind the entire scrutinee to this variable
                let var_name = self.interner.trace(*id);
                let alloca = self.builder().alloca(&scrutinee.get_type(), var_name);
                self.builder().store(scrutinee, &alloca);
                symbols.insert(*id, alloca);
            }
            Pattern::Wildcard | Pattern::Lit(_) => {
                // No bindings for wildcards or literals
            }
            Pattern::Constructor(_, sub_patterns) => {
                // Extract payload and bind sub-patterns
                if !sub_patterns.is_empty() {
                    let payload = self.builder().extract_value(scrutinee, 1, "pat.payload");
                    for (i, sub_pat) in sub_patterns.iter().enumerate() {
                        let field = self.builder().extract_value(&payload, i, "pat.field");
                        self.bind_pattern_vars(&field, sub_pat, symbols);
                    }
                }
            }
        }
    }

    /// Bind pattern variables when scrutinee is a pointer to ADT struct
    fn bind_pattern_vars_ptr<'b>(&mut self, scrutinee_ptr: &LLVMValue, pattern: &Pattern, symbols: &mut VarEnv<'b>) {
        match pattern {
            Pattern::Var(id) => {
                // Bind the pointer directly (variable holds pointer to ADT)
                let var_name = self.interner.trace(*id);
                let alloca = self.builder().alloca(&scrutinee_ptr.get_type(), var_name);
                self.builder().store(scrutinee_ptr, &alloca);
                symbols.insert(*id, alloca);
            }
            Pattern::Wildcard | Pattern::Lit(_) => {
                // No bindings for wildcards or literals
            }
            Pattern::Constructor(_, sub_patterns) => {
                // Extract payload fields via pointer and bind sub-patterns
                if !sub_patterns.is_empty() {
                    let payload_ptr = self.builder().struct_field_ptr(scrutinee_ptr, 1, "pat.payload.ptr");
                    for (i, sub_pat) in sub_patterns.iter().enumerate() {
                        let field_ptr = self.builder().struct_field_ptr(&payload_ptr, i, "pat.field.ptr");
                        let field_val = self.builder().load(&field_ptr, "pat.field");
                        self.bind_pattern_vars(&field_val, sub_pat, symbols);
                    }
                }
            }
        }
    }

    /// Generate code for simple pattern matching (literals, wildcards, variables)
    fn gen_simple_match<'a: 'b, 'b>(
        &mut self,
        scrutinee: &LLVMValue,
        arms: &'a [(Pattern, Option<P<Form>>, Box<TaggedTerm>)],
        symbols: &mut VarEnv<'b>,
        parent: &LLVMFunction,
        cont_blk: &LLVMBasicBlock,
    ) -> LLVMValue {
        if arms.is_empty() {
            // No arms - return a default value
            return self.context().get_int32_const(0);
        }

        let mut results: Vec<(LLVMValue, LLVMBasicBlock)> = Vec::new();

        // Generate a chain of conditional branches
        for (i, (pattern, _guard, body)) in arms.iter().enumerate() {
            let is_last = i == arms.len() - 1;

            match pattern {
                Pattern::Wildcard | Pattern::Var(_) => {
                    // Always matches - generate body directly
                    let mut arm_symbols = symbols.sub_env();
                    self.bind_pattern_vars(scrutinee, pattern, &mut arm_symbols);

                    let result = self.gen_expr(body, &mut arm_symbols);
                    self.builder().br(cont_blk);
                    let end_blk = self.builder().get_insert_block();
                    results.push((result, end_blk));
                    break; // No need to check further arms
                }
                Pattern::Lit(lit) => {
                    // Generate comparison
                    let lit_val = self.generator.gen_lit(lit);
                    let cmp = match lit {
                        Lit::Int(_) => {
                            unsafe {
                                LLVMValue::from_ref(llvm_sys::core::LLVMBuildICmp(
                                    self.builder().raw_ptr(),
                                    LLVMIntPredicate::LLVMIntEQ,
                                    scrutinee.raw_ptr(),
                                    lit_val.raw_ptr(),
                                    crate::codegen::llvm::raw_string("cmp")
                                ))
                            }
                        }
                        Lit::Bool(_) => {
                            unsafe {
                                LLVMValue::from_ref(llvm_sys::core::LLVMBuildICmp(
                                    self.builder().raw_ptr(),
                                    LLVMIntPredicate::LLVMIntEQ,
                                    scrutinee.raw_ptr(),
                                    lit_val.raw_ptr(),
                                    crate::codegen::llvm::raw_string("cmp")
                                ))
                            }
                        }
                        _ => {
                            // For other types, fall through (simplified)
                            self.context().get_int1_const(1)
                        }
                    };

                    let then_blk = self.context().append_basic_block(parent, &format!("match.then.{}", i));
                    let else_blk = if is_last {
                        cont_blk.clone()
                    } else {
                        self.context().append_basic_block(parent, &format!("match.else.{}", i))
                    };

                    self.builder().cond_br(&cmp, &then_blk, &else_blk);

                    // Generate then block
                    self.builder().set_position_at_end(&then_blk);
                    let result = self.gen_expr(body, symbols);
                    self.builder().br(cont_blk);
                    let end_blk = self.builder().get_insert_block();
                    results.push((result, end_blk));

                    // Continue from else block if not last
                    if !is_last {
                        self.builder().set_position_at_end(&else_blk);
                    }
                }
                Pattern::Constructor(_, _) => {
                    // Constructor patterns are handled in the ADT case
                    // This shouldn't happen in simple match
                    unreachable!("Constructor pattern in simple match")
                }
            }
        }

        // Build phi node for result
        self.builder().set_position_at_end(cont_blk);
        if !results.is_empty() {
            let result_ty = results[0].0.get_type();
            let incoming: Vec<_> = results.iter()
                .map(|(v, b)| (v, b))
                .collect();
            self.builder().phi_node(&result_ty, &incoming, "match.result")
        } else {
            self.context().get_int32_const(0)
        }
    }
}

impl EmitProvider for LLVMCodegen {
    fn gen_module<T>(&mut self, module: T) where T: IntoIterator<Item = FunDef> {}
}
