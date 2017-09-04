use core::*;
use types::*;
use internal::*;
use utils::*;

use codegen::llvm::*;

use std::ops::Deref;


pub trait EmitProvider {
    fn gen_module<T>(&mut self, module: T) where T: IntoIterator<Item = FunDef>;
}

pub struct LLVMEmit(LLVMCodegen);
pub type VarEnv<'a> = SymTable<'a, &'a str, LLVMValue>;

fn with_var<'a, 'b: 'a, F, R>(
    sym: &mut VarEnv<'a>,
    var: &'b str,
    val: LLVMValue,
    mut cb: F) -> R
    where F: FnMut(&mut VarEnv<'a>) -> R
{
    let old = sym.insert(var, val);
    let res = cb(sym);
    if let Some(o) = old {
        sym.insert(var, o);
    } else {
        sym.remove(&var);
    }
    res
}


impl LLVMEmit {
    pub fn new(name: &str) -> Self {
        LLVMEmit(LLVMCodegen::new(name))
    }
    pub fn dump(&mut self) {
        self.0.module.dump()
    }
    pub fn gen_top_level(&mut self, def: &FunDef, prelude: &VarEnv) {
        // A global function definition
        let fun_type = def.ref_type();
        let llvm_fun_type = self.0.get_llvm_type(fun_type);
        let fun = self.0.module.get_or_add_function(def.name(), &llvm_fun_type);

        // Check redefinition

        if fun.count_basic_blocks() != 0 {
            panic!("Redefinition of function");
        }
        let block = self.0.context.append_basic_block(&fun, "entry");

        self.builder().set_position_at_end(&block);

        // Create a sub environment for current function generating
        let mut symtbl = prelude.sub_env();

        let mut params_ref: Vec<_> = def.parameters().iter().collect();

        let last_param: Option<&VarDecl> = if let Some(&VarDecl(_, ref _last_type)) =
            def.parameters().last() {
            match _last_type.body() {
                &Type::Prod(..) => params_ref.pop(),
                _ => None,
            }
        } else {
            None
        };


        // Spread length
        let len_head = params_ref.len();

        // For each parameter, set argument name,
        //   create store instruction, add to
        //   symbol table.
        for (i, param) in params_ref.into_iter().enumerate() {
            let &VarDecl(ref pname, ref ptype) = param;
            let arg = fun.get_param(i);
            arg.set_name(pname.as_str());

            let alloca = self.alloca_for_var(&fun, param);

            self.builder().store(&arg, &alloca);
            symtbl.insert(pname.as_str(), alloca);
        }

        if let Some(param) = last_param {
            let &VarDecl(ref last_name, ref last_type) = param;
            let flatten_count = last_type.body().prod_to_vec().len();

            // Handle the last parameter
            let last_alloca = self.alloca_for_var(&fun, param);

            for i in 0..flatten_count {
                let idx_name = i.to_string();
                let arg = fun.get_param(i + len_head);
                let arg_name = last_name.clone() + idx_name.as_str();
                arg.set_name(arg_name.as_str());
                let field = self.builder().struct_field_ptr(&last_alloca, i, idx_name.as_str());
                self.builder().store(&arg, &field);
            }
        }


        let fun_body = self.gen_expr(def.body(), &mut symtbl);
        self.builder().ret(&fun_body);

        unsafe {
            if LLVMVerifyFunction(fun.raw_ptr(),
                                LLVMVerifierFailureAction::LLVMPrintMessageAction) !=
            0 {
                println!("Function verify failed");
            }
        }
    }
    pub fn gen_expr<'a: 'b, 'b>(&mut self,
                                term: &'a TaggedTerm,
                                symbols: &mut VarEnv<'b>)
                                -> LLVMValue {
        use self::Term::*;
        match *term.body() {
            Lit(ref lit) => self.0.gen_lit(lit),
            Var(ref vn) => {
                let var_name = vn.as_str();
                match symbols.lookup(&var_name) {
                    Some(v) => self.builder().load(v, var_name),
                    _ => unreachable!(), // global functions should be converted to closure

                    // // It must be a global definition because of type check
                    // None => {
                    //     let llvm_type = self.0.get_llvm_type_or_ptr(term.ref_scheme().body());
                    //     self.0
                    //         .module
                    //         .get_or_add_function(var_name, &llvm_type)
                    //         .into_value()
                    // }
                }
            }
            Binary(op, ref lhs, ref rhs) => {
                let lval = self.gen_expr(lhs, symbols);
                let rval = self.gen_expr(rhs, symbols);

                self.0.bin_operator(op, lval, rval, lhs.ref_scheme().body())
            }
            Let(ref var_decl, ref val, ref exp) => {
                let &VarDecl(ref var, ref tyvar) = var_decl;
                let var_name = var.as_str();
                let block = self.builder().insert_block();
                let fun = block.get_parent();

                let init = self.gen_expr(val, symbols);
                let alloca = self.0.create_entry_block_alloca(&fun, var_name, tyvar.body());
                self.builder().store(&init, &alloca);


                with_var(symbols, var_name, alloca, |sym| {
                    self.gen_expr(exp.deref(), sym)
                })
            }
            ApplyCls(ref callee, ref args) => {
                let callee_ty = self.0.get_llvm_type(callee.ref_scheme().body());
                // get the pointer to closure struct
                let callee_ptr = self.gen_expr(callee, symbols); //.into_function();

                let mut argsv: Vec<_> =
                    args.iter().map(|arg| self.gen_expr(arg, symbols)).collect();

                // get fvs from closure struct
                let fvs_ptr = self.builder().struct_field_ptr(&callee_ptr, 1, "cls.fv");
                // add fv into arguments
                argsv.push(fvs_ptr);

                // get actual function entry
                let fn_entry_ptr = self.builder().struct_field_ptr(&callee_ptr, 0, "cls.fn");
                // get a void* pointer
                let fn_entry = self.builder().load(&fn_entry_ptr, "cls.fn.actual");
                // cast to function pointer
                let fun =
                    self.builder().bit_cast(&fn_entry, &callee_ty, "cls.callee").into_function();

                self.builder().call(&fun, &mut argsv, "call")
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
            MakeCls(ref var_decl, box ref cls, box ref exp) => {
                let &VarDecl(ref var, ref tyvar) = var_decl;
                let fun_ty = self.0.get_llvm_type(tyvar.body());
                // make a closure type
                let fv_ty: Vec<LLVMType> = cls.fv()
                    .into_iter()
                    .map(|fv_name| symbols.lookup(&fv_name).unwrap().get_type())
                    .collect();
                let cls_actual_ty = self.0.get_actual_cls_type(&fv_ty);

                // allocate for closure
                let alloca = self.builder().alloca(&cls_actual_ty, "cls.actual");

                // set function entry
                let elm_ptr_fun = self.builder().struct_field_ptr(&alloca, 0, "cls.fn");
                let fn_ent = self.0
                    .module
                    .get_or_add_function(cls.entry(), &fun_ty)
                    .into_value();
                let ptr_ty = self.0
                    .context
                    .get_int8_type()
                    .get_ptr(0);
                let fn_ent_ptr = self.builder().bit_cast(&fn_ent, &ptr_ty, "fn");
                self.builder().store(&fn_ent_ptr, &elm_ptr_fun);

                // store free vars
                let elm_ptr_fv = self.builder().struct_field_ptr(&alloca, 1, "cls.fv");
                for (i, fv_name) in cls.fv().into_iter().enumerate() {
                    // if variable stored a pointer, it will get (type **)
                    // if stored a value, (type *)
                    let val_ptr = symbols.lookup(&fv_name).unwrap();
                    let val = self.builder().load(&val_ptr, "tmp");
                    let fv = self.builder().struct_field_ptr(&elm_ptr_fv, i, fv_name);
                    self.builder().store(&val, &fv);
                }

                let cls_ty = self.0.get_closure_type();
                let cls_var = self.builder().bit_cast(&alloca, &cls_ty, "cls");

                with_var(symbols, var_decl.name(), cls_var, |sym| {
                    self.gen_expr(exp, sym)
                })
            }
            List(_) => unimplemented!(),
            Unary(_, _) => unimplemented!(),
            If(_, _, _) => unimplemented!(),
        }
    }

    fn builder(&self) -> &LLVMBuilder {
        &self.0.builder
    }

    fn alloca_for_var(&self, fun: &LLVMFunction, var: &VarDecl) -> LLVMValue {
        let &VarDecl(ref pname, ref ptype) = var;
        let name = pname.as_str();
        let ty = ptype.body();
        self.0.create_entry_block_alloca(&fun, name, ty)
    }

}

impl EmitProvider for LLVMCodegen {
    fn gen_module<T>(&mut self, module: T) where T: IntoIterator<Item = FunDef> {}
}
