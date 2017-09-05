use core::*;
use types::*;
use internal::*;
use utils::*;

use codegen::llvm::*;

use std::ops::Deref;


pub trait EmitProvider {
    fn gen_module<T>(&mut self, module: T) where T: IntoIterator<Item = FunDef>;
}

pub struct LLVMEmit<'i> {
    generator: LLVMCodegen,
    interner: &'i mut Interner,
}

pub type VarEnv<'a> = SymTable<'a, Id, LLVMValue>;

fn with_var<'a, 'b: 'a, F, R>(sym: &mut VarEnv<'a>, var: Id, val: LLVMValue, mut cb: F) -> R
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


impl<'i> LLVMEmit<'i> {
    pub fn new(name: &str, interner: &'i mut Interner) -> Self {
        LLVMEmit {
            generator: LLVMCodegen::new(name),
            interner,
        }
    }
    pub fn dump(&mut self) {
        self.generator.module.dump()
    }
    pub fn gen_top_level(&mut self, def: &FunDef, prelude: &VarEnv) {
        // A global function definition
        let fun_type = def.ref_type();
        let void_ret: bool = if let &Type::Arr(_, box Type::Void) = fun_type {
            true
        } else {
            false
        };
        let llvm_fun_type = self.generator.get_llvm_type(fun_type);
        let def_name = def.name();
        let fun = self.generator.module.get_or_add_function(def_name, &llvm_fun_type);
        let arg_count = fun.count_params();

        // Check redefinition

        if fun.count_basic_blocks() != 0 {
            panic!("Redefinition of function");
        }
        let block = self.generator.context.append_basic_block(&fun, "entry");

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
        let fv_ty_actual = self.generator.context.get_struct_type(&fv_tys, false);
        let fv_ptr_actual = self.builder().bit_cast(&p_fvs, &fv_ty_actual.get_ptr(0), "fv");
        for (i, fv) in fv_allocas.into_iter().enumerate() {
            let fv_id = formal_fvs[i].name();
            let fv_name = self.interner.trace(fv_id);
            let fv_val_ptr =
                self.builder().struct_field_ptr(&fv_ptr_actual, i, "tmp");
            let fv_val = self.builder().load(&fv_val_ptr, fv_name);
            self.builder().store(&fv_val, &fv);
            symtbl.insert(fv_id, fv);
        }


        let fun_body = self.gen_expr(def.body(), &mut symtbl);

        self.builder().ret(&fun_body);

        fun.verify(LLVMVerifierFailureAction::LLVMPrintMessageAction);
        self.generator.passer.run(&fun);
    }
    pub fn gen_expr<'a: 'b, 'b>(&mut self,
                                term: &'a TaggedTerm,
                                symbols: &mut VarEnv<'b>)
                                -> LLVMValue {
        use self::Term::*;
        match *term.body() {
            Lit(ref lit) => self.generator.gen_lit(lit),
            Var(vn) => {
                let var_name = self.interner.trace(vn);
                match symbols.lookup(&vn) {
                    Some(v) => self.builder().load(v, var_name),
                    _ => {
                        println!("cannot find variable {}", self.interner.trace(vn));
                        unreachable!()
                    } // global functions should be converted to closure
                }
            }
            Binary(op, ref lhs, ref rhs) => {
                let lval = self.gen_expr(lhs, symbols);
                let rval = self.gen_expr(rhs, symbols);

                self.generator.bin_operator(op, lval, rval, lhs.ref_scheme().body())
            }
            Let(ref var_decl, ref val, ref exp) => {
                let &VarDecl(var, ref tyvar) = var_decl;
                let block = self.builder().insert_block();
                let fun = block.get_parent();

                let init = self.gen_expr(val, symbols);
                let alloca = {
                    let var_name = self.interner.trace(var);
                    self.generator.create_entry_block_alloca(&fun, var_name, tyvar.body())
                };
                self.builder().store(&init, &alloca);


                with_var(symbols, var, alloca, |sym| self.gen_expr(exp.deref(), sym))
            }
            ApplyCls(ref callee, ref args) => {
                // get the pointer to closure struct
                let callee_ptr = self.gen_expr(callee, symbols); //.into_function();

                let mut argsv: Vec<_> =
                    args.iter().map(|arg| self.gen_expr(arg, symbols)).collect();

                // get fvs from closure struct
                // add fv into arguments
                let fvs = {
                    let fvs_ptr = self.builder().struct_field_ptr(&callee_ptr, 1, "cls.fvptr");
                    self.builder().load(&fvs_ptr, "cls.fv")
                };
                argsv.push(fvs);

                // get actual function entry
                let fn_entry_ptr = self.builder().struct_field_ptr(&callee_ptr, 0, "cls.fn");
                // get a void* pointer
                let fn_entry = self.builder().load(&fn_entry_ptr, "cls.fn.actual");
                // cast to function pointer
                let callee_ty = self.generator.get_llvm_type(callee.ref_scheme().body()).get_ptr(0);
                // self.builder().bit_cast(&fn_entry, &callee_ty, "cls.callee")
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
                let fun_ty = self.generator.get_llvm_type(tyvar.body());
                // make a closure type
                let fv_ty: Vec<LLVMType> = cls.fv()
                    .into_iter()
                    .map(|fv_name| {
                             symbols.lookup(&fv_name)
                                 .unwrap()
                                 .get_type()
                                 .get_element()
                         })
                    .collect();
                let cls_actual_ty = self.generator.get_actual_cls_type(&fv_ty);

                // allocate for closure
                let cls_ptr = self.builder().alloca(&cls_actual_ty, "cls.actual");

                // set function entry
                let elm_ptr_fun = self.builder().struct_field_ptr(&cls_ptr, 0, "cls.fn");
                let fn_ent = self.generator
                    .module
                    .get_or_add_function(cls.entry(), &fun_ty)
                    .into_value();
                let ptr_ty = self.generator
                    .context
                    .get_int8_type()
                    .get_ptr(0);
                let fn_ent_ptr = self.builder().bit_cast(&fn_ent, &ptr_ty, "fn");
                self.builder().store(&fn_ent_ptr, &elm_ptr_fun);

                // store free vars
                let elm_ptr_fv = self.builder().struct_field_ptr(&cls_ptr, 1, "cls.fv");
                for (i, fv_id) in cls.fv().into_iter().enumerate() {
                    // if variable stored a pointer, it will get (type **)
                    // if stored a value, (type *)
                    let val_ptr = symbols.lookup(&fv_id).unwrap();
                    let val = self.builder().load(&val_ptr, "tmp");
                    let fv = {
                        let fv_name = self.interner.trace(fv_id);
                        self.builder().struct_field_ptr(&elm_ptr_fv, i, fv_name)
                    };
                    self.builder().store(&val, &fv);
                }

                let cls_var = {
                    let cls_ty = self.generator.get_closure_type().get_ptr(0);
                    let val = self.builder().bit_cast(&cls_ptr, &cls_ty, "cls");
                    self.get_value_ptr(&val)
                };

                with_var(symbols,
                         var_decl.name(),
                         cls_var,
                         |sym| self.gen_expr(exp, sym))
            }
            List(_) => unimplemented!(),
            Unary(_, _) => unimplemented!(),
            If(_, _, _) => unimplemented!(),
        }
    }

    fn builder(&self) -> &LLVMBuilder {
        &self.generator.builder
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
}

impl EmitProvider for LLVMCodegen {
    fn gen_module<T>(&mut self, module: T) where T: IntoIterator<Item = FunDef> {}
}
