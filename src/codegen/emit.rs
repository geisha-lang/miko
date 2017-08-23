use core::*;
use types::*;
use internal::*;
use utils::*;

use codegen::llvm::*;

use std::ops::Deref;


pub trait EmitProvider {
    fn gen_module<T>(&mut self, module: T) where T: IntoIterator<Item =FunDef>;
}

pub struct LLVMEmit(LLVMCodegen);
pub type VarEnv<'a> = SymTable<'a, &'a str, LLVMValue>;

impl LLVMEmit {
    pub fn new(name: &str) -> Self {
        LLVMEmit(LLVMCodegen::new(name))
    }
    pub fn dump(&mut self) {
        self.0.module.dump()
    }
    pub fn gen_top_level(&mut self, def: &FunDef, prelude: &VarEnv) {
        unsafe {
            // A global function definition
            let fun_type = def.ref_type();
            let llvm_fun_type = self.0.get_llvm_type(fun_type);
            let fun = self.0.module.get_or_add_function(def.name(), &llvm_fun_type);

            // Check redefinition

            if fun.count_basic_blocks() != 0 {
                panic!("Redefinition of function");
            }
            let block = self.0.context.append_basic_block(&fun, "entry");

            self.0.builder.set_position_at_end(&block);

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
            for (i, &VarDecl(ref pname, ref ptype)) in params_ref.into_iter().enumerate() {
                let arg = fun.get_param(i);
                arg.set_name(pname.as_str());

                let alloca = self.0.create_entry_block_alloca(&fun, pname.as_str(), ptype.body());

                self.0.builder.store(&arg, &alloca);
                symtbl.insert(pname.as_str(), alloca);
            }

            if let Some(&VarDecl(ref last_name, ref last_type)) = last_param {
                let flatten_count = last_type.body().prod_to_vec().len();
                // Handle the last parameter
                let last_alloca =
                    self.0.create_entry_block_alloca(&fun, last_name.as_str(), last_type.body());
                // println!("fuck: {:?}", last_type_flat.clone());
                for i in 0..flatten_count {
                    let idx_name = i.to_string();
                    let arg = fun.get_param(i + len_head);
                    let arg_name = last_name.clone() + idx_name.as_str();
                    arg.set_name(arg_name.as_str());
                    let field =
                        self.0.builder.struct_field(&last_alloca, i as u32, idx_name.as_str());
                    self.0.builder.store(&arg, &field);
                }
            }



            // TODO: Make closure pointer a parameter
            // TODO: return pointer instead of stack value for structure type

            let fun_body = self.gen_expr(def.body(), &mut symtbl);
            self.0.builder.ret(&fun_body);

            if LLVMVerifyFunction(fun.raw_ptr(),
                                  LLVMVerifierFailureAction::LLVMPrintMessageAction) !=
               0 {
                println!("Function verify failed");
            }
        }
    }
    pub unsafe fn gen_expr<'a: 'b, 'b>(&mut self,
                                       term: &'a TaggedTerm,
                                       symbols: &mut VarEnv<'b>)
                                       -> LLVMValue {
        use self::Term::*;
        match *term.body() {
            Lit(ref lit) => self.0.gen_lit(lit),
            Var(ref vn) => {
                let var_name = vn.as_str();
                match symbols.lookup(&var_name) {
                    Some(v) => self.0.builder.load(v, var_name),
                    // It must be a global definition because of type check
                    None => {
                        let llvm_type = self.0.get_llvm_type(term.ref_scheme().body());
                        self.0
                            .module
                            .get_or_add_function(var_name, &llvm_type)
                            .into_value()
                    }
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
                let block = self.0.builder.insert_block();
                let fun = block.get_parent();

                let init = self.gen_expr(val, symbols);
                let alloca = self.0.create_entry_block_alloca(&fun, var_name, tyvar.body());
                self.0.builder.store(&init, &alloca);

                let old = symbols.insert(var_name, alloca);

                let res = self.gen_expr(exp.deref(), symbols);

                if let Some(o) = old {
                    symbols.insert(var_name, o);
                } else {
                    symbols.remove(&var_name);
                }
                res
            }
            ApplyCls(ref callee, ref args) => {
                let callee_ref = self.gen_expr(callee, symbols).into_function();

                let mut argsv: Vec<_> =
                    args.iter().map(|arg| self.gen_expr(arg, symbols)).collect();
                let name = self.0.new_symbol_string();
                self.0.builder.call(&callee_ref, &mut argsv, name.as_str())
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
            MakeCls(ref var_decl, ref cls, ref exp) => unimplemented!(),
            List(_) => unimplemented!(),
            Unary(_, _) => unimplemented!(),
            If(_, _, _) => unimplemented!(),
        }
    }
}

impl EmitProvider for LLVMCodegen {
    fn gen_module<T>(&mut self, module: T) where T: IntoIterator<Item =FunDef> {}
}
