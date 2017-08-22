
use core::*;
use types::*;
use internal::*;

use codegen::llvm::*;
pub use codegen::llvm::{ VarEnv };

use std::ops::Deref;


pub trait EmitProvider {
    fn gen_module<T>(&mut self, module: T)
        where T: IntoIterator<Item=Definition>;
}

pub struct LLVMEmit(LLVMCodegen);

impl LLVMEmit {
    pub fn new(name: &str) -> Self {
        LLVMEmit(LLVMCodegen::new(name))
    }
    pub fn dump(&mut self) {
        unsafe {
            LLVMDumpModule(self.0.module);
        }
    }
    pub fn gen_top_level(&mut self, def: &Definition, prelude: &VarEnv) {
        unsafe {
            // A global function definition
            let fun_type = def.ref_type();
            let fun = self.0.get_or_add_function(def.name(), fun_type);
            println!("{:?}", fun_type);

            // Check redefinition
            if LLVMCountBasicBlocks(fun) != 0 {
                panic!("Redefinition of function");
            }

            let bb = LLVMAppendBasicBlockInContext(self.0.context, fun, raw_string("entry"));

            LLVMPositionBuilderAtEnd(self.0.builder, bb);

            // Create a sub environment for current function generating
            let mut symtbl = prelude.sub_env();

            let mut params_ref: Vec<_> = def.parameters().iter().collect();

            let last_param: Option<&VarDecl> = if let Some(&VarDecl(_, ref _last_type)) = def.parameters().last() {
                match _last_type.body() {
                    &Type::Prod(..) => {
                        params_ref.pop()
                    },
                    _ => None
                }
            } else { None };


            // Spread length
            let len_head = params_ref.len();

//            let paramc = LLVMCountParams(fun) as usize;
            // LLVM parameter counts should be the sum
//            if paramc != len_head + last_type_flat.len() + 1 {
//                eprintln!("expected {:?}, found {:?}", paramc, len_head + last_type_flat.len() + 1);
//                panic!("Redefinition of function with different argument count");
//            }
            // For each parameter, set argument name,
            //   create store instruction, add to
            //   symbol table.
            for (i, &VarDecl(ref pname, ref ptype)) in params_ref.into_iter().enumerate() {
                let llarg = LLVMGetParam(fun, i as c_uint);
                // let lltype = self.get_llvm_type(ptype.body());
                LLVMSetValueName(llarg, raw_string(pname.as_str()));

                let alloca = self.0.create_entry_block_alloca(fun, pname.as_str(), ptype.body());

                LLVMBuildStore(self.0.builder, llarg, alloca);
                symtbl.insert(pname.as_str(), alloca);
            }

            if let Some(&VarDecl(ref last_name, ref _last_type)) = last_param {
//                let last_type =
                let last_type_flat = _last_type.body().prod_to_vec();
                // Handle the last parameter
                let last_alloca = self.0.create_entry_block_alloca(fun, last_name.as_str(), _last_type.body());
                println!("fuck: {:?}", last_type_flat.clone());
                for (i, ty) in last_type_flat.into_iter().enumerate() {
                    let idx_name = i.to_string();
                    let llarg = LLVMGetParam(fun, (i + len_head) as c_uint);
                    let argname = last_name.clone() + idx_name.as_str();
                    LLVMSetValueName(llarg, raw_string(argname.as_str()));

                    let field = LLVMBuildStructGEP(self.0.builder, last_alloca, i as c_uint, raw_string(idx_name.as_str()));
                    LLVMBuildStore(self.0.builder, llarg, field);
                }
            }




            // TODO: Make closure pointer a parameter
            // TODO: return pointer instead of stack value for structure type

            let fun_body = self.gen_expr(def.body(), &mut symtbl);
            LLVMBuildRet(self.0.builder, fun_body);

            if LLVMVerifyFunction(fun, LLVMVerifierFailureAction::LLVMPrintMessageAction) != 0 {
                println!("Function verify failed");
            }
        }
    }
    pub unsafe fn gen_expr<'a: 'b, 'b>(&mut self,
                                       term: &'a TaggedTerm,
                                       symbols: &mut VarEnv<'b>)
                                       -> LLVMValueRef {
        use self::Term::*;
        match *term.body() {
            Lit(ref lit) => self.0.gen_lit(lit),
            Var(ref vn) => {
                let var_name = vn.as_str();
                match symbols.lookup(&var_name) {
                    Some(v) => {
                        LLVMBuildLoad(self.0.builder, *v, raw_string(var_name))
                    }
                    // It must be a global definition because of type check
                    None => self.0.get_or_add_function(var_name, term.ref_scheme().body()),
                }
            }
            Binary(op, ref lhs, ref rhs) => {
                let lval = self.gen_expr(lhs, symbols);
                let rval = self.gen_expr(rhs, symbols);
                let instr = get_llvm_op(op, lhs.ref_scheme().body());

                instr(self.0.builder, lval, rval, self.0.new_symbol().unwrap().into_raw())
            }
            Let(ref var_decl, ref val, ref exp) => {
                let &VarDecl(ref var, ref tyvar) = var_decl;
                let var_name = var.as_str();

                let blk = LLVMGetInsertBlock(self.0.builder);
                let fun = LLVMGetBasicBlockParent(blk);

                let init = self.gen_expr(val, symbols);
                let alloca = self.0.create_entry_block_alloca(fun, var_name, tyvar.body());
                LLVMBuildStore(self.0.builder, init, alloca);

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
                // let funty = self.get_fun_type(callee.deref().tag.ref_scheme());
                let callee_ref = self.gen_expr(callee, symbols);

                let mut argsv = vec![];
                for arg in args.iter() {
                    // TODO: unpack tuple
                    argsv.push(self.gen_expr(arg, symbols));
                }

                LLVMBuildCall(self.0.builder,
                              callee_ref,
                              argsv.as_mut_ptr(),
                              argsv.len() as c_uint,
                              self.0.new_symbol().unwrap().into_raw())
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
    fn gen_module<T>(&mut self, module: T)
        where T: IntoIterator<Item=Definition>
    {

    }
}