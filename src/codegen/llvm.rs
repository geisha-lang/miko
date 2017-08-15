use std::char;
use std::ffi::{CString, NulError};
use std::io::{self, Read, Write, BufReader};
use std::ptr;
use std::collections::HashMap;
use std::iter::*;

use std::ops::Deref;
use libc::{c_uint, c_ulonglong, c_char};


use llvm_sys::prelude::{LLVMBuilderRef, LLVMContextRef, LLVMModuleRef, LLVMPassManagerRef, LLVMTypeRef, LLVMValueRef};
use llvm_sys::execution_engine::{LLVMExecutionEngineRef, LLVMGenericValueToFloat, LLVMRunFunction, LLVMGenericValueRef};
use llvm_sys::analysis::{LLVMVerifyFunction, LLVMVerifierFailureAction};
use llvm_sys::LLVMRealPredicate;
use llvm_sys::core::*;

// use codegen::llvm::*;
use syntax::interm::*;
use core::term::*;
use types::*;
use utils::*;

#[derive(Debug, Clone)]
pub struct LLVMCodegen {
    module: LLVMModuleRef,
    builder: LLVMBuilderRef,
    context: LLVMContextRef,
    unique: usize,
}

pub type VarEnv<'a> = SymTable<'a, &'a str, LLVMValueRef>;

fn get_llvm_op(op: BinOp, p_type: &Type)
    -> unsafe extern "C"
        fn(LLVMBuilderRef, LLVMValueRef, LLVMValueRef, *const ::libc::c_char) -> LLVMValueRef
{
    use self::BinOp::*;
    use self::Type::*;
    if let &Con(ref ty_name) = p_type {
        let name_ref = ty_name.as_str();
        match op {
            Add => match name_ref {
                "Int" => LLVMBuildAdd,
                "Float" => LLVMBuildFAdd,
                _ => unreachable!()
            },
            Sub => match name_ref {
                "Int" => LLVMBuildSub,
                "Float" => LLVMBuildFSub,
                _ => unreachable!()
            },
            Mul => match name_ref {
                "Int" => LLVMBuildMul,
                "Float" => LLVMBuildFMul,
                _ => unreachable!()
            },
            Div => LLVMBuildFDiv,
            Rem => LLVMBuildURem, // TODO: WTF LLVM builder ???
            _ => unreachable!()
        }
    } else {
        unreachable!()
    }
}

unsafe fn raw_string(s: &str) -> *mut c_char {
    CString::new(s).unwrap().into_raw()
}

impl LLVMCodegen {
    pub fn new(name: &str) -> LLVMCodegen {
        unsafe {
            let ctx = LLVMContextCreate();
            let modu = LLVMModuleCreateWithNameInContext(CString::new(name).unwrap().into_raw(), ctx);
            let builder = LLVMCreateBuilderInContext(ctx);
            LLVMCodegen {
                module: modu,
                context: ctx,
                builder: builder,
                unique: 0
            }
        }
    }
    pub fn gen_top_level(&mut self, def: &Definition, prelude: &VarEnv) {
        unsafe {
            // A global function definition
            let fun_type = def.ref_type();
            let fun = self.get_or_add_function(def.name(), fun_type);

            // Check redefinition
            if LLVMCountBasicBlocks(fun) != 0 {
                panic!("Redefinition of function"); // TODO: Error return
            }
            let paramc = LLVMCountParams(fun) as usize;
            if paramc != def.parameters().len() {
                // TODO: Error return
                // self.dump();
                panic!("Redefinition of function with different argument count");
            }

            let mut symtbl = prelude.sub_env();


            let bb = LLVMAppendBasicBlockInContext(
                self.context,
                fun,
                CString::new("entry").unwrap().into_raw());

            LLVMPositionBuilderAtEnd(self.builder, bb);

            // For each parameter, set argument name,
            //   create store instruction, add to
            //   symbol table.
            for (i, &VarDecl(ref pname, ref ptype)) in def.parameters().iter().enumerate() {
                let llarg = LLVMGetParam(fun, i as c_uint);
                // let lltype = self.get_llvm_type(ptype.body());
                LLVMSetValueName(llarg, raw_string(pname.as_str()));

                let alloca = self.create_entry_block_alloca(fun, pname.as_str(), ptype.body());

                LLVMBuildStore(self.builder, llarg, alloca);
                symtbl.insert(pname.as_str(), alloca);
            }

            // TODO: Make closure pointer a parameter
            // make free var symbols point to free var allocas

            let fun_body = self.gen_expr(def.body(), &mut symtbl);
            LLVMBuildRet(self.builder, fun_body);

            if LLVMVerifyFunction(fun, LLVMVerifierFailureAction::LLVMPrintMessageAction) != 0 {
                println!("Function verify failed");
            }
        }
    }

    pub fn dump(&mut self) {
        unsafe {
            LLVMDumpModule(self.module);
        }
    }

    fn gensym(&mut self) -> Result<CString, NulError> {
        let mut f = String::new();
        f.push_str(self.unique.to_string().as_str());
        self.unique = self.unique + 1;
        CString::new(f)
    }

    unsafe fn get_fun_type(&mut self, scm: &Scheme) -> LLVMTypeRef {
        match scm {
            &Scheme::Mono(ref ty) => self.get_llvm_type(ty.deref()),
            _ => unreachable!()
        }
    }

    unsafe fn get_llvm_type(&mut self, ty: &Type) -> LLVMTypeRef {
        use self::Type::*;
        match ty {
            &Con(ref n) => match n.as_str() {
                // Primary types
                "Int" => LLVMInt32TypeInContext(self.context),
                "Float" => LLVMDoubleTypeInContext(self.context),
                "Char" => LLVMInt8TypeInContext(self.context),

                // User defined types
                t => self.gen_user_type(t)
            },
            &Arr(ref p, ref ret) => {
                let mut psty: Vec<LLVMTypeRef> = vec![];
                let mut r: &Type = p.deref();
                loop {
                    if let &Prod(ref t, ref rest) = r {
                        psty.push(self.get_llvm_type(t.deref()));
                        r = rest;
                    } else {
                        psty.push(self.get_llvm_type(r.deref()));
                        break;
                    }
                }
                let argc = psty.len();
                LLVMFunctionType(self.get_llvm_type(ret.deref()), psty.as_mut_ptr(), argc as c_uint, 0)
            },
            &Void => LLVMVoidTypeInContext(self.context),
            &Prod(ref l, ref r) => unimplemented!(), // TODO: make a struct represent tuple
            &Comp(ref c, ref p) => unimplemented!(), // TODO: determine the type

            &Var(..) => panic!("Unmaterized type"),
        }
    }

    unsafe fn gen_lit(&mut self, lit: &Lit) -> LLVMValueRef {
        use self::Lit::*;
        match lit {
            &Float(f) => LLVMConstReal(LLVMDoubleTypeInContext(self.context), f),
            &Int(i) => LLVMConstInt(LLVMInt32TypeInContext(self.context), i as c_ulonglong, 1),
            &Bool(true) => LLVMConstInt(LLVMInt1TypeInContext(self.context), 1 as c_ulonglong, 0),
            &Bool(false) => LLVMConstInt(LLVMInt1TypeInContext(self.context), 0 as c_ulonglong, 0),
            // TODO: String represent
            &Str(ref s) => unimplemented!(),
        }
    }

    unsafe fn get_or_add_function(&mut self, fname: &str, fty: &Type) -> LLVMValueRef {

        // TODO: make closure pointer a parameter

        let n = CString::new(fname).unwrap().into_raw();
        let f = LLVMGetNamedFunction(self.module, n);
        if f.is_null() {
            let llvm_ty = self.get_llvm_type(fty);
            LLVMAddFunction(
                self.module,
                raw_string(fname),
                llvm_ty)
        } else {
            f
        }
    }

    unsafe fn gen_user_type(&mut self, tyname: &str) -> LLVMTypeRef {
        // TODO: make a user defined type definition
        unimplemented!()
    }

    unsafe fn create_entry_block_alloca(&mut self, fun: LLVMValueRef, var_name: &str, ty: &Type) -> LLVMValueRef {
        let builder = LLVMCreateBuilderInContext(self.context);
        let bb = LLVMGetEntryBasicBlock(fun);
        let fi = LLVMGetFirstInstruction(bb);
        let llvm_ty = self.get_llvm_type(ty);
        LLVMPositionBuilder(builder, bb, fi);
        LLVMBuildAlloca(builder, llvm_ty, raw_string(var_name))
    }


    unsafe fn gen_expr<'a: 'b, 'b>(
        &mut self,
        term: &'a TaggedTerm,
        symbols: &mut VarEnv<'b>)
        -> LLVMValueRef
    {
        // TODO: fix variable ref
        //  change `form` code to `term` code
        //  get var type from environment
        //  instead of form attribute
        use self::Term::*;
        // println!("{:?}", form);
        match *term.body() {
            Lit(ref lit) => self.gen_lit(lit),
            Var(ref vn) => {
                let var_name = vn.as_str();
                match symbols.lookup(&var_name) {
                    Some(v) => {
                        LLVMBuildLoad(self.builder, *v, raw_string(var_name))
                        // *v
                    },
                    // Impossible because of type check
                    None => self.get_or_add_function(var_name, term.ref_scheme().body())
                }
            }
            Binary(op, ref lhs, ref rhs) => {
                let lval = self.gen_expr(lhs, symbols);
                let rval = self.gen_expr(rhs, symbols);
                let instr = get_llvm_op(op, lhs.ref_scheme().body());

                instr(self.builder, lval, rval, self.gensym().unwrap().into_raw())
            }
            Let(ref var_decl, ref val, ref exp) => {
                let &VarDecl(ref var, ref tyvar) = var_decl;
                let var_name = var.as_str();

                let blk = LLVMGetInsertBlock(self.builder);
                let fun = LLVMGetBasicBlockParent(blk);

                let init = self.gen_expr(val, symbols);
                let alloca = self.create_entry_block_alloca(fun, var_name, tyvar.body());
                LLVMBuildStore(self.builder, init, alloca);

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

                LLVMBuildCall(
                    self.builder,
                    callee_ref,
                    argsv.as_mut_ptr(),
                    argsv.len() as c_uint,
                    self.gensym().unwrap().into_raw())
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
            },
            MakeCls(ref var_decl, ref cls, ref exp) => unimplemented!(),
            List(_) => unimplemented!(),
            Unary(_, _) => unimplemented!(),
            If(_, _, _) => unimplemented!()
        }
    }
}

