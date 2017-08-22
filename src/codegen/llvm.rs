use std::char;
use std::ffi::{CString, NulError};
use std::io::{self, BufReader, Read, Write};
use std::ptr;
use std::collections::HashMap;
use std::iter::*;

use std::ops::Deref;
pub use libc::{c_char, c_uint, c_ulonglong};


pub use llvm_sys::prelude::{LLVMBuilderRef, LLVMContextRef, LLVMModuleRef, LLVMPassManagerRef,
                        LLVMTypeRef, LLVMValueRef};
pub use llvm_sys::execution_engine::{LLVMExecutionEngineRef, LLVMGenericValueRef,
                                 LLVMGenericValueToFloat, LLVMRunFunction};
pub use llvm_sys::analysis::{LLVMVerifierFailureAction, LLVMVerifyFunction};
pub use llvm_sys::LLVMRealPredicate;
pub use llvm_sys::core::*;

// use codegen::llvm::*;
use core::*;
use internal::*;
use types::*;
use utils::*;


#[derive(Debug)]
pub struct LLVMModule(LLVMModuleRef);
#[derive(Debug)]
pub struct LLVMContext(LLVMContextRef);
#[derive(Debug)]
pub struct LLVMValue(LLVMValueRef);
#[derive(Debug)]
pub struct LLVMFunction(LLVMValueRef);
#[derive(Debug)]
pub struct LLVMType(LLVMTypeRef);
#[derive(Debug)]
pub struct LLVMBuilder(LLVMBuilderRef);

macro_rules! method_raw_ptr {
    ($raw_type: ty) => {
    pub fn raw_ptr(&mut self) -> $raw_type {
        self.0
    }
    };
}
macro_rules! method_get_type {
    ($name: ident, $fun: ident) => {
    pub fn $name(&self) -> LLVMType {
        unsafe {
            LLVMType::from_ref($fun(self.0))
        }
    }
    };
}

impl LLVMContext {
    pub fn new() -> Self {
        unsafe {
            LLVMContext(LLVMContextCreate())
        }
    }
    method_raw_ptr!(LLVMContextRef);
    method_get_type!(get_int1_type, LLVMInt1TypeInContext);
    method_get_type!(get_int8_type, LLVMInt8TypeInContext);
    method_get_type!(get_int32_type, LLVMInt32TypeInContext);
    method_get_type!(get_double_type, LLVMDoubleTypeInContext);
    method_get_type!(get_void_type, LLVMVoidTypeInContext);

    pub fn get_function_type(ret: &mut LLVMType, param: &mut Vec<LLVMType>, is_var_arg: bool) -> LLVMType {
        let mut ps: Vec<_> = param.iter_mut().map(|t| t.raw_ptr()).collect();
        let pc = ps.len() as c_uint;
        let flag = if is_var_arg { 1 } else { 0 };
        let fun = unsafe {
            LLVMFunctionType(ret.raw_ptr(), ps.as_mut_ptr(), pc, flag)
        };
        LLVMType::from_ref(fun)
    }

    pub fn get_struct_type(&mut self, types: &mut Vec<LLVMType>, packed: bool) -> LLVMType {
        let mut mems: Vec<_> = types.iter_mut().map(|t| t.raw_ptr()).collect();
        let flag = if packed { 1 } else { 0 };
        let t = unsafe {
            LLVMStructTypeInContext(self.0, mems.as_mut_ptr(), mems.len() as c_uint, flag)
        };
        LLVMType(t)
    }
}

impl LLVMType {
    pub fn from_ref(ptr: LLVMTypeRef) -> Self {
        LLVMType(ptr)
    }
    method_raw_ptr!(LLVMTypeRef);
}

impl Drop for LLVMModule {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeModule(self.0);
        }
    }
}
impl Drop for LLVMContext {
    fn drop(&mut self) {
        unsafe {
            LLVMContextDispose(self.0);
        }
    }
}
impl Drop for LLVMBuilder {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeBuilder(self.0);
        }
    }
}

#[derive(Debug, Clone)]
pub struct LLVMCodegen {
    pub module: LLVMModuleRef,
    pub builder: LLVMBuilderRef,
    pub context: LLVMContextRef,
    unique: usize,
}

pub type VarEnv<'a> = SymTable<'a, &'a str, LLVMValueRef>;

type LLVMOperateBuild = unsafe extern "C" fn(LLVMBuilderRef,
                                             LLVMValueRef,
                                             LLVMValueRef,
                                             *const ::libc::c_char)
                                             -> LLVMValueRef;

pub fn get_llvm_op(op: BinOp,
               operand_ty: &Type)
               -> LLVMOperateBuild {
    use self::BinOp::*;
    use self::Type::*;
    if let &Con(ref ty_name) = operand_ty {
        let name_ref = ty_name.as_str();
        match (op, name_ref) {
            (Add, "Int") => LLVMBuildAdd,
            (Add, "Float") => LLVMBuildFAdd,
            (Sub, "Int") => LLVMBuildSub,
            (Sub, "Float") => LLVMBuildFSub,
            (Mul, "Int") => LLVMBuildMul,
            (Mul, "Float") => LLVMBuildFMul,
            (Div, _) => LLVMBuildFDiv, // TODO: I dont know exactly which builder
            (Rem, _) => LLVMBuildURem, //       should I use for these two
            _ => unimplemented!(),
        }
    } else {
        unreachable!()
    }
}

pub fn is_primitive_type(t: &Type) -> bool {
    if let &Type::Con(ref n) = t {
        match n.as_str() {
            "Int" | "Float" | "Char" => true,
            _ => false
        }
    } else {
        false
    }
}

pub unsafe fn raw_string(s: &str) -> *mut c_char {
    CString::new(s).unwrap().into_raw()
}

impl LLVMCodegen {
    pub fn new(name: &str) -> LLVMCodegen {
        unsafe {
            let ctx = LLVMContextCreate();
            let modu = LLVMModuleCreateWithNameInContext(raw_string(name), ctx);
            let builder = LLVMCreateBuilderInContext(ctx);
            LLVMCodegen {
                module: modu,
                context: ctx,
                builder: builder,
                unique: 0,
            }
        }
    }


    pub fn new_symbol(&mut self) -> Result<CString, NulError> {
        let mut f = String::new();
        f.push_str(self.unique.to_string().as_str());
        self.unique = self.unique + 1;
        CString::new(f)
    }

    pub unsafe fn get_fun_type(&mut self, scm: &Scheme) -> LLVMTypeRef {
        match scm {
            &Scheme::Mono(ref ty) => self.get_llvm_type(ty.deref()),
            _ => unreachable!(),
        }
    }

    fn get_closure_type(&mut self) -> LLVMTypeRef {
        unsafe {
            let ptr = LLVMPointerType(LLVMInt8TypeInContext(self.context), 0);
            let mut mem = vec![LLVMInt16TypeInContext(self.context), ptr];
            LLVMStructTypeInContext(self.context, mem.as_mut_ptr(), mem.len() as c_uint, 0)
        }
    }

    pub unsafe fn get_llvm_type_or_ptr(&mut self, ty: &Type) -> LLVMTypeRef {
        let ret = self.get_llvm_type(ty);
        if !is_primitive_type(ty) {
            LLVMPointerType(ret, 0 as c_uint)
        } else {
            ret
        }
    }

    pub unsafe fn get_llvm_type(&mut self, ty: &Type) -> LLVMTypeRef {
        use self::Type::*;
        match ty {
            &Con(ref n) => {
                match n.as_str() {
                    // Primary types
                    "Int" => LLVMInt32TypeInContext(self.context),
                    "Float" => LLVMDoubleTypeInContext(self.context),
                    "Char" => LLVMInt8TypeInContext(self.context),

                    // User defined types
                    t => self.gen_user_type(t),
                }
            }
            &Arr(box ref p, box ref ret) => {
                // type of parameters and returned value should be pointer if not primitive
                let cls_type = LLVMPointerType(self.get_closure_type(), 0);
                let mut retty = self.get_llvm_type_or_ptr(ret);
                let psty = p.prod_to_vec();
                let mut llvm_psty: Vec<_> =
                    psty.into_iter().map(|t| self.get_llvm_type_or_ptr(t)).collect();
                llvm_psty.push(cls_type);
                let argc = llvm_psty.len();
                println!("{:?}", llvm_psty);
                LLVMFunctionType(retty, llvm_psty.as_mut_ptr(), argc as c_uint, 0)
            }
            &Void => LLVMVoidTypeInContext(self.context),
            &Prod(..) => {
                // let ty_name = ty.to_string().as_str();

                // let llvm_t = LLVMGetStructName();
                let mut tys: Vec<_> = ty.prod_to_vec()
                    .iter()
                    .map(|t| self.get_llvm_type(t))
                    .collect();
                let count = tys.len();
                LLVMStructTypeInContext(self.context, tys.as_mut_ptr(), count as c_uint, 0)
            } // TODO: make a struct represent tuple
            &Comp(ref c, ref p) => unimplemented!(), // TODO: determine the type

            &Var(..) => panic!("Unmaterized type"),
        }
    }

    pub unsafe fn gen_lit(&mut self, lit: &Lit) -> LLVMValueRef {
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

    pub unsafe fn get_or_add_function(&mut self, fname: &str, fty: &Type) -> LLVMValueRef {

        // TODO: make closure pointer a parameter

        let n = raw_string(fname);
        let f = LLVMGetNamedFunction(self.module, n);
        if f.is_null() {
            let llvm_ty = self.get_llvm_type(fty);
            LLVMAddFunction(self.module, raw_string(fname), llvm_ty)
        } else {
            f
        }
    }

    pub unsafe fn gen_user_type(&mut self, tyname: &str) -> LLVMTypeRef {
        // TODO: make a user defined type definition
        unimplemented!()
    }

    pub unsafe fn create_entry_block_alloca(&mut self,
                                        fun: LLVMValueRef,
                                        var_name: &str,
                                        ty: &Type)
                                        -> LLVMValueRef {
        let builder = LLVMCreateBuilderInContext(self.context);
        let bb = LLVMGetEntryBasicBlock(fun);
        let fi = LLVMGetFirstInstruction(bb);
        let llvm_ty = self.get_llvm_type(ty);
        LLVMPositionBuilder(builder, bb, fi);
        LLVMBuildAlloca(builder, llvm_ty, raw_string(var_name))
    }


}



