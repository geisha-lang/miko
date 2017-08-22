use std::char;
use std::ffi::{CString, NulError};
use std::io::{self, BufReader, Read, Write};
use std::ptr;
use std::collections::HashMap;
use std::iter::*;

use std::ops::Deref;
pub use libc::{c_char, c_uint, c_ulonglong};


pub use llvm_sys::prelude::{LLVMBuilderRef, LLVMContextRef, LLVMModuleRef, LLVMPassManagerRef,
                            LLVMTypeRef, LLVMValueRef, LLVMBasicBlockRef};
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


#[derive(Debug, Clone)]
pub struct LLVMModule(LLVMModuleRef);
#[derive(Debug, Clone)]
pub struct LLVMContext(LLVMContextRef);
#[derive(Debug, Clone)]
pub struct LLVMValue(LLVMValueRef);
#[derive(Debug, Clone)]
pub struct LLVMFunction(LLVMValueRef);
#[derive(Debug, Clone)]
pub struct LLVMType(LLVMTypeRef);
#[derive(Debug, Clone)]
pub struct LLVMBuilder(LLVMBuilderRef);
#[derive(Debug, Clone)]
pub struct LLVMBasicBlock(LLVMBasicBlockRef);


macro_rules! method_raw_ptr {
    ($raw_type: ty) => {
        pub fn raw_ptr(&self) -> $raw_type {
            self.0.clone()
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
        unsafe { LLVMContext(LLVMContextCreate()) }
    }
    method_raw_ptr!(LLVMContextRef);
    method_get_type!(get_int1_type, LLVMInt1TypeInContext);
    method_get_type!(get_int8_type, LLVMInt8TypeInContext);
    method_get_type!(get_int16_type, LLVMInt16TypeInContext);
    method_get_type!(get_int32_type, LLVMInt32TypeInContext);
    method_get_type!(get_double_type, LLVMDoubleTypeInContext);
    method_get_type!(get_void_type, LLVMVoidTypeInContext);

    pub fn get_function_type(ret: &LLVMType, param: &Vec<LLVMType>, is_var_arg: bool) -> LLVMType {
        let mut ps: Vec<_> = param.iter().map(|t| t.raw_ptr()).collect();
        let pc = ps.len() as c_uint;
        let flag = if is_var_arg { 1 } else { 0 };
        let fun = unsafe { LLVMFunctionType(ret.raw_ptr(), ps.as_mut_ptr(), pc, flag) };
        LLVMType::from_ref(fun)
    }

    pub fn get_struct_type(&self, types: &Vec<LLVMType>, packed: bool) -> LLVMType {
        let mut mems: Vec<_> = types.iter().map(|t| t.raw_ptr()).collect();
        let flag = if packed { 1 } else { 0 };
        let t = unsafe {
            LLVMStructTypeInContext(self.0.clone(),
                                    mems.as_mut_ptr(),
                                    mems.len() as c_uint,
                                    flag)
        };
        LLVMType(t)
    }

    pub fn get_double_const(&self, val: f64) -> LLVMValue {
        unsafe {
            LLVMValue::from_ref(LLVMConstReal(self.get_double_type().raw_ptr(),
                                              val as ::libc::c_double))
        }
    }
    pub fn get_int32_const(&self, val: i32) -> LLVMValue {
        unsafe {
            LLVMValue::from_ref(LLVMConstInt(self.get_int32_type().raw_ptr(),
                                             val as c_ulonglong,
                                             1))
        }
    }
    pub fn get_uint8_const(&self, val: u8) -> LLVMValue {
        unsafe {
            LLVMValue::from_ref(LLVMConstInt(self.get_int8_type().raw_ptr(), val as c_ulonglong, 0))
        }
    }
    pub fn get_int1_const(&self, val: u64) -> LLVMValue {
        unsafe {
            LLVMValue::from_ref(LLVMConstInt(self.get_int1_type().raw_ptr(), val as c_ulonglong, 1))
        }
    }

    pub fn append_basic_block(&self, fun: &LLVMFunction, name: &str) -> LLVMBasicBlock {
        unsafe {
            LLVMBasicBlock::from_ref(LLVMAppendBasicBlockInContext(self.raw_ptr(),
                                                                   fun.raw_ptr(),
                                                                   raw_string(name)))
        }
    }
}

impl LLVMModule {
    pub fn new(name: &str) -> Self {
        unsafe { LLVMModule(LLVMModuleCreateWithName(raw_string(name))) }
    }
    pub fn in_ctx(name: &str, ctx: &LLVMContext) -> Self {
        unsafe { LLVMModule(LLVMModuleCreateWithNameInContext(raw_string(name), ctx.raw_ptr())) }
    }
    method_raw_ptr!(LLVMModuleRef);
    pub fn dump(&self) {
        unsafe { LLVMDumpModule(self.raw_ptr()) }
    }

    pub fn get_or_add_function(&self, fun_name: &str, fty: &LLVMType) -> LLVMFunction {
        unsafe {
            let n = raw_string(fun_name);
            let f = LLVMGetNamedFunction(self.0.clone(), n);
            let t = if f.is_null() {
                LLVMAddFunction(self.raw_ptr(), raw_string(fun_name), fty.raw_ptr())
            } else {
                f
            };
            LLVMFunction::from_ref(t)
        }
    }
}

impl LLVMType {
    pub fn from_ref(ptr: LLVMTypeRef) -> Self {
        LLVMType(ptr)
    }
    method_raw_ptr!(LLVMTypeRef);

    pub fn get_ptr(&self, address_space: usize) -> Self {
        unsafe { LLVMType(LLVMPointerType(self.0.clone(), address_space as c_uint)) }
    }
}

impl LLVMValue {
    pub fn from_ref(ptr: LLVMValueRef) -> Self {
        LLVMValue(ptr)
    }
    method_raw_ptr!(LLVMValueRef);

    pub fn set_name(&self, name: &str) {
        unsafe { LLVMSetValueName(self.0.clone(), raw_string(name)) }
    }

    pub fn into_function(self) -> LLVMFunction {
        LLVMFunction::from_ref(self.0)
    }
}

impl LLVMFunction {
    method_raw_ptr!(LLVMValueRef);
    pub fn from_ref(ptr: LLVMValueRef) -> Self {
        unsafe { LLVMFunction(ptr) }
    }
    pub fn into_value(self) -> LLVMValue {
        LLVMValue::from_ref(self.0)
    }
    pub fn count_basic_blocks(&self) -> usize {
        unsafe { LLVMCountBasicBlocks(self.raw_ptr()) as usize }
    }

    pub fn count_params(&self) -> usize {
        unsafe { LLVMCountParams(self.raw_ptr()) as usize }
    }

    pub fn get_param(&self, idx: usize) -> LLVMValue {
        unsafe { LLVMValue::from_ref(LLVMGetParam(self.raw_ptr(), idx as c_uint)) }
    }

    pub fn get_entry_basic_block(&self) -> LLVMBasicBlock {
        unsafe { LLVMBasicBlock::from_ref(LLVMGetEntryBasicBlock(self.raw_ptr())) }
    }
}

macro_rules! method_build_instr {
    ($name: ident, $fun: ident, $($param:ident : $ty:ty),* => $t:ident: &str) => {
    pub fn $name(&self, $($param: $ty),* , $t: &str) -> LLVMValue {
        unsafe {
            LLVMValue::from_ref($fun(self.raw_ptr(), $($param.raw_ptr()),*, raw_string($t)))
        }
    }
    };
    ($name: ident, $fun: ident, $($param:ident : $ty:ty),*) => {
    pub fn $name(&self, $($param: $ty),*) -> LLVMValue {
        unsafe {
            LLVMValue::from_ref($fun(self.raw_ptr(), $($param.raw_ptr()),*))
        }
    }
    };
}

impl LLVMBuilder {
    pub fn in_ctx(ctx: &LLVMContext) -> Self {
        unsafe { LLVMBuilder(LLVMCreateBuilderInContext(ctx.raw_ptr())) }
    }
    method_raw_ptr!(LLVMBuilderRef);

    pub fn set_position(&self, block: &LLVMBasicBlock, instr: &LLVMValue) {
        unsafe { LLVMPositionBuilder(self.raw_ptr(), block.raw_ptr(), instr.raw_ptr()) }
    }
    pub fn set_position_at_end(&self, block: &LLVMBasicBlock) {
        unsafe { LLVMPositionBuilderAtEnd(self.raw_ptr(), block.raw_ptr()) }
    }

    pub fn insert_block(&self) -> LLVMBasicBlock {
        unsafe { LLVMBasicBlock::from_ref(LLVMGetInsertBlock(self.raw_ptr())) }
    }

    method_build_instr!(alloca, LLVMBuildAlloca, ty: &LLVMType => target: &str);
    method_build_instr!(load, LLVMBuildLoad, ptr: &LLVMValue => target: &str);
    method_build_instr!(store, LLVMBuildStore, val: &LLVMValue, ptr: &LLVMValue);
    method_build_instr!(ret, LLVMBuildRet, val: &LLVMValue);

    pub fn call(&self, fun: &LLVMFunction, args: &mut Vec<LLVMValue>, name: &str) -> LLVMValue {
        let mut _args: Vec<_> = args.iter_mut().map(|arg| arg.raw_ptr()).collect();
        unsafe {
            LLVMValue::from_ref(LLVMBuildCall(self.raw_ptr(),
                                              fun.raw_ptr(),
                                              _args.as_mut_ptr(),
                                              args.len() as c_uint,
                                              raw_string(name)))
        }

    }

    pub fn struct_field(&self, ptr: &LLVMValue, idx: u32, name: &str) -> LLVMValue {
        unsafe {
            LLVMValue::from_ref(LLVMBuildStructGEP(self.raw_ptr(),
                                                   ptr.raw_ptr(),
                                                   idx,
                                                   raw_string(name)))
        }
    }
}

impl LLVMBasicBlock {
    pub fn from_ref(ptr: LLVMBasicBlockRef) -> Self {
        LLVMBasicBlock(ptr)
    }
    method_raw_ptr!(LLVMBasicBlockRef);

    pub fn get_parent(&self) -> LLVMFunction {
        unsafe { LLVMFunction::from_ref(LLVMGetBasicBlockParent(self.raw_ptr())) }
    }

    pub fn get_first_instr(&self) -> LLVMValue {
        unsafe { LLVMValue::from_ref(LLVMGetFirstInstruction(self.raw_ptr())) }
    }
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
    pub module: LLVMModule,
    pub builder: LLVMBuilder,
    pub context: LLVMContext,
    unique: usize,
}


type LLVMOperateBuild = unsafe extern "C" fn(LLVMBuilderRef,
                                             LLVMValueRef,
                                             LLVMValueRef,
                                             *const ::libc::c_char)
                                             -> LLVMValueRef;

pub fn get_llvm_op(op: BinOp, operand_ty: &Type) -> LLVMOperateBuild {
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
            _ => false,
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
            let ctx = LLVMContext::new();
            let modu = LLVMModule::in_ctx(name, &ctx);
            let builder = LLVMBuilder::in_ctx(&ctx);
            LLVMCodegen {
                module: modu,
                context: ctx,
                builder: builder,
                unique: 0,
            }
        }
    }


    pub fn new_symbol(&mut self) -> Result<CString, NulError> {
        let f = String::new() + self.unique.to_string().as_str();
        self.unique = self.unique + 1;
        CString::new(f)
    }

    pub fn new_symbol_string(&mut self) -> String {
        let f = String::new() + self.unique.to_string().as_str();
        self.unique = self.unique + 1;
        f
    }

    fn get_closure_type(&self) -> LLVMType {
        unsafe {

            let ptr = self.context.get_int8_type().get_ptr(0);
            let mem = vec![self.context.get_int16_type(), ptr];
            self.context.get_struct_type(&mem, false)
        }
    }

    pub fn get_llvm_type_or_ptr(&self, ty: &Type) -> LLVMType {
        let ret = self.get_llvm_type(ty);
        if !is_primitive_type(ty) {
            ret.get_ptr(0)
        } else {
            ret
        }
    }

    pub fn get_llvm_type(&self, ty: &Type) -> LLVMType {
        use self::Type::*;
        match ty {
            &Con(ref n) => {
                match n.as_str() {

                    // Primary types
                    "Int" => self.context.get_int32_type(),
                    "Float" => self.context.get_double_type(),
                    "Char" => self.context.get_int8_type(),

                    // User defined types
                    t => self.gen_user_type(t),
                }
            }
            &Arr(box ref p, box ref ret) => {
                // Type of parameters and returned value should be pointer if not primitive
                let mut cls_type = self.get_closure_type().get_ptr(0);
                let mut retty = self.get_llvm_type_or_ptr(ret);
                let psty = p.prod_to_vec();
                let mut llvm_psty: Vec<_> =
                    psty.into_iter().map(|t| self.get_llvm_type_or_ptr(t)).collect();
                llvm_psty.push(cls_type);
                LLVMContext::get_function_type(&retty, &llvm_psty, false)
            }
            &Void => self.context.get_void_type(),
            &Prod(..) => {
                let mut tys: Vec<_> = ty.prod_to_vec()
                    .iter()
                    .map(|t| self.get_llvm_type(t))
                    .collect();
                self.context.get_struct_type(&tys, true)
            }
            &Comp(ref c, ref p) => unimplemented!(), // TODO: determine the type

            &Var(..) => panic!("Unmaterized type"),
        }
    }

    pub fn gen_lit(&mut self, lit: &Lit) -> LLVMValue {
        use self::Lit::*;
        match lit {
            &Float(f) => self.context.get_double_const(f),
            &Int(i) => self.context.get_int32_const(i),
            &Bool(true) => self.context.get_int1_const(1),
            &Bool(false) => self.context.get_int1_const(0),
            // TODO: String represent
            &Str(ref s) => unimplemented!(),
        }
    }

    pub fn gen_user_type(&self, tyname: &str) -> LLVMType {
        // TODO: make a user defined type definition
        unimplemented!()
    }

    pub unsafe fn create_entry_block_alloca(&self,
                                            fun: &LLVMFunction,
                                            var_name: &str,
                                            ty: &Type)
                                            -> LLVMValue {
        let builder = LLVMBuilder::in_ctx(&self.context);
        let block = fun.get_entry_basic_block();
        let fi = block.get_first_instr();
        let llvm_ty = self.get_llvm_type(ty);
        builder.set_position(&block, &fi);
        builder.alloca(&llvm_ty, var_name)
    }

    pub fn bin_operator(&mut self,
                        op: BinOp,
                        lhs: LLVMValue,
                        rhs: LLVMValue,
                        operand_ty: &Type)
                        -> LLVMValue {
        let fun = get_llvm_op(op, operand_ty);
        unsafe {
            LLVMValue::from_ref(fun(self.builder.raw_ptr(),
                                    lhs.raw_ptr(),
                                    rhs.raw_ptr(),
                                    self.new_symbol().unwrap().into_raw()))
        }
    }
}
