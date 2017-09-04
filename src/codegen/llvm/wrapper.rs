pub use llvm_sys::prelude::{LLVMBuilderRef, LLVMContextRef, LLVMModuleRef, LLVMPassManagerRef,
                            LLVMTypeRef, LLVMValueRef, LLVMBasicBlockRef};
pub use llvm_sys::execution_engine::{LLVMExecutionEngineRef, LLVMGenericValueRef,
                                     LLVMGenericValueToFloat, LLVMRunFunction};
pub use llvm_sys::analysis::{LLVMVerifierFailureAction, LLVMVerifyFunction};
pub use llvm_sys::LLVMRealPredicate;
pub use llvm_sys::core::*;

pub use libc::{c_char, c_uint, c_ulonglong};
use std::char;
use std::ffi::{CString, NulError};
use std::ptr;



pub trait LLVMWrapper<T> {
    fn from_ref(ptr: T) -> Self;
    fn raw_ptr(&self) -> T;
}

macro_rules! make_LLVM_wrapper {
    ($origin:ty, $wrapper:ident) => {
        #[derive(Debug, Clone)]
        pub struct $wrapper($origin);
        impl LLVMWrapper<$origin> for $wrapper {
            fn from_ref(ptr: $origin) -> Self {
                $wrapper(ptr)
            }
            fn raw_ptr(&self) -> $origin {
                self.0.clone()
            }
        }
    };
    ($origin:ty, $wrapper:ident, Copy) => {
        #[derive(Debug, Clone, Copy)]
        pub struct $wrapper($origin);
        impl LLVMWrapper<$origin> for $wrapper {
            fn from_ref(ptr: $origin) -> Self {
                $wrapper(ptr)
            }
            fn raw_ptr(&self) -> $origin {
                self.0.clone()
            }
        }
    }
}

make_LLVM_wrapper!(LLVMModuleRef, LLVMModule);
make_LLVM_wrapper!(LLVMContextRef, LLVMContext);
make_LLVM_wrapper!(LLVMValueRef, LLVMValue, Copy);
make_LLVM_wrapper!(LLVMValueRef, LLVMFunction, Copy);
make_LLVM_wrapper!(LLVMTypeRef, LLVMType, Copy);
make_LLVM_wrapper!(LLVMBuilderRef, LLVMBuilder);
make_LLVM_wrapper!(LLVMBasicBlockRef, LLVMBasicBlock);


pub unsafe fn raw_string(s: &str) -> *mut c_char {
    CString::new(s).unwrap().into_raw()
}


macro_rules! method_type_getter {
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
    method_type_getter!(get_int1_type, LLVMInt1TypeInContext);
    method_type_getter!(get_int8_type, LLVMInt8TypeInContext);
    method_type_getter!(get_int16_type, LLVMInt16TypeInContext);
    method_type_getter!(get_int32_type, LLVMInt32TypeInContext);
    method_type_getter!(get_double_type, LLVMDoubleTypeInContext);
    method_type_getter!(get_void_type, LLVMVoidTypeInContext);

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
    pub fn get_ptr(&self, address_space: usize) -> Self {
        unsafe { LLVMType(LLVMPointerType(self.0.clone(), address_space as c_uint)) }
    }
}

impl LLVMValue {
    pub fn set_name(&self, name: &str) {
        unsafe { LLVMSetValueName(self.0.clone(), raw_string(name)) }
    }

    pub fn into_function(self) -> LLVMFunction {
        LLVMFunction::from_ref(self.0)
    }
    pub fn get_type(&self) -> LLVMType {
        unsafe {
            LLVMType::from_ref(LLVMTypeOf(self.0))
        }
    }
}

impl LLVMFunction {
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
    method_build_instr!(bit_cast, LLVMBuildBitCast, val: &LLVMValue, dest_ty: &LLVMType => name: &str);

    pub fn call(&self, fun: &LLVMFunction, args: &mut Vec<LLVMValue>, name: &str) -> LLVMValue {
        let mut _args: Vec<_> = args.iter_mut().map(|arg| arg.raw_ptr()).collect();
        unsafe {
            let ret = LLVMBuildCall(self.raw_ptr(),
                                    fun.raw_ptr(),
                                    _args.as_mut_ptr(),
                                    args.len() as c_uint,
                                    raw_string(name));
            LLVMValue::from_ref(ret)
        }

    }

    pub fn struct_field_ptr(&self, ptr: &LLVMValue, idx: usize, name: &str) -> LLVMValue {
        unsafe {
            let ret = LLVMBuildStructGEP(self.raw_ptr(), ptr.raw_ptr(), idx as u32, raw_string(name));
            LLVMValue::from_ref(ret)
        }
    }
}

impl LLVMBasicBlock {
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

pub type LLVMOperateBuild = unsafe extern "C" fn(LLVMBuilderRef,
                                             LLVMValueRef,
                                             LLVMValueRef,
                                             *const ::libc::c_char)
                                             -> LLVMValueRef;

