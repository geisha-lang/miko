pub use llvm_sys::prelude::{
    LLVMBasicBlockRef, LLVMBuilderRef, LLVMContextRef, LLVMModuleRef, LLVMTypeRef, LLVMValueRef,
};
use llvm_sys::analysis::{LLVMVerifierFailureAction, LLVMVerifyFunction};
pub use llvm_sys::core::*;
pub use llvm_sys::{LLVMIntPredicate, LLVMRealPredicate};
use llvm_sys::{LLVMLinkage, LLVMUnnamedAddr};

use libc::{c_char, c_uint, c_ulonglong};
use std::ffi::CString;
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
                self.0
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
                self.0
            }
        }
    };
}

make_LLVM_wrapper!(LLVMModuleRef, LLVMModule);
make_LLVM_wrapper!(LLVMContextRef, LLVMContext);
make_LLVM_wrapper!(LLVMValueRef, LLVMValue, Copy);
make_LLVM_wrapper!(LLVMValueRef, LLVMFunction, Copy);
make_LLVM_wrapper!(LLVMTypeRef, LLVMType, Copy);
make_LLVM_wrapper!(LLVMBuilderRef, LLVMBuilder);
make_LLVM_wrapper!(LLVMBasicBlockRef, LLVMBasicBlock);

pub fn to_cstring(s: &str) -> CString {
    CString::new(s).unwrap_or_else(|_| CString::new(s.replace('\0', "")).unwrap())
}

macro_rules! method_type_getter {
    ($name:ident, $fun:ident) => {
        pub fn $name(&self) -> LLVMType {
            unsafe { LLVMType::from_ref($fun(self.0)) }
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
    method_type_getter!(get_int64_type, LLVMInt64TypeInContext);
    method_type_getter!(get_double_type, LLVMDoubleTypeInContext);
    method_type_getter!(get_void_type, LLVMVoidTypeInContext);

    /// LLVM 15+ opaque pointer type (`ptr`)
    pub fn get_ptr_type(&self) -> LLVMType {
        unsafe { LLVMType::from_ref(LLVMPointerTypeInContext(self.0, 0)) }
    }

    pub fn get_array_type(elem_ty: &LLVMType, count: u64) -> LLVMType {
        unsafe { LLVMType::from_ref(LLVMArrayType2(elem_ty.raw_ptr(), count)) }
    }

    pub fn get_function_type(ret: &LLVMType, param: &[LLVMType], is_var_arg: bool) -> LLVMType {
        let mut ps: Vec<_> = param.iter().map(|t| t.raw_ptr()).collect();
        let pc = ps.len() as c_uint;
        let flag = if is_var_arg { 1 } else { 0 };
        let fun = unsafe { LLVMFunctionType(ret.raw_ptr(), ps.as_mut_ptr(), pc, flag) };
        LLVMType::from_ref(fun)
    }

    pub fn get_struct_type(&self, types: &[LLVMType], packed: bool) -> LLVMType {
        let mut mems: Vec<_> = types.iter().map(|t| t.raw_ptr()).collect();
        let flag = if packed { 1 } else { 0 };
        let t = unsafe {
            LLVMStructTypeInContext(self.0, mems.as_mut_ptr(), mems.len() as c_uint, flag)
        };
        LLVMType(t)
    }

    /// Emit a null-terminated private global constant `[N+1 x i8]` and return its `ptr` (`LLVMValue`).
    pub fn create_global_string(&self, module: &LLVMModule, s: &str, name: &str) -> LLVMValue {
        unsafe {
            let const_arr = LLVMConstStringInContext2(
                self.0,
                s.as_ptr() as *const c_char,
                s.len(),
                0, // null-terminate!
            );
            let arr_ty = LLVMTypeOf(const_arr);
            let cname = to_cstring(name);
            let global = LLVMAddGlobal(module.raw_ptr(), arr_ty, cname.as_ptr());
            LLVMSetInitializer(global, const_arr);
            LLVMSetGlobalConstant(global, 1);
            LLVMSetLinkage(global, LLVMLinkage::LLVMPrivateLinkage);
            LLVMSetUnnamedAddress(global, LLVMUnnamedAddr::LLVMGlobalUnnamedAddr);
            LLVMValue::from_ref(global)
        }
    }

    pub fn get_double_const(&self, val: f64) -> LLVMValue {
        unsafe { LLVMValue::from_ref(LLVMConstReal(self.get_double_type().raw_ptr(), val)) }
    }

    pub fn get_int32_const(&self, val: i32) -> LLVMValue {
        unsafe {
            LLVMValue::from_ref(LLVMConstInt(
                self.get_int32_type().raw_ptr(),
                val as i64 as u64 as c_ulonglong,
                1,
            ))
        }
    }

    pub fn get_uint8_const(&self, val: u8) -> LLVMValue {
        unsafe {
            LLVMValue::from_ref(LLVMConstInt(
                self.get_int8_type().raw_ptr(),
                val as c_ulonglong,
                0,
            ))
        }
    }

    pub fn get_int1_const(&self, val: u64) -> LLVMValue {
        unsafe {
            LLVMValue::from_ref(LLVMConstInt(
                self.get_int1_type().raw_ptr(),
                val as c_ulonglong,
                0,
            ))
        }
    }

    pub fn get_int64_const(&self, val: i64) -> LLVMValue {
        unsafe {
            LLVMValue::from_ref(LLVMConstInt(
                self.get_int64_type().raw_ptr(),
                val as u64 as c_ulonglong,
                1,
            ))
        }
    }

    pub fn append_basic_block(&self, fun: &LLVMFunction, name: &str) -> LLVMBasicBlock {
        let cname = to_cstring(name);
        unsafe {
            LLVMBasicBlock::from_ref(LLVMAppendBasicBlockInContext(
                self.raw_ptr(),
                fun.raw_ptr(),
                cname.as_ptr(),
            ))
        }
    }
}

impl LLVMModule {
    pub fn in_ctx(name: &str, ctx: &LLVMContext) -> Self {
        let cname = to_cstring(name);
        unsafe { LLVMModule(LLVMModuleCreateWithNameInContext(cname.as_ptr(), ctx.raw_ptr())) }
    }

    pub fn dump(&self) {
        unsafe { LLVMDumpModule(self.raw_ptr()) }
    }

    pub fn get_function(&self, fun_name: &str) -> Option<LLVMFunction> {
        let cname = to_cstring(fun_name);
        unsafe {
            let f = LLVMGetNamedFunction(self.0, cname.as_ptr());
            if f.is_null() {
                None
            } else {
                Some(LLVMFunction::from_ref(f))
            }
        }
    }

    pub fn add_function(&self, fun_name: &str, fty: &LLVMType) -> LLVMFunction {
        let cname = to_cstring(fun_name);
        unsafe {
            let f = LLVMAddFunction(self.raw_ptr(), cname.as_ptr(), fty.raw_ptr());
            LLVMFunction::from_ref(f)
        }
    }

    pub fn add_global(&self, ty: &LLVMType, name: &str) -> LLVMValue {
        let cname = to_cstring(name);
        unsafe {
            let g = LLVMAddGlobal(self.raw_ptr(), ty.raw_ptr(), cname.as_ptr());
            LLVMSetInitializer(g, LLVMConstNull(ty.raw_ptr()));
            LLVMValue::from_ref(g)
        }
    }

    pub fn add_external_global(&self, ty: &LLVMType, name: &str) -> LLVMValue {
        let cname = to_cstring(name);
        unsafe {
            let g = LLVMAddGlobal(self.raw_ptr(), ty.raw_ptr(), cname.as_ptr());
            LLVMValue::from_ref(g)
        }
    }

    pub fn get_global(&self, name: &str) -> Option<LLVMValue> {
        let cname = to_cstring(name);
        unsafe {
            let g = LLVMGetNamedGlobal(self.raw_ptr(), cname.as_ptr());
            if g.is_null() {
                None
            } else {
                Some(LLVMValue::from_ref(g))
            }
        }
    }
}

impl LLVMType {
    pub fn get_null(&self) -> LLVMValue {
        unsafe { LLVMValue::from_ref(LLVMConstNull(self.raw_ptr())) }
    }

    pub fn get_undef(&self) -> LLVMValue {
        unsafe { LLVMValue::from_ref(LLVMGetUndef(self.raw_ptr())) }
    }

    pub fn is_void(&self) -> bool {
        unsafe { LLVMGetTypeKind(self.raw_ptr()) == llvm_sys::LLVMTypeKind::LLVMVoidTypeKind }
    }

    pub fn get_return_type(&self) -> Self {
        unsafe { LLVMType(LLVMGetReturnType(self.raw_ptr())) }
    }

    pub fn get_param_type(&self, idx: usize) -> Self {
        unsafe {
            let count = LLVMCountParamTypes(self.0) as usize;
            if idx >= count {
                panic!("Parameter index {} out of bounds (count: {})", idx, count);
            }
            let mut params = vec![ptr::null_mut(); count];
            LLVMGetParamTypes(self.0, params.as_mut_ptr());
            LLVMType(params[idx])
        }
    }

    pub fn count_param_types(&self) -> usize {
        unsafe { LLVMCountParamTypes(self.0) as usize }
    }
}

impl LLVMValue {
    pub fn set_name(&self, name: &str) {
        unsafe { LLVMSetValueName2(self.0, name.as_ptr() as *const c_char, name.len()) }
    }

    pub fn into_function(self) -> LLVMFunction {
        LLVMFunction::from_ref(self.0)
    }

    pub fn get_type(&self) -> LLVMType {
        unsafe { LLVMType::from_ref(LLVMTypeOf(self.0)) }
    }

    pub fn dump(&self) {
        unsafe { LLVMDumpValue(self.raw_ptr()) }
    }

    pub fn set_tail_call(&self, is_tail: bool) {
        unsafe { LLVMSetTailCall(self.raw_ptr(), if is_tail { 1 } else { 0 }) }
    }
}

impl LLVMFunction {
    pub fn into_value(self) -> LLVMValue {
        LLVMValue::from_ref(self.0)
    }

    pub fn get_function_type(&self) -> LLVMType {
        unsafe { LLVMType::from_ref(LLVMGlobalGetValueType(self.raw_ptr())) }
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

    pub fn verify(&self, action: LLVMVerifierFailureAction) -> bool {
        unsafe { LLVMVerifyFunction(self.raw_ptr(), action) == 0 }
    }
}

impl LLVMBuilder {
    pub fn in_ctx(ctx: &LLVMContext) -> Self {
        unsafe { LLVMBuilder(LLVMCreateBuilderInContext(ctx.raw_ptr())) }
    }

    pub fn set_position(&self, block: &LLVMBasicBlock, instr: &LLVMValue) {
        unsafe {
            if instr.raw_ptr().is_null() {
                LLVMPositionBuilderAtEnd(self.raw_ptr(), block.raw_ptr());
            } else {
                LLVMPositionBuilder(self.raw_ptr(), block.raw_ptr(), instr.raw_ptr());
            }
        }
    }

    pub fn set_position_at_end(&self, block: &LLVMBasicBlock) {
        unsafe { LLVMPositionBuilderAtEnd(self.raw_ptr(), block.raw_ptr()) }
    }

    pub fn get_insert_block(&self) -> LLVMBasicBlock {
        unsafe { LLVMBasicBlock::from_ref(LLVMGetInsertBlock(self.raw_ptr())) }
    }

    pub fn current_block_has_terminator(&self) -> bool {
        unsafe {
            let blk = LLVMGetInsertBlock(self.raw_ptr());
            !blk.is_null() && !LLVMGetBasicBlockTerminator(blk).is_null()
        }
    }

    pub fn alloca(&self, ty: &LLVMType, dest: &str) -> LLVMValue {
        let c = to_cstring(dest);
        unsafe { LLVMValue::from_ref(LLVMBuildAlloca(self.raw_ptr(), ty.raw_ptr(), c.as_ptr())) }
    }

    pub fn store(&self, val: &LLVMValue, ptr: &LLVMValue) -> LLVMValue {
        unsafe { LLVMValue::from_ref(LLVMBuildStore(self.raw_ptr(), val.raw_ptr(), ptr.raw_ptr())) }
    }

    /// Explicit-type load (`LLVMBuildLoad2`), compatible with LLVM 20 opaque pointers.
    pub fn load(&self, elem_ty: &LLVMType, ptr: &LLVMValue, dest: &str) -> LLVMValue {
        let c = to_cstring(dest);
        unsafe {
            LLVMValue::from_ref(LLVMBuildLoad2(
                self.raw_ptr(),
                elem_ty.raw_ptr(),
                ptr.raw_ptr(),
                c.as_ptr(),
            ))
        }
    }

    /// Relaxed (`monotonic`) 4-byte aligned atomic load for fast safepoint flag polling.
    pub fn load_atomic_monotonic_i32(&self, elem_ty: &LLVMType, ptr: &LLVMValue, dest: &str) -> LLVMValue {
        let c = to_cstring(dest);
        unsafe {
            let inst = LLVMBuildLoad2(
                self.raw_ptr(),
                elem_ty.raw_ptr(),
                ptr.raw_ptr(),
                c.as_ptr(),
            );
            LLVMSetOrdering(inst, llvm_sys::LLVMAtomicOrdering::LLVMAtomicOrderingMonotonic);
            LLVMSetAlignment(inst, 4);
            LLVMValue::from_ref(inst)
        }
    }

    /// Explicit-type struct GEP (`LLVMBuildStructGEP2`), compatible with LLVM 20 opaque pointers.
    pub fn struct_field_ptr(
        &self,
        struct_ty: &LLVMType,
        ptr: &LLVMValue,
        idx: usize,
        name: &str,
    ) -> LLVMValue {
        let c = to_cstring(name);
        unsafe {
            LLVMValue::from_ref(LLVMBuildStructGEP2(
                self.raw_ptr(),
                struct_ty.raw_ptr(),
                ptr.raw_ptr(),
                idx as u32,
                c.as_ptr(),
            ))
        }
    }

    pub fn gep(
        &self,
        elem_ty: &LLVMType,
        ptr: &LLVMValue,
        indices: &mut [LLVMValue],
        name: &str,
    ) -> LLVMValue {
        let c = to_cstring(name);
        let mut raw_indices: Vec<_> = indices.iter().map(|v| v.raw_ptr()).collect();
        unsafe {
            LLVMValue::from_ref(LLVMBuildGEP2(
                self.raw_ptr(),
                elem_ty.raw_ptr(),
                ptr.raw_ptr(),
                raw_indices.as_mut_ptr(),
                raw_indices.len() as c_uint,
                c.as_ptr(),
            ))
        }
    }

    pub fn add(&self, lhs: &LLVMValue, rhs: &LLVMValue, name: &str) -> LLVMValue {
        let c = to_cstring(name);
        unsafe {
            LLVMValue::from_ref(LLVMBuildAdd(
                self.raw_ptr(),
                lhs.raw_ptr(),
                rhs.raw_ptr(),
                c.as_ptr(),
            ))
        }
    }

    pub fn mul(&self, lhs: &LLVMValue, rhs: &LLVMValue, name: &str) -> LLVMValue {
        let c = to_cstring(name);
        unsafe {
            LLVMValue::from_ref(LLVMBuildMul(
                self.raw_ptr(),
                lhs.raw_ptr(),
                rhs.raw_ptr(),
                c.as_ptr(),
            ))
        }
    }

    pub fn sext(&self, val: &LLVMValue, dest_ty: &LLVMType, name: &str) -> LLVMValue {
        let c = to_cstring(name);
        unsafe {
            LLVMValue::from_ref(LLVMBuildSExt(
                self.raw_ptr(),
                val.raw_ptr(),
                dest_ty.raw_ptr(),
                c.as_ptr(),
            ))
        }
    }

    pub fn trunc(&self, val: &LLVMValue, dest_ty: &LLVMType, name: &str) -> LLVMValue {
        let c = to_cstring(name);
        unsafe {
            LLVMValue::from_ref(LLVMBuildTrunc(
                self.raw_ptr(),
                val.raw_ptr(),
                dest_ty.raw_ptr(),
                c.as_ptr(),
            ))
        }
    }

    pub fn zext(&self, val: &LLVMValue, dest_ty: &LLVMType, name: &str) -> LLVMValue {
        let c = to_cstring(name);
        unsafe {
            LLVMValue::from_ref(LLVMBuildZExt(
                self.raw_ptr(),
                val.raw_ptr(),
                dest_ty.raw_ptr(),
                c.as_ptr(),
            ))
        }
    }

    pub fn sub(&self, lhs: &LLVMValue, rhs: &LLVMValue, name: &str) -> LLVMValue {
        let c = to_cstring(name);
        unsafe {
            LLVMValue::from_ref(LLVMBuildSub(
                self.raw_ptr(),
                lhs.raw_ptr(),
                rhs.raw_ptr(),
                c.as_ptr(),
            ))
        }
    }

    pub fn and(&self, lhs: &LLVMValue, rhs: &LLVMValue, name: &str) -> LLVMValue {
        let c = to_cstring(name);
        unsafe {
            LLVMValue::from_ref(LLVMBuildAnd(
                self.raw_ptr(),
                lhs.raw_ptr(),
                rhs.raw_ptr(),
                c.as_ptr(),
            ))
        }
    }

    pub fn or(&self, lhs: &LLVMValue, rhs: &LLVMValue, name: &str) -> LLVMValue {
        let c = to_cstring(name);
        unsafe {
            LLVMValue::from_ref(LLVMBuildOr(
                self.raw_ptr(),
                lhs.raw_ptr(),
                rhs.raw_ptr(),
                c.as_ptr(),
            ))
        }
    }

    pub fn shl(&self, lhs: &LLVMValue, rhs: &LLVMValue, name: &str) -> LLVMValue {
        let c = to_cstring(name);
        unsafe {
            LLVMValue::from_ref(LLVMBuildShl(
                self.raw_ptr(),
                lhs.raw_ptr(),
                rhs.raw_ptr(),
                c.as_ptr(),
            ))
        }
    }

    pub fn lshr(&self, lhs: &LLVMValue, rhs: &LLVMValue, name: &str) -> LLVMValue {
        let c = to_cstring(name);
        unsafe {
            LLVMValue::from_ref(LLVMBuildLShr(
                self.raw_ptr(),
                lhs.raw_ptr(),
                rhs.raw_ptr(),
                c.as_ptr(),
            ))
        }
    }

    pub fn ret(&self, val: &LLVMValue) -> LLVMValue {
        unsafe { LLVMValue::from_ref(LLVMBuildRet(self.raw_ptr(), val.raw_ptr())) }
    }

    pub fn ret_void(&self) -> LLVMValue {
        unsafe { LLVMValue::from_ref(LLVMBuildRetVoid(self.raw_ptr())) }
    }

    pub fn cond_br(
        &self,
        cond: &LLVMValue,
        then_blk: &LLVMBasicBlock,
        else_blk: &LLVMBasicBlock,
    ) -> LLVMValue {
        unsafe {
            LLVMValue::from_ref(LLVMBuildCondBr(
                self.raw_ptr(),
                cond.raw_ptr(),
                then_blk.raw_ptr(),
                else_blk.raw_ptr(),
            ))
        }
    }

    pub fn br(&self, cont: &LLVMBasicBlock) -> LLVMValue {
        unsafe { LLVMValue::from_ref(LLVMBuildBr(self.raw_ptr(), cont.raw_ptr())) }
    }

    pub fn unreachable(&self) -> LLVMValue {
        unsafe { LLVMValue::from_ref(LLVMBuildUnreachable(self.raw_ptr())) }
    }

    pub fn xor(&self, lhs: &LLVMValue, rhs: &LLVMValue, name: &str) -> LLVMValue {
        let c = to_cstring(name);
        unsafe {
            LLVMValue::from_ref(LLVMBuildXor(
                self.raw_ptr(),
                lhs.raw_ptr(),
                rhs.raw_ptr(),
                c.as_ptr(),
            ))
        }
    }

    pub fn neg(&self, val: &LLVMValue, name: &str) -> LLVMValue {
        let c = to_cstring(name);
        unsafe { LLVMValue::from_ref(LLVMBuildNeg(self.raw_ptr(), val.raw_ptr(), c.as_ptr())) }
    }

    pub fn fneg(&self, val: &LLVMValue, name: &str) -> LLVMValue {
        let c = to_cstring(name);
        unsafe { LLVMValue::from_ref(LLVMBuildFNeg(self.raw_ptr(), val.raw_ptr(), c.as_ptr())) }
    }

    pub fn icmp(&self, pred: LLVMIntPredicate, lhs: &LLVMValue, rhs: &LLVMValue, name: &str) -> LLVMValue {
        let c = to_cstring(name);
        unsafe {
            LLVMValue::from_ref(LLVMBuildICmp(
                self.raw_ptr(),
                pred,
                lhs.raw_ptr(),
                rhs.raw_ptr(),
                c.as_ptr(),
            ))
        }
    }

    pub fn fcmp(&self, pred: LLVMRealPredicate, lhs: &LLVMValue, rhs: &LLVMValue, name: &str) -> LLVMValue {
        let c = to_cstring(name);
        unsafe {
            LLVMValue::from_ref(LLVMBuildFCmp(
                self.raw_ptr(),
                pred,
                lhs.raw_ptr(),
                rhs.raw_ptr(),
                c.as_ptr(),
            ))
        }
    }

    pub fn phi_node<'a, I>(&self, ty: &LLVMType, incoming: I, dest: &str) -> LLVMValue
    where
        I: IntoIterator<Item = &'a (&'a LLVMValue, &'a LLVMBasicBlock)>,
    {
        let c = to_cstring(dest);
        unsafe {
            let phi = LLVMBuildPhi(self.raw_ptr(), ty.raw_ptr(), c.as_ptr());
            let (mut vals, mut blks): (Vec<_>, Vec<_>) = incoming
                .into_iter()
                .map(|&(val, blk)| (val.raw_ptr(), blk.raw_ptr()))
                .unzip();
            if !vals.is_empty() {
                LLVMAddIncoming(
                    phi,
                    vals.as_mut_ptr(),
                    blks.as_mut_ptr(),
                    vals.len() as c_uint,
                );
            }
            LLVMValue::from_ref(phi)
        }
    }

    pub fn call(&self, fun: &LLVMFunction, args: &mut [LLVMValue], name: &str) -> LLVMValue {
        let fn_ty = fun.get_function_type();
        self.call_value(&fn_ty, &fun.into_value(), args, name)
    }

    pub fn call_value(
        &self,
        fn_ty: &LLVMType,
        fun_val: &LLVMValue,
        args: &mut [LLVMValue],
        name: &str,
    ) -> LLVMValue {
        let mut raw_args: Vec<_> = args.iter().map(|arg| arg.raw_ptr()).collect();
        unsafe {
            let ret_ty = LLVMGetReturnType(fn_ty.raw_ptr());
            let c = if LLVMGetTypeKind(ret_ty) == llvm_sys::LLVMTypeKind::LLVMVoidTypeKind {
                to_cstring("")
            } else {
                to_cstring(name)
            };
            let ret = LLVMBuildCall2(
                self.raw_ptr(),
                fn_ty.raw_ptr(),
                fun_val.raw_ptr(),
                raw_args.as_mut_ptr(),
                raw_args.len() as c_uint,
                c.as_ptr(),
            );
            LLVMValue::from_ref(ret)
        }
    }

    pub fn extract_value(&self, agg: &LLVMValue, idx: usize, name: &str) -> LLVMValue {
        let c = to_cstring(name);
        unsafe {
            LLVMValue::from_ref(LLVMBuildExtractValue(
                self.raw_ptr(),
                agg.raw_ptr(),
                idx as c_uint,
                c.as_ptr(),
            ))
        }
    }

    pub fn insert_value(
        &self,
        agg: &LLVMValue,
        val: &LLVMValue,
        idx: usize,
        name: &str,
    ) -> LLVMValue {
        let c = to_cstring(name);
        unsafe {
            LLVMValue::from_ref(LLVMBuildInsertValue(
                self.raw_ptr(),
                agg.raw_ptr(),
                val.raw_ptr(),
                idx as c_uint,
                c.as_ptr(),
            ))
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

impl Drop for LLVMBuilder {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeBuilder(self.0);
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
