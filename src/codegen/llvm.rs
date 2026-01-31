use std::char;
use std::ffi::{CString, NulError};
use std::io::{self, BufReader, Read, Write};
use std::ptr;
use std::collections::HashMap;
use std::iter::*;

use std::ops::Deref;

use crate::core::*;
use crate::internal::*;
use crate::types::*;
use crate::utils::*;

pub use libllvm::*;


#[derive(Debug, Clone)]
pub struct LLVMCodegen {
    pub module: LLVMModule,
    pub builder: LLVMBuilder,
    pub context: LLVMContext,
    pub passer: LLVMFunctionPassManager,
    unique: usize,
    pub adt_registry: AdtRegistry,
}

type LLVMOpBuilder<'a> = dyn Fn(LLVMBuilderRef,
                            LLVMValueRef,
                            LLVMValueRef,
                            *const ::libc::c_char)
                            -> LLVMValueRef + 'a;
pub fn get_llvm_op<'a>(op: BinOp, operand_ty: &'a Type) -> Box<LLVMOpBuilder<'a>> {
    use self::BinOp::*;
    use self::Type::*;

    // Get type name, defaulting to "Int" for type variables (polymorphic contexts)
    let type_name = match operand_ty {
        Con(ty_name) => ty_name.as_str(),
        Var(_) => "Int", // Default to Int for unresolved type variables
        _ => "Int",      // Default to Int for other cases
    };

    Box::new(move |builder, lhs, rhs, dest|
        unsafe {
            match (op, type_name) {
                (Add, "Int") => LLVMBuildAdd(builder, lhs, rhs, dest),
                (Add, "Float") => LLVMBuildFAdd(builder, lhs, rhs, dest),
                (Sub, "Int") => LLVMBuildSub(builder, lhs, rhs, dest),
                (Sub, "Float") => LLVMBuildFSub(builder, lhs, rhs, dest),
                (Mul, "Int") => LLVMBuildMul(builder, lhs, rhs, dest),
                (Mul, "Float") => LLVMBuildFMul(builder, lhs, rhs, dest),
                (Eq, "Int") => LLVMBuildICmp(builder, LLVMIntPredicate::LLVMIntEQ, lhs, rhs, dest),
                (Lt, "Int") => LLVMBuildICmp(builder, LLVMIntPredicate::LLVMIntSLT, lhs, rhs, dest),
                (Le, "Int") => LLVMBuildICmp(builder, LLVMIntPredicate::LLVMIntSLE, lhs, rhs, dest),
                (Gt, "Int") => LLVMBuildICmp(builder, LLVMIntPredicate::LLVMIntSGT, lhs, rhs, dest),
                (Ge, "Int") => LLVMBuildICmp(builder, LLVMIntPredicate::LLVMIntSGE, lhs, rhs, dest),
                (Eq, "Double") => LLVMBuildFCmp(builder, LLVMRealPredicate::LLVMRealOEQ, lhs, rhs, dest),
                (Lt, "Double") => LLVMBuildFCmp(builder, LLVMRealPredicate::LLVMRealOLT, lhs, rhs, dest),
                (Le, "Double") => LLVMBuildFCmp(builder, LLVMRealPredicate::LLVMRealOLE, lhs, rhs, dest),
                (Gt, "Double") => LLVMBuildFCmp(builder, LLVMRealPredicate::LLVMRealOGT, lhs, rhs, dest),
                (Ge, "Double") => LLVMBuildFCmp(builder, LLVMRealPredicate::LLVMRealOGE, lhs, rhs, dest),
                (And, "Bool") => LLVMBuildAnd(builder, lhs, rhs, dest),
                (Or, "Bool") => LLVMBuildOr(builder, lhs, rhs, dest),
                (Div, _) => LLVMBuildFDiv(builder, lhs, rhs, dest),
                (Rem, _) => LLVMBuildURem(builder, lhs, rhs, dest),
                // Default to Int operations for type variables
                (Add, _) => LLVMBuildAdd(builder, lhs, rhs, dest),
                (Sub, _) => LLVMBuildSub(builder, lhs, rhs, dest),
                (Mul, _) => LLVMBuildMul(builder, lhs, rhs, dest),
                (Eq, _) => LLVMBuildICmp(builder, LLVMIntPredicate::LLVMIntEQ, lhs, rhs, dest),
                (Lt, _) => LLVMBuildICmp(builder, LLVMIntPredicate::LLVMIntSLT, lhs, rhs, dest),
                (Le, _) => LLVMBuildICmp(builder, LLVMIntPredicate::LLVMIntSLE, lhs, rhs, dest),
                (Gt, _) => LLVMBuildICmp(builder, LLVMIntPredicate::LLVMIntSGT, lhs, rhs, dest),
                (Ge, _) => LLVMBuildICmp(builder, LLVMIntPredicate::LLVMIntSGE, lhs, rhs, dest),
                (And, _) => LLVMBuildAnd(builder, lhs, rhs, dest),
                (Or, _) => LLVMBuildOr(builder, lhs, rhs, dest),
                _ => unimplemented!("Binary operation {:?} not implemented for type {}", op, type_name),
            }
        })
}

pub fn is_primitive_type(t: &Type) -> bool {
    match t {
        Type::Con(n) => {
            match n.as_str() {
                "Int" | "Float" | "Char" | "String" | "Void" | "Bool" => true,
                _ => false,
            }
        }
        // Type variables are treated as primitive (i64) values
        Type::Var(_) => true,
        _ => false,
    }
}

/// Check if a type is an ADT (user-defined algebraic data type)
pub fn is_adt_type(t: &Type) -> bool {
    match t {
        Type::Con(n) => {
            match n.as_str() {
                "Int" | "Float" | "Char" | "String" | "Void" | "Bool" => false,
                _ => true,
            }
        }
        Type::Comp(base, _) => {
            // Parametric ADT like List a
            if let Type::Con(n) = base.as_ref() {
                match n.as_str() {
                    "Int" | "Float" | "Char" | "String" | "Void" | "Bool" => false,
                    _ => true,
                }
            } else {
                false
            }
        }
        _ => false,
    }
}

impl LLVMCodegen {
    pub fn new(name: &str, adt_registry: AdtRegistry) -> LLVMCodegen {
        let context = LLVMContext::new();
        let module = LLVMModule::in_ctx(name, &context);
        let builder = LLVMBuilder::in_ctx(&context);
        let passer = LLVMFunctionPassManager::init_for_module(&module);
        LLVMCodegen {
            module,
            context,
            builder,
            passer,
            unique: 0,
            adt_registry,
        }
    }


    pub fn new_symbol(&mut self) -> Result<CString, NulError> {
        let f = String::from("tmp") + self.unique.to_string().as_str();
        self.unique = self.unique + 1;
        CString::new(f)
    }

    pub fn new_symbol_string(&mut self) -> String {
        let f = String::from("tmp") + self.unique.to_string().as_str();
        self.unique = self.unique + 1;
        f
    }

    pub fn get_closure_type(&self) -> LLVMType {
        // closure = [i8*, i8*]
        let mem = vec![self.context.get_int8_type().get_ptr(0),
                       self.context.get_int8_type().get_ptr(0)];
        self.context.get_struct_type(&mem, false)
    }

    pub fn get_actual_cls_type(&self, fv_ty: &Vec<LLVMType>) -> LLVMType {
        let mem = vec![self.context.get_int8_type().get_ptr(0),
                       self.context.get_struct_type(&fv_ty, false)];
        self.context.get_struct_type(&mem, false)
    }

    /// Get a pointer type if the type is not primitive
    pub fn get_llvm_type_or_ptr(&self, ty: &Type) -> LLVMType {
        let ret = self.get_llvm_type(ty);
        if !is_primitive_type(ty) {
            ret.get_ptr(0)
        } else {
            ret
        }
    }

    pub fn get_main_type(&self) -> LLVMType {
        let retty = self.context.get_int32_type();
        LLVMContext::get_function_type(&retty, &vec![], false)
    }

    pub fn get_llvm_type(&self, ty: &Type) -> LLVMType {
        use self::Type::*;
        match ty {
            Con(n) => {
                match n.as_str() {

                    // Primary types
                    "Int" => self.context.get_int32_type(),
                    "Float" => self.context.get_double_type(),
                    "Char" => self.context.get_int8_type(),
                    "Bool" => self.context.get_int1_type(),
                    "String" => self.context.get_int8_type().get_ptr(0),
                    "Void" => self.context.get_void_type(),

                    // User defined types
                    t => self.gen_user_type(t),
                }
            }
            Arr(p, ret) => {
                // Type of parameters and returned value should be pointer if not primitive
                let fvs_ty = self.context.get_int8_type().get_ptr(0);
                let retty = if let Type::Arr(..) = ret.as_ref() {
                    self.get_closure_type().get_ptr(0)
                } else {
                    self.get_llvm_type_or_ptr(ret)
                };
                let psty = match p.as_ref() {
                    Type::Void => vec![],
                    _ => p.prod_to_vec()
                };
                let mut llvm_psty: Vec<_> =
                    psty.into_iter().map(|t| self.get_llvm_type_or_ptr(t)).collect();
                llvm_psty.push(fvs_ty);
                LLVMContext::get_function_type(&retty, &llvm_psty, false)
            }
            Void => self.context.get_void_type(),
            Prod(..) => {
                let tys: Vec<_> = ty.prod_to_vec()
                    .iter()
                    .map(|t| self.get_llvm_type(t))
                    .collect();
                self.context.get_struct_type(&tys, true)
            }
            Comp(base, _param) => {
                if let Type::Con(name) = base.as_ref() {
                    self.gen_user_type(name)
                } else {
                    self.get_llvm_type(base)
                }
            }

            // Unresolved type variable - use i64 as universal boxed representation
            // This can hold both pointers (via inttoptr) and primitives (via zext/sext)
            Var(..) => self.context.get_int64_type(),
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
            &Str(ref s) => self.context.get_const_string(s.as_str()),
        }
    }

    pub fn gen_user_type(&self, tyname: &str) -> LLVMType {
        if let Some(adt_info) = self.adt_registry.get(tyname) {
            self.get_adt_llvm_type(adt_info)
        } else {
            panic!("Unknown user type: {}", tyname)
        }
    }

    fn get_adt_llvm_type(&self, adt_info: &AdtInfo) -> LLVMType {
        let tag_ty = self.context.get_int32_type();

        // Find variant with most fields for payload sizing
        let max_fields = adt_info.variants.iter()
            .filter(|v| !v.field_types.is_empty())
            .max_by_key(|v| v.field_types.len());

        match max_fields {
            Some(variant) => {
                let payload_ty = self.get_payload_llvm_type(&variant.field_types);
                self.context.get_struct_type(&vec![tag_ty, payload_ty], false)
            }
            None => {
                // All unit variants
                self.context.get_struct_type(&vec![tag_ty], false)
            }
        }
    }

    fn get_payload_llvm_type(&self, field_types: &[Type]) -> LLVMType {
        let llvm_types: Vec<LLVMType> = field_types.iter()
            .map(|t| self.get_field_llvm_type(t))
            .collect();
        self.context.get_struct_type(&llvm_types, false)
    }

    fn get_field_llvm_type(&self, ty: &Type) -> LLVMType {
        match ty {
            Type::Con(name) => match name.as_str() {
                "Int" => self.context.get_int32_type(),
                "Float" => self.context.get_double_type(),
                "Char" => self.context.get_int8_type(),
                "Bool" => self.context.get_int1_type(),
                "String" => self.context.get_int8_type().get_ptr(0),
                "Void" => self.context.get_void_type(),
                // Other ADTs: use opaque pointer to avoid infinite recursion
                // ADT values are always stored/passed as pointers
                _ => self.context.get_int8_type().get_ptr(0),
            },
            Type::Arr(..) => self.get_closure_type().get_ptr(0),
            Type::Comp(_base, _) => {
                // Polymorphic type like List a - use opaque pointer
                // ADT values are always stored/passed as pointers
                self.context.get_int8_type().get_ptr(0)
            }
            // Type variable: use i64 as a universal representation
            // that can hold both pointers (via inttoptr) and primitives (via zext/sext)
            Type::Var(_) => self.context.get_int64_type(),
            Type::Prod(..) => {
                let tys: Vec<_> = ty.prod_to_vec().iter()
                    .map(|t| self.get_field_llvm_type(t))
                    .collect();
                self.context.get_struct_type(&tys, false)
            }
            Type::Void => self.context.get_void_type(),
        }
    }

    /// Get the i64 type for storing polymorphic values
    pub fn get_int64_type(&self) -> LLVMType {
        self.context.get_int64_type()
    }

    /// Get the LLVM type for an ADT (Algebraic Data Type)
    /// ADTs are represented as a tagged union: { i32 tag, payload }
    /// where payload is the largest variant's fields or an opaque byte array
    pub fn get_adt_type(&self, max_payload_size: usize) -> LLVMType {
        // For now, use a simple representation: { i32 tag, [max_payload_size x i8] }
        // This is a simple approach; a more sophisticated one would compute exact union sizes
        let tag_ty = self.context.get_int32_type();
        if max_payload_size == 0 {
            // Unit-only ADT
            self.context.get_struct_type(&vec![tag_ty], false)
        } else {
            // ADT with payload - use an array of bytes as a generic payload
            let payload_ty = unsafe {
                LLVMType::from_ref(llvm_sys::core::LLVMArrayType(
                    self.context.get_int8_type().raw_ptr(),
                    max_payload_size as u32
                ))
            };
            self.context.get_struct_type(&vec![tag_ty, payload_ty], false)
        }
    }

    /// Create a struct type for a specific ADT variant's payload
    pub fn get_variant_payload_type(&self, field_types: &[&Type]) -> LLVMType {
        if field_types.is_empty() {
            // No payload - return void type
            self.context.get_void_type()
        } else {
            let llvm_types: Vec<LLVMType> = field_types.iter()
                .map(|t| self.get_llvm_type(t))
                .collect();
            self.context.get_struct_type(&llvm_types, false)
        }
    }

    pub fn get_or_add_function(&self, fun_name: &str, fty: &Type) -> LLVMFunction {
        match self.module.get_function(fun_name) {
            Some(f) => f,
            None => {
                let ty = self.get_llvm_type(fty);
                self.module.add_function(fun_name, &ty)
            }
        }
    }


    pub fn create_entry_block_alloca(&self,
                                     fun: &LLVMFunction,
                                     var_name: &str,
                                     ty: &Type)
                                     -> LLVMValue {
        let builder = LLVMBuilder::in_ctx(&self.context);
        let block = fun.get_entry_basic_block();
        let fi = block.get_first_instr();
        let llvm_ty = match ty {
            Type::Arr(..) => self.get_closure_type().get_ptr(0),
            _ if is_adt_type(ty) => self.get_llvm_type(ty).get_ptr(0),
            _ => self.get_llvm_type(ty)
        };
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
        LLVMValue::from_ref(fun(self.builder.raw_ptr(),
                                lhs.raw_ptr(),
                                rhs.raw_ptr(),
                                self.new_symbol().unwrap().into_raw()))
    }
}
