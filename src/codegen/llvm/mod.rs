mod wrapper;

use std::char;
use std::ffi::{CString, NulError};
use std::io::{self, BufReader, Read, Write};
use std::ptr;
use std::collections::HashMap;
use std::iter::*;

use std::ops::Deref;

use core::*;
use internal::*;
use types::*;
use utils::*;

pub use codegen::llvm::wrapper::*;


#[derive(Debug, Clone)]
pub struct LLVMCodegen {
    pub module: LLVMModule,
    pub builder: LLVMBuilder,
    pub context: LLVMContext,
    unique: usize,
}


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

impl LLVMCodegen {
    pub fn new(name: &str) -> LLVMCodegen {
        let context = LLVMContext::new();
        let module = LLVMModule::in_ctx(name, &context);
        let builder = LLVMBuilder::in_ctx(&context);
        LLVMCodegen {
            module,
            context,
            builder,
            unique: 0,
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

    pub fn get_closure_type(&self) -> LLVMType {
        // closure = [i8*, i8*]
        let mut mem = vec![self.context.get_int8_type().get_ptr(0),
                           self.context.get_int8_type().get_ptr(0)];
        self.context.get_struct_type(&mem, false)
    }

    pub fn get_actual_cls_type(&self, fv_ty: &Vec<LLVMType>) -> LLVMType {
        let mut mem = vec![self.context.get_int8_type().get_ptr(0),
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
                let cls_type = self.get_closure_type().get_ptr(0);
                let retty = self.get_llvm_type_or_ptr(ret);
                let psty = p.prod_to_vec();
                let mut llvm_psty: Vec<_> =
                    psty.into_iter().map(|t| self.get_llvm_type_or_ptr(t)).collect();
                llvm_psty.push(cls_type);
                LLVMContext::get_function_type(&retty, &llvm_psty, false)
            }
            &Void => self.context.get_void_type(),
            &Prod(..) => {
                let tys: Vec<_> = ty.prod_to_vec()
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

    pub fn create_entry_block_alloca(&self,
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
