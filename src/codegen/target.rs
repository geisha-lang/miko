use llvm_sys::target_machine::*;
use llvm_sys::target::*;
use super::llvm::*;

use std::ptr;
use libc::{ c_char };

use std::ffi::{CString, NulError};

fn alloc<T>(d: T) -> *mut T {
    Box::into_raw(Box::new(d))
}

pub fn module_emit_file(file_name: &str, modu: &mut LLVMModule) -> Result<(), String> {
    unsafe {
        LLVM_InitializeAllTargetInfos();
        LLVM_InitializeAllTargets();
        LLVM_InitializeAllTargetMCs();
        LLVM_InitializeAllAsmParsers();
        LLVM_InitializeAllAsmPrinters();
        let target_triple = LLVMGetDefaultTargetTriple();
        LLVMSetTarget(modu.raw_ptr(), target_triple);

        let target: *mut LLVMTargetRef = alloc(ptr::null_mut());
        let err_msg: *mut *mut c_char = alloc(ptr::null_mut());
        if LLVMGetTargetFromTriple(target_triple, target, err_msg) != 0 {
            let err = CString::from_raw(*err_msg.as_ref().unwrap());
            eprintln!("Get LLVM target failed");
            eprintln!("{}", err.to_str().unwrap());
            panic!();
        }
        LLVMDisposeMessage(*err_msg.as_ref().unwrap());

        let target_machine = LLVMCreateTargetMachine(
            *target.as_ref().unwrap(),
            target_triple,
            raw_string("generic"),
            raw_string(""),
            LLVMCodeGenOptLevel::LLVMCodeGenLevelDefault,
            LLVMRelocMode::LLVMRelocDefault,
            LLVMCodeModel::LLVMCodeModelDefault
        );
        let success = LLVMTargetMachineEmitToFile(
            target_machine,
            modu.raw_ptr(),
            raw_string(file_name),
            LLVMCodeGenFileType::LLVMObjectFile,
            err_msg
        );
        if success != 0 {
            let err = CString::from_raw(*err_msg.as_ref().unwrap()).into_string().unwrap();
            Err(err)
        } else {
            Ok(())
        }
    }
}

