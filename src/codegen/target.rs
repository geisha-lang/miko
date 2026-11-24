use llvm_sys::target::*;
use llvm_sys::target_machine::*;
use llvm_sys::transforms::pass_builder::*;

use super::llvm::*;
use libc::c_char;
use std::ffi::CStr;
use std::ptr;

pub fn init_llvm_targets() {
    unsafe {
        LLVM_InitializeAllTargetInfos();
        LLVM_InitializeAllTargets();
        LLVM_InitializeAllTargetMCs();
        LLVM_InitializeAllAsmParsers();
        LLVM_InitializeAllAsmPrinters();
    }
}

unsafe fn create_host_target_machine(modu: &LLVMModule) -> Result<LLVMTargetMachineRef, String> {
    init_llvm_targets();
    let target_triple = LLVMGetDefaultTargetTriple();
    LLVMSetTarget(modu.raw_ptr(), target_triple);

    let mut target: LLVMTargetRef = ptr::null_mut();
    let mut err_msg: *mut c_char = ptr::null_mut();
    if LLVMGetTargetFromTriple(target_triple, &mut target, &mut err_msg) != 0 {
        let msg = if err_msg.is_null() {
            "Unknown LLVM target error".to_string()
        } else {
            let s = CStr::from_ptr(err_msg).to_string_lossy().into_owned();
            LLVMDisposeMessage(err_msg);
            s
        };
        LLVMDisposeMessage(target_triple);
        return Err(msg);
    }

    let cpu = to_cstring("generic");
    let features = to_cstring("");
    let tm = LLVMCreateTargetMachine(
        target,
        target_triple,
        cpu.as_ptr(),
        features.as_ptr(),
        LLVMCodeGenOptLevel::LLVMCodeGenLevelDefault,
        LLVMRelocMode::LLVMRelocPIC,
        LLVMCodeModel::LLVMCodeModelDefault,
    );
    LLVMDisposeMessage(target_triple);

    if tm.is_null() {
        return Err("Failed to create LLVMTargetMachine".to_string());
    }

    let td = LLVMCreateTargetDataLayout(tm);
    LLVMSetModuleDataLayout(modu.raw_ptr(), td);
    LLVMDisposeTargetData(td);
    Ok(tm)
}

pub fn configure_module_target(modu: &LLVMModule) {
    unsafe {
        if let Ok(tm) = create_host_target_machine(modu) {
            LLVMDisposeTargetMachine(tm);
        }
    }
}

/// Run LLVM 20 New PassManager optimizations (`mem2reg`, `instcombine`, `gvn`, `simplifycfg`, `tailcallelim`).
pub fn optimize_module(modu: &LLVMModule) -> Result<(), String> {
    unsafe {
        let tm = create_host_target_machine(modu)?;
        let opts = LLVMCreatePassBuilderOptions();
        let passes = to_cstring(
            "function(mem2reg,instcombine,reassociate,gvn,simplifycfg,tailcallelim)",
        );
        let err = LLVMRunPasses(modu.raw_ptr(), passes.as_ptr(), tm, opts);
        LLVMDisposePassBuilderOptions(opts);
        LLVMDisposeTargetMachine(tm);

        if !err.is_null() {
            let msg_ptr = llvm_sys::error::LLVMGetErrorMessage(err);
            let msg = CStr::from_ptr(msg_ptr).to_string_lossy().into_owned();
            llvm_sys::error::LLVMDisposeErrorMessage(msg_ptr);
            return Err(format!("LLVM optimization pass error: {}", msg));
        }
        Ok(())
    }
}

pub fn module_emit_file(file_name: &str, modu: &mut LLVMModule) -> Result<(), String> {
    optimize_module(modu)?;
    unsafe {
        let tm = create_host_target_machine(modu)?;
        let mut c_filename: Vec<c_char> = file_name
            .bytes()
            .map(|b| b as c_char)
            .chain(std::iter::once(0))
            .collect();
        let mut err_msg: *mut c_char = ptr::null_mut();
        let status = LLVMTargetMachineEmitToFile(
            tm,
            modu.raw_ptr(),
            c_filename.as_mut_ptr(),
            LLVMCodeGenFileType::LLVMObjectFile,
            &mut err_msg,
        );
        LLVMDisposeTargetMachine(tm);

        if status != 0 {
            let err = if err_msg.is_null() {
                "LLVMTargetMachineEmitToFile failed".to_string()
            } else {
                let s = CStr::from_ptr(err_msg).to_string_lossy().into_owned();
                LLVMDisposeMessage(err_msg);
                s
            };
            Err(err)
        } else {
            Ok(())
        }
    }
}
