#![feature(libc)]
extern crate llvm_sys;
extern crate libc;

pub mod wrapper;

pub use wrapper::*;
pub use llvm_sys::analysis::LLVMVerifierFailureAction;