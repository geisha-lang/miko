#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(non_snake_case)]
#![allow(unreachable_code)]
#![allow(unused_variables)]
#![recursion_limit = "200"]

extern crate libllvm;

extern crate llvm_sys;

extern crate bytecount;
extern crate libc;

pub mod utils;

pub mod syntax;
pub mod typeinfer;
pub mod codegen;
pub mod types;
pub mod core;
pub mod internal;
pub mod modules;
