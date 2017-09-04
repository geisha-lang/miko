#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(non_snake_case)]
#![allow(unreachable_code)]
#![allow(unused_variables)]
#![recursion_limit = "200"]

#![feature(placement_new_protocol)]
#![feature(libc)]
#![feature(box_syntax, box_patterns)]
 
extern crate bytecount;
extern crate llvm_sys;
extern crate libc;

pub mod utils;

pub mod syntax;
pub mod typeinfer;
pub mod codegen;
pub mod types;
pub mod core;
pub mod internal;
