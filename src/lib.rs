#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(non_snake_case)]
#![allow(unreachable_code)]
#![recursion_limit = "200"]

#![feature(placement_new_protocol)]
#![feature(libc)]

#[macro_use]
extern crate pest;
extern crate llvm_sys;
extern crate libc;

pub mod utils;

pub mod syntax;
pub mod parser;
pub mod typeinfer;
pub mod codegen;
pub mod types;
// pub mod core;
