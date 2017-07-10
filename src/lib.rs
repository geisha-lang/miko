#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(non_snake_case)]
#![allow(unreachable_code)]
#![recursion_limit = "200"]

 #![feature(placement_new_protocol)]
// #[macro_use]
// extern crate nom;
// extern crate regex;
// #[macro_use]
// extern crate lazy_static;
// #[macro_use(position)]
// extern crate nom_locate;


#[macro_use]
extern crate pest;
// #[macro_use]
// extern crate pest_derive;

pub mod utils;

pub mod syntax;
pub mod parser;
pub mod typeinfer;
