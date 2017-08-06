
use std::collections::HashMap;

use utils::*;
use types::*;



pub type TypeEnv<'a> = SymTable<'a, String, Scheme>;

