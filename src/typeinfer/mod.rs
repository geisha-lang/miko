mod subst;
mod typeenv;
mod infer;

use syntax::*;
use self::subst::*;
use self::infer::*;
use self::typeenv::*;

// pub fn type_check<'b, 'a: 'b, T>(_env: &'a TypeEnv<'a>, program: &'a mut Vec<Def>) -> TypeEnv<'b> {
//     // Add top level definition names into environment.
//     // Types are set to temporaries.
//     let name_types: Vec<_> = program.iter()
//         .filter(|ref v| v.is_value())
//         .map(|ref v| (v.ident.as_str(), v.value_type().unwrap()))
//         .collect();

//     let mut env = _env.extend_n(name_types);
    
// }
