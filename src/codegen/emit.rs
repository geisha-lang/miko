
use syntax::form::*;

pub trait ModuleProvider {
    fn gen_module<T>(&mut self, modu: T)
        where T: IntoIterator<Item=Def>;
}
