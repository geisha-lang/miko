
pub type Name = String;
pub type P<T> = Box<T>;
/// Shorter alias of Box
pub fn P<T>(t: T) -> Box<T> {
    Box::new(t)
}

