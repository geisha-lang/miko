// Optional value container (`std.option`)

pub data Option a {
    None,
    Some(a),
}

pub def isSome(opt: Option a): Bool = match opt {
    Some(_) -> true,
    None -> false,
}

pub def isNone(opt: Option a): Bool = match opt {
    None -> true,
    Some(_) -> false,
}

pub def unwrapOr(opt: Option a, fallback: a): a = match opt {
    Some(v) -> v,
    None -> fallback,
}

pub def mapOption(opt: Option a, f: a -> b): Option b = match opt {
    Some(v) -> Some(f(v)),
    None -> None,
}

pub def andThenOption(opt: Option a, f: a -> Option b): Option b = match opt {
    Some(v) -> f(v),
    None -> None,
}
