// Fallible computation result (`std.result`)

pub data Result a e {
    Ok(a),
    Err(e),
}

pub def isOk(res: Result a e): Bool = match res {
    Ok(_) -> true,
    Err(_) -> false,
}

pub def isErr(res: Result a e): Bool = match res {
    Err(_) -> true,
    Ok(_) -> false,
}

pub def unwrapResultOr(res: Result a e, fallback: a): a = match res {
    Ok(v) -> v,
    Err(_) -> fallback,
}

pub def mapResult(res: Result a e, f: a -> b): Result b e = match res {
    Ok(v) -> Ok(f(v)),
    Err(err) -> Err(err),
}

pub def mapErr(res: Result a e, f: e -> e2): Result a e2 = match res {
    Ok(v) -> Ok(v),
    Err(err) -> Err(f(err)),
}

pub def andThenResult(res: Result a e, f: a -> Result b e): Result b e = match res {
    Ok(v) -> f(v),
    Err(err) -> Err(err),
}
