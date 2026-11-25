use std::cell::RefCell;
use std::collections::HashSet;
use crate::internal::*;
use crate::syntax::form::*;
use crate::utils::*;
use crate::types::*;

fn unescape_char(n: char) -> Result<char, &'static str> {
    match n {
        'n' => Ok('\n'),
        'r' => Ok('\r'),
        't' => Ok('\t'),
        '0' => Ok('\0'),
        '\\' => Ok('\\'),
        '"' => Ok('"'),
        '\'' => Ok('\''),
        _ => Err("Invalid escape sequence"),
    }
}

fn escape_string(s: &str) -> Result<String, &'static str> {
    let mut cs = s.chars();
    let mut r = String::new();
    while let Some(ch) = cs.next() {
        match ch {
            '\\' => {
                if let Some(n) = cs.next() {
                    r.push(unescape_char(n)?);
                } else {
                    return Err("Unexpected end of string");
                }
            }
            _ => r.push(ch),
        }
    }
    Ok(r)
}

fn parse_char_content(s: &str) -> Result<u8, &'static str> {
    let mut cs = s.chars();
    let c = match cs.next() {
        Some('\\') => {
            let esc = cs.next().ok_or("Empty escape in char literal")?;
            unescape_char(esc)?
        }
        Some(ch) => ch,
        None => return Err("Empty char literal"),
    };
    if cs.next().is_some() {
        return Err("Char literal must contain a single character");
    }
    if (c as u32) > 255 {
        return Err("Char literal out of 8-bit range");
    }
    Ok(c as u8)
}

fn make_spanned_binexpr(op: BinOp, lhs: Form, rhs: Form) -> Form {
    let span = lhs.tag.pos.union(&rhs.tag.pos);
    Form::new(span, Expr::Binary(op, Box::new(lhs), Box::new(rhs)))
}

fn make_spanned_unexpr(start: usize, op: UnOp, rhs: Form) -> Form {
    let span = Span::new(start, rhs.tag.pos.end);
    Form::new(span, Expr::Unary(op, Box::new(rhs)))
}

/// Encode an operator symbol into a valid, injective ASCII identifier (`$op$...`).
pub fn encode_op_name(op: &str) -> String {
    let mut parts = Vec::with_capacity(op.len());
    for ch in op.chars() {
        let tok = match ch {
            '+' => "plus",
            '-' => "minus",
            '*' => "star",
            '/' => "slash",
            '%' => "rem",
            '=' => "eq",
            '!' => "bang",
            '<' => "lt",
            '>' => "gt",
            '&' => "amp",
            '|' => "bar",
            '^' => "caret",
            ':' => "colon",
            '~' => "tilde",
            '?' => "qmark",
            '@' => "at",
            '#' => "hash",
            '$' => "dollar",
            '\\' => "bslash",
            _ => "sym",
        };
        parts.push(tok);
    }
    format!("$op${}", parts.join("_"))
}

fn is_reserved_non_op(s: &str) -> bool {
    matches!(s, "=" | ":" | "->" | "=>" | "&&" | "||" | "!" | "~" | "?")
}

fn builtin_binop_from_str(s: &str) -> Option<BinOp> {
    match s {
        "+" => Some(BinOp::Add),
        "-" => Some(BinOp::Sub),
        "*" => Some(BinOp::Mul),
        "/" => Some(BinOp::Div),
        "%" => Some(BinOp::Rem),
        "&" => Some(BinOp::BitAnd),
        "|" => Some(BinOp::BitOr),
        "^" => Some(BinOp::BitXor),
        "<<" => Some(BinOp::Shl),
        ">>" => Some(BinOp::Shr),
        ">>>" => Some(BinOp::Ushr),
        "==" => Some(BinOp::Eq),
        "!=" => Some(BinOp::Ne),
        "<" => Some(BinOp::Lt),
        "<=" => Some(BinOp::Le),
        ">" => Some(BinOp::Gt),
        ">=" => Some(BinOp::Ge),
        _ => None,
    }
}

/// Refined Scala / Swift / Rust precedence bucket (1 = lowest, 11 = highest custom infix):
///   1: multi-char `|`-leading pipelines (e.g. `|>`, below `||` and `&&`)
///      [short-circuit `||` then `&&` sit between 1 and 2]
///   2: `=` / `!`-leading equality & inequality (`==`, `!=`, `===`, `=/=`)
///   3: `<` / `>`-leading comparisons & combinators except shifts (`<`, `<=`, `>`, `>=`, `<+>`, `<|>`, `>>=`)
///   4: `:`-leading cons/pair operators (`::` is right-associative)
///   5: single `|` bitwise OR
///   6: `^`-leading bitwise XOR (`^`, `^^`)
///   7: `&`-leading bitwise AND (`&`, `&+`, excluding `&&`)
///   8: bitwise shifts (`<<`, `>>`, `>>>`)
///   9: `+` / `-`-leading additive operators (`+`, `-`, `++`, `+:`)
///  10: `*` / `/` / `%`-leading multiplicative operators (`*`, `/`, `%`, `**`)
///  11: other symbol chars (`@`, `#`, `$`, `?`, multi-char `~`, `\`)
fn op_precedence_bucket(op: &str) -> Option<u8> {
    if is_reserved_non_op(op) {
        return None;
    }
    match op {
        "|" => return Some(5),
        "<<" | ">>" | ">>>" => return Some(8),
        _ => {}
    }
    let first = op.chars().next()?;
    let b = match first {
        '|' => 1,
        '=' | '!' => 2,
        '<' | '>' => 3,
        ':' => 4,
        '^' => 6,
        '&' => 7,
        '+' | '-' => 9,
        '*' | '/' | '%' => 10,
        '@' | '#' | '$' | '?' | '~' | '\\' => 11,
        _ => return None,
    };
    Some(b)
}

/// Scala right-associativity rule: operators ending in `:` (e.g. `::`, `+:`, `~:`) are right-associative!
fn is_right_assoc_op(op: &str) -> bool {
    op.ends_with(':')
}

fn make_infix_expr(op_str: &str, lhs: Form, rhs: Form, interner: &RefCell<Interner>) -> Form {
    if let Some(bop) = builtin_binop_from_str(op_str) {
        make_spanned_binexpr(bop, lhs, rhs)
    } else {
        let span = lhs.tag.pos.union(&rhs.tag.pos);
        let op_id = interner.borrow_mut().intern(&encode_op_name(op_str));
        let callee = Form::new(span, Expr::Var(op_id));
        Form::new(
            span,
            Expr::Apply(Box::new(callee), vec![Box::new(lhs), Box::new(rhs)]),
        )
    }
}

enum PostfixOp {
    Call(usize, Vec<Form>),
    Field(usize, String),
    Try(usize),
}

enum BlockItem {
    LetVar(Span, VarDecl, E),
    LetPat(Span, Pattern, E),
    Expr(E),
}

fn fold_block_items(items: Vec<BlockItem>) -> Result<Expr, &'static str> {
    let mut it = items.into_iter().rev();
    let tail_expr = match it.next() {
        Some(BlockItem::Expr(e)) => e,
        _ => return Err("Last item in a block must be an expression"),
    };

    let mut current_tail: Vec<E> = vec![tail_expr];

    for item in it {
        match item {
            BlockItem::Expr(e) => {
                current_tail.push(e);
            }
            BlockItem::LetVar(span, v, val) => {
                current_tail.reverse();
                let body_form = if current_tail.len() == 1 {
                    *current_tail.pop().unwrap()
                } else {
                    let bspan = current_tail[0]
                        .tag
                        .pos
                        .union(&current_tail.last().unwrap().tag.pos);
                    Form::new(bspan, Expr::Block(current_tail))
                };
                let full_span = span.union(&body_form.tag.pos);
                let let_form = Form::new(full_span, Expr::Let(v, val, Box::new(body_form)));
                current_tail = vec![Box::new(let_form)];
            }
            BlockItem::LetPat(span, pat, val) => {
                current_tail.reverse();
                let body_form = if current_tail.len() == 1 {
                    *current_tail.pop().unwrap()
                } else {
                    let bspan = current_tail[0]
                        .tag
                        .pos
                        .union(&current_tail.last().unwrap().tag.pos);
                    Form::new(bspan, Expr::Block(current_tail))
                };
                let full_span = span.union(&body_form.tag.pos);
                let arm = MatchArm {
                    pattern: pat,
                    guard: None,
                    body: Box::new(body_form),
                };
                let match_form = Form::new(full_span, Expr::Match(val, vec![arm]));
                current_tail = vec![Box::new(match_form)];
            }
        }
    }

    current_tail.reverse();
    Ok(Expr::Block(current_tail))
}

fn build_constrained_scheme(
    explicit_binders: Vec<String>,
    cs: Vec<TypeConstraint>,
    body: Type,
) -> Scheme {
    let mut seen = HashSet::new();
    let mut binders = Vec::new();
    for b in explicit_binders {
        if seen.insert(b.clone()) {
            binders.push(b);
        }
    }
    for c in &cs {
        if seen.insert(c.type_var.clone()) {
            binders.push(c.type_var.clone());
        }
    }
    for fv in body.ftv_ordered() {
        if seen.insert(fv.clone()) {
            binders.push(fv);
        }
    }
    let constraints: Vec<SchemeConstraint> = cs
        .into_iter()
        .map(|c| SchemeConstraint {
            concept: c.concept,
            type_arg: c.type_var,
        })
        .collect();
    if constraints.is_empty() {
        if binders.is_empty() {
            Scheme::Mono(body)
        } else {
            Scheme::Poly(binders, body)
        }
    } else {
        Scheme::PolyConstrained(binders, constraints, body)
    }
}

peg::parser! {
    pub grammar geisha_parser(interner: &RefCell<Interner>) for str {
        // Comments & whitespace
        rule line_comment() = "//" (!['\n'] [_])*
        rule block_comment() = "/*" (!"*/" [_])* "*/"
        rule ws_char() = [' ' | '\n' | '\r' | '\t']

        rule whitespace() = quiet!{ (ws_char() / line_comment() / block_comment())* }
        rule whitespace_inline() = quiet!{ ([' ' | '\t'] / block_comment())* line_comment()? }
        rule whitespace_inline_only() = quiet!{ ([' ' | '\t'] / block_comment())+ }

        rule keywords() = (
            "def" / "extern" / "if" / "else" / "while" / "let" / "in" / "type" / "data"
            / "match" / "concept" / "instance" / "forall"
            / "pub" / "use" / "mod" / "open" / "as"
            / "true" / "false" / "Void"
        ) !identifier_char()

        rule lexeme<T>(x: rule<T>) -> T
            = whitespace() !keywords() t:x() { t }

        rule token<T>(x: rule<T>) -> T
            = whitespace() t:x() { t }

        rule reserved<T>(x: rule<T>) -> T
            = whitespace() r:x() !identifier_char() { r }

        rule parens<T>(x: rule<T>) -> T
            = token(<"(">) t:x() token(<")">) { t }

        rule spanned<T>(x: rule<T>) -> (Span, T)
            = start:position!() p:x() end:position!() {
                (Span::new(start, end), p)
            }

        rule identifier_char()
            = ['a'..='z' | 'A'..='Z' | '0'..='9' | '_']

        rule string_content() -> &'input str
            = $((!['"' | '\\'] [_] / "\\" [_])*)

        rule char_content() -> &'input str
            = $((!['\'' | '\\'] [_] / "\\" [_])+)

        // Identifiers (plus parenthesized operator names `(+)` / `(|>)` / `(<+>)` where definitions/imports expect a name)
        rule op_char()
            = ['!' | '#' | '$' | '%' | '&' | '*' | '+' | '-' | '/' | ':' | '<' | '=' | '>' | '?' | '@' | '\\' | '^' | '|' | '~']

        rule raw_op_str() -> &'input str
            = token(<$((!("//" / "/*") op_char())+)>)

        rule definable_op_str() -> &'input str
            = s:raw_op_str() {?
                if matches!(s, "=" | ":" | "->" | "=>" | "&&" | "||" | "!" | "?") {
                    Err("Reserved syntax punctuation cannot be redefined as an operator")
                } else {
                    Ok(s)
                }
            }

        rule infix_op_left(bucket: u8) -> &'input str
            = s:raw_op_str() {?
                if op_precedence_bucket(s) == Some(bucket) && !is_right_assoc_op(s) {
                    Ok(s)
                } else {
                    Err("not left-assoc op at this precedence")
                }
            }

        rule infix_op_right(bucket: u8) -> &'input str
            = s:raw_op_str() {?
                if op_precedence_bucket(s) == Some(bucket) && is_right_assoc_op(s) {
                    Ok(s)
                } else {
                    Err("not right-assoc op at this precedence")
                }
            }

        rule identifier() -> Id
            = id:lexeme(<$(['a'..='z' | 'A'..='Z' | '_'] identifier_char()*)>) {
                interner.borrow_mut().intern(id)
            }

        rule callable_identifier() -> Id
            = token(<"(">) op:definable_op_str() token(<")">) {
                interner.borrow_mut().intern(&encode_op_name(op))
            }
            / identifier()

        rule lower_identifier_str() -> &'input str
            = lexeme(<$(['a'..='z' | '_'] identifier_char()*)>)

        rule type_constant_identifier() -> &'input str
            = whitespace() !("def" / "extern" / "if" / "else" / "while" / "let" / "in" / "type" / "data" / "match" / "concept" / "instance" / "forall" / "pub" / "use" / "mod" / "open" / "as" / "true" / "false" / "Void") !identifier_char()
              s:$(['A'..='Z'] identifier_char()*) { s }
            / whitespace() !"Void" !identifier_char() s:$(['A'..='Z'] identifier_char()*) { s }

        rule upper_identifier_str() -> &'input str
            = whitespace() s:$(['A'..='Z'] identifier_char()*) {?
                if s == "Void" {
                    Err("Void is a built-in type")
                } else {
                    Ok(s)
                }
            }

        rule type_variable_identifier() -> &'input str
            = lexeme(<$(['a'..='z'] identifier_char()*)>)

        rule integer() -> &'input str
            = token(<$("-"? ['1'..='9']['0'..='9']*)>)
            / token(<$("-"? "0")>)

        rule float() -> &'input str
            = token(<$("-"? ['0'..='9']+ "." ['0'..='9']+ (['e' | 'E'] ['+' | '-']? ['0'..='9']+)?)>)

        rule string() -> &'input str
            = token(<"\"" s:string_content() "\"" { s }>)

        rule char_lit_raw() -> &'input str
            = token(<"'" s:char_content() "'" { s }>)

        rule definition_delimite() = whitespace_inline() (";" / "\r\n" / "\n")

        pub rule module() -> Vec<Def>
            = whitespace() d:(dd:definition() definition_delimite()* { dd })* whitespace() { d }

        // ====================================================================
        // Module System Grammar
        // ====================================================================

        rule visibility() -> Visibility
            = reserved(<"pub">) { Visibility::Public }
            / { Visibility::Private }

        rule module_path() -> ModulePath
            = segments:identifier() ++ token(<".">) { ModulePath::new(segments) }

        rule module_path_for_multi() -> ModulePath
            = segments:(identifier() ++ token(<".">)) token(<".">) &token(<"{">) {
                ModulePath::new(segments)
            }

        rule use_spec() -> UseSpec
            = name:callable_identifier() reserved(<"as">) alias:callable_identifier() { UseSpec::with_alias(name, alias) }
            / name:callable_identifier() { UseSpec::new(name) }

        rule use_item_list() -> Vec<UseSpec>
            = token(<"{">) specs:use_spec() ++ token(<",">) token(<",">)? token(<"}">) { specs }

        rule use_statement() -> UseItem
            = path:module_path_for_multi() list:use_item_list() { UseItem::Multiple(path, list) }
            / path:module_path() reserved(<"as">) alias:identifier() { UseItem::Alias(path, alias) }
            / path:module_path() { UseItem::Single(path) }

        rule open_statement() -> UseItem
            = reserved(<"open">) path:module_path() { UseItem::Glob(path) }

        rule use_declaration() -> (Visibility, UseItem)
            = vis:visibility() reserved(<"use">) item:use_statement() { (vis, item) }

        rule open_declaration() -> (Visibility, UseItem)
            = vis:visibility() item:open_statement() { (vis, item) }

        rule inline_module_def() -> ModuleDef
            = start:position!() reserved(<"mod">) name:identifier()
              token(<"{">) items:module_item()* token(<"}">) end:position!() {
                ModuleDef {
                    name,
                    items,
                    pos: Span::new(start, end),
                }
            }

        rule module_declaration() -> Id
            = reserved(<"mod">) name:identifier() { name }

        rule module_item() -> ModuleItem
            = vis:visibility() d:inline_module_def() definition_delimite()* { ModuleItem::SubModule(vis, Box::new(d)) }
            / decl:use_declaration() definition_delimite()* { ModuleItem::Use(decl.0, decl.1) }
            / decl:open_declaration() definition_delimite()* { ModuleItem::Use(decl.0, decl.1) }
            / vis:visibility() name:module_declaration() definition_delimite()* { ModuleItem::ModDecl(vis, name) }
            / d:definition() definition_delimite()* { ModuleItem::Def(d.visibility, d) }

        pub rule file_module(path: &ModulePath) -> FileModule
            = whitespace() items:module_item()* whitespace() { FileModule { path: path.clone(), items } }

        // ====================================================================
        // Top-level Definitions
        // ====================================================================

        pub rule definition() -> Def
            = t:forward_declaration() definition_delimite()* d:spanned(<pub_form_definition()>) {?
                let (forward_name, scm) = t;
                let (span, (vis, name, mut form)) = d;
                if forward_name == name {
                    form.tag.annotate = Some(scm);
                    Ok(Def::value_with_visibility(span, name, Box::new(form), vis))
                } else {
                    Err("Forward declaration must be followed by its definition")
                }
            }
            / d:spanned(<pub_form_definition()>) {
                let (span, (vis, name, form)) = d;
                Def::value_with_visibility(span, name, Box::new(form), vis)
            }
            / d:spanned(<pub_extern_definition()>) {
                let (span, mut def) = d;
                def.pos = span;
                def
            }
            / d:spanned(<pub_type_definition()>) {
                let (span, (vis, mut def)) = d;
                def.pos = span;
                def.visibility = vis;
                def
            }

        rule pub_extern_definition() -> Def
            = vis:visibility() reserved(<"extern">) link_sym:string()? reserved(<"def">)
              name:identifier() token(<"(">) ps:extern_param_list() token(<")">)
              ret_ty:(token(<":">) t:type_expression() { t })? {?
                let link_name = match link_sym {
                    Some(raw) => escape_string(raw)?,
                    None => interner.borrow().trace(name).to_string(),
                };
                let ret = ret_ty.unwrap_or(Type::Void);
                let fn_ty = Type::Arr(ps, Box::new(ret));
                Ok(Def {
                    ident: name,
                    pos: Span::default(),
                    node: Item::Extern {
                        link_name,
                        ty: Scheme::Mono(fn_ty),
                    },
                    visibility: vis,
                })
            }

        rule extern_param() -> Type
            = _:identifier() token(<":">) t:type_expression() { t }
            / t:type_expression() { t }

        rule extern_param_list() -> Vec<Type>
            = ps:extern_param() ** token(<",">) token(<",">)? { ps }

        rule pub_form_definition() -> (Visibility, Id, Form)
            = reserved(<"pub">) p:form_definition() { (Visibility::Public, p.0, p.1) }
            / p:form_definition() { (Visibility::Private, p.0, p.1) }

        rule pub_type_definition() -> (Visibility, Def)
            = reserved(<"pub">) d:type_definition() { (Visibility::Public, d) }
            / d:type_definition() { (Visibility::Private, d) }

        rule forward_declaration() -> (Id, Scheme)
            = n:callable_identifier() token(<":">) t:type_scheme() { (n, t) }

        rule form_definition() -> (Id, Form)
            = function_definition()
            / value_definition()

        rule value_definition() -> (Id, Form)
            = reserved(<"def">) name:callable_identifier() ann:(token(<":">) s:type_scheme() { s })? token(<"=">) value:expression() {
                let mut value = value;
                if let Some(scm) = ann {
                    value.tag.annotate = Some(scm);
                }
                (name, value)
            }

        rule function_definition() -> (Id, Form)
            = reserved(<"def">) name:callable_identifier()
              token(<"(">) ps:parameter_sequence() token(<")">)
              ret_ann:(token(<":">) s:type_scheme() { s })?
              token(<"=">) body:expression() {
                  let span = body.tag.pos;
                  let (ret_ty, fn_scheme_ann) = match ret_ann {
                      Some(Scheme::Mono(t)) => (Some(t), None),
                      Some(poly_scm) => (None, Some(poly_scm)),
                      None => (None, None),
                  };
                  let lambda = Expr::Abs(Lambda { param: ps, ret_ty, body: Box::new(body) });
                  let mut form = Form::new(span, lambda);
                  if let Some(scm) = fn_scheme_ann {
                      form.tag.annotate = Some(scm);
                  }
                  (name, form)
              }

        rule type_definition() -> Def
            = reserved(<"type">) a:spanned(<type_alias()>) {
                let (span, (name, item)) = a;
                Def {
                    ident: interner.borrow_mut().intern(name.as_str()),
                    pos: span,
                    node: item,
                    visibility: Visibility::Private,
                }
            }
            / reserved(<"data">) a:spanned(<type_algebra()>) {
                let (span, (name, item)) = a;
                Def {
                    ident: interner.borrow_mut().intern(name.as_str()),
                    pos: span,
                    node: item,
                    visibility: Visibility::Private,
                }
            }
            / reserved(<"concept">) a:spanned(<concept_definition()>) {
                let (span, (name, item)) = a;
                Def {
                    ident: interner.borrow_mut().intern(name.as_str()),
                    pos: span,
                    node: item,
                    visibility: Visibility::Private,
                }
            }
            / reserved(<"instance">) a:spanned(<instance_definition()>) {
                let (span, (name, item)) = a;
                Def {
                    ident: interner.borrow_mut().intern(name.as_str()),
                    pos: span,
                    node: item,
                    visibility: Visibility::Private,
                }
            }

        // Concept & Instance definitions
        rule concept_definition() -> (String, Item)
            = n:upper_identifier_str() p:type_parameter_sequence()
              s:constraint_prefix()?
              token(<"{">) m:concept_method() ** token(<",">) token(<",">)? token(<"}">) {
                let params: Vec<Id> = p.iter().map(|s| interner.borrow_mut().intern(s)).collect();
                let superclasses = s.unwrap_or_default();
                (n.to_string(), Item::Concept {
                    type_params: params,
                    superclasses,
                    methods: m,
                })
            }

        rule constraint_prefix() -> Vec<TypeConstraint>
            = token(<"(">) cs:type_constraint() ++ token(<",">) token(<",">)? token(<")">) token(<"=>">) { cs }
            / c:type_constraint() token(<"=>">) { vec![c] }

        rule type_constraint() -> TypeConstraint
            = c:upper_identifier_str() v:type_variable_identifier() {
                TypeConstraint {
                    concept: c.to_string(),
                    type_var: v.to_string(),
                }
            }

        rule concept_method() -> MethodDecl
            = n:callable_identifier() token(<":">) t:type_scheme() {
                MethodDecl { name: n, ty: t }
            }

        rule instance_definition() -> (String, Item)
            = cs:constraint_prefix()? cn:upper_identifier_str()
              tas:instance_type_args()
              token(<"{">) ms:instance_method() ** (token(<",">) / definition_delimite()) token(<",">)? token(<"}">) {
                let constraints = cs.unwrap_or_default();
                let instance_name = format!(
                    "instance_{}_{}",
                    cn,
                    tas.iter().map(|t| t.to_string()).collect::<Vec<_>>().join("_")
                );
                (instance_name, Item::Instance {
                    concept_name: cn.to_string(),
                    type_args: tas,
                    constraints,
                    methods: ms,
                })
            }

        rule instance_type_args() -> Vec<Type>
            = ts:(type_atom() ++ whitespace_inline_only()) { ts }
            / t:type_atom() { vec![t] }

        rule instance_method() -> MethodImpl
            = reserved(<"def">) n:callable_identifier()
              token(<"(">) ps:parameter_sequence() token(<")">)
              ret_ann:(token(<":">) t:type_expression() { t })?
              token(<"=">) body:expression() {
                let span = body.tag.pos;
                let lambda = Expr::Abs(Lambda { param: ps, ret_ty: ret_ann, body: Box::new(body) });
                MethodImpl { name: n, body: Box::new(Form::new(span, lambda)) }
            }

        rule type_alias() -> (String, Item)
            = n:upper_identifier_str() p:type_parameter_sequence() token(<"=">) a:type_scheme() {
                let param = p.into_iter().map(|s| interner.borrow_mut().intern(s)).collect();
                (n.to_string(), Item::Alias(param, Box::new(a)))
            }

        rule type_algebra() -> (String, Item)
            = n:upper_identifier_str() p:type_parameter_sequence()
              token(<"{">) v:type_algebra_variant() ++ token(<",">) token(<",">)? token(<"}">) {
                let param = p.into_iter().map(|s| interner.borrow_mut().intern(s)).collect();
                (n.to_string(), Item::Alg(param, v))
            }

        rule type_algebra_variant() -> Variant
            = n:upper_identifier_str() ts:parens(<type_expression() ++ token(<",">)>) {
                let body = ts.into_iter().map(|t| Field { name: None, ty: Box::new(t) }).collect();
                Variant { name: n.to_string(), body: VariantBody::Tuple(body) }
            }
            / n:upper_identifier_str() fs:struct_fields() {
                Variant { name: n.to_string(), body: VariantBody::Struct(fs) }
            }
            / n:upper_identifier_str() {
                Variant { name: n.to_string(), body: VariantBody::Unit }
            }

        rule struct_fields() -> Vec<Field>
            = token(<"{">) fs:struct_field_declaration() ++ token(<",">) token(<",">)? token(<"}">) { fs }

        rule struct_field_declaration() -> Field
            = n:identifier() token(<":">) t:type_expression() {
                Field { name: Some(interner.borrow().trace(n).to_string()), ty: Box::new(t) }
            }

        // ====================================================================
        // Type Grammar (Right-Associative `->`, Explicit Arity vs First-Class Tuples)
        // ====================================================================

        rule type_parameter_sequence() -> Vec<&'input str>
            = (whitespace_inline_only() v:type_variable_identifier() { v })*

        rule qualified_type_name() -> String
            = mods:(m:lower_identifier_str() token(<".">) { m })+ n:upper_identifier_str() {
                format!("{}.{}", mods.join("."), n)
            }

        rule type_constant() -> Type
            = reserved(<"Void">) { Type::Void }
            / q:qualified_type_name() { Type::Con(q) }
            / n:upper_identifier_str() { Type::Con(n.to_string()) }

        rule type_variable() -> Type
            = n:type_variable_identifier() { Type::Var(n.to_string()) }

        rule type_atom() -> Type
            = token(<"(">) token(<")">) { Type::Void }
            / token(<"(">) first:type_expression() token(<",">) rest:(type_expression() ++ token(<",">)) token(<",">)? token(<")">) {
                let mut elems = vec![first];
                elems.extend(rest);
                Type::Tuple(elems)
            }
            / parens(<type_expression()>)
            / type_constant()
            / type_variable()

        rule btype() -> Type
            = first:type_atom() rest:(whitespace_inline_only() a:type_atom() { a })* {
                let mut v = vec![first];
                v.extend(rest);
                Type::compose_n(v)
            }

        pub rule type_expression() -> Type
            // () -> R  or  (T1, T2, ...) -> R  or  (T1) -> R
            = token(<"(">) ps:(type_expression() ** token(<",">)) token(<",">)? token(<")">) token(<"->">) rhs:type_expression() {
                Type::Arr(ps, Box::new(rhs))
            }
            // Single-parameter arrow: A -> B -> C == A -> (B -> C) (Right-associative!)
            / lhs:btype() token(<"->">) rhs:type_expression() {
                let params = if lhs == Type::Void { vec![] } else { vec![lhs] };
                Type::Arr(params, Box::new(rhs))
            }
            / t:btype() { t }

        pub rule type_scheme() -> Scheme
            // Canonical: `forall a b. (Eq a, Ord b) => (a, b) -> Bool`
            = reserved(<"forall">) f:type_variable_identifier()+ token(<".">) cs:constraint_prefix() b:type_expression() {
                let binders = f.into_iter().map(|s| s.to_string()).collect();
                build_constrained_scheme(binders, cs, b)
            }
            // Sugar: `forall (Eq a). (a, a) -> Bool` (collects all free type vars deterministically)
            / reserved(<"forall">) token(<"(">) cs:type_constraint() ++ token(<",">) token(<",">)? token(<")">) token(<".">) b:type_expression() {
                build_constrained_scheme(vec![], cs, b)
            }
            // Unconstrained: `forall a b. (a, b) -> a`
            / reserved(<"forall">) f:type_variable_identifier()+ token(<".">) b:type_expression() {
                let mut seen = HashSet::new();
                let mut binders = Vec::new();
                for s in f {
                    if seen.insert(s.to_string()) {
                        binders.push(s.to_string());
                    }
                }
                Scheme::Poly(binders, b)
            }
            // Implicit constraint prefix without explicit `forall`: `(Eq a) => (a, a) -> Bool`
            / cs:constraint_prefix() b:type_expression() {
                build_constrained_scheme(vec![], cs, b)
            }
            / b:type_expression() { Scheme::Mono(b) }

        // ====================================================================
        // Expressions & Patterns
        // ====================================================================

        rule spanned_form(e: rule<Expr>) -> Form
            = _e:spanned(<e()>) {
                let (s, expr) = _e;
                Form::new(s, expr)
            }

        pub rule expression() -> Form
            = precedence! {
                // Bucket 1: Multi-char `|`-leading pipelines (e.g. `|>`, lowest so `a == b && c |> f` works naturally)
                lhs:(@) op:infix_op_left(1) rhs:@ { make_infix_expr(op, lhs, rhs, interner) }
                lhs:@ op:infix_op_right(1) rhs:(@) { make_infix_expr(op, lhs, rhs, interner) }
                --
                // Short-circuit logical OR
                lhs:(@) token(<"||">) rhs:@ { make_spanned_binexpr(BinOp::Or, lhs, rhs) }
                --
                // Short-circuit logical AND
                lhs:(@) token(<"&&">) rhs:@ { make_spanned_binexpr(BinOp::And, lhs, rhs) }
                --
                // Bucket 2: `=` / `!` equality & inequality (`==`, `!=`, `===`, `=/=`, ...)
                lhs:(@) op:infix_op_left(2) rhs:@ { make_infix_expr(op, lhs, rhs, interner) }
                lhs:@ op:infix_op_right(2) rhs:(@) { make_infix_expr(op, lhs, rhs, interner) }
                --
                // Bucket 3: `<` / `>` comparisons & combinators (`<`, `<=`, `>`, `>=`, `<|>`, `<+>`, `>>=`, ...)
                lhs:(@) op:infix_op_left(3) rhs:@ { make_infix_expr(op, lhs, rhs, interner) }
                lhs:@ op:infix_op_right(3) rhs:(@) { make_infix_expr(op, lhs, rhs, interner) }
                --
                // Bucket 4: `:`-leading cons/pair ops (`::` is right-associative!)
                lhs:(@) op:infix_op_left(4) rhs:@ { make_infix_expr(op, lhs, rhs, interner) }
                lhs:@ op:infix_op_right(4) rhs:(@) { make_infix_expr(op, lhs, rhs, interner) }
                --
                // Bucket 5: Bitwise OR (`|`)
                lhs:(@) op:infix_op_left(5) rhs:@ { make_infix_expr(op, lhs, rhs, interner) }
                lhs:@ op:infix_op_right(5) rhs:(@) { make_infix_expr(op, lhs, rhs, interner) }
                --
                // Bucket 6: `^`-leading bitwise XOR (`^`, `^^`, ...)
                lhs:(@) op:infix_op_left(6) rhs:@ { make_infix_expr(op, lhs, rhs, interner) }
                lhs:@ op:infix_op_right(6) rhs:(@) { make_infix_expr(op, lhs, rhs, interner) }
                --
                // Bucket 7: `&`-leading bitwise AND (`&`, `&+`, ...)
                lhs:(@) op:infix_op_left(7) rhs:@ { make_infix_expr(op, lhs, rhs, interner) }
                lhs:@ op:infix_op_right(7) rhs:(@) { make_infix_expr(op, lhs, rhs, interner) }
                --
                // Bucket 8: Bitwise shifts (`<<`, `>>`, `>>>`)
                lhs:(@) op:infix_op_left(8) rhs:@ { make_infix_expr(op, lhs, rhs, interner) }
                lhs:@ op:infix_op_right(8) rhs:(@) { make_infix_expr(op, lhs, rhs, interner) }
                --
                // Bucket 9: `+` / `-` additive ops (`+`, `-`, `++`, `+:`, ...)
                lhs:(@) op:infix_op_left(9) rhs:@ { make_infix_expr(op, lhs, rhs, interner) }
                lhs:@ op:infix_op_right(9) rhs:(@) { make_infix_expr(op, lhs, rhs, interner) }
                --
                // Bucket 10: `*` / `/` / `%` multiplicative ops (`*`, `/`, `%`, `**`, ...)
                lhs:(@) op:infix_op_left(10) rhs:@ { make_infix_expr(op, lhs, rhs, interner) }
                lhs:@ op:infix_op_right(10) rhs:(@) { make_infix_expr(op, lhs, rhs, interner) }
                --
                // Bucket 11: Other special symbol chars (`@`, `#`, `$`, `?`, multi-char `~`, `\`)
                lhs:(@) op:infix_op_left(11) rhs:@ { make_infix_expr(op, lhs, rhs, interner) }
                lhs:@ op:infix_op_right(11) rhs:(@) { make_infix_expr(op, lhs, rhs, interner) }
                --
                whitespace() start:position!() "!" !"=" e:@ { make_spanned_unexpr(start, UnOp::Not, e) }
                whitespace() start:position!() "-" !['>' | '0'..='9'] e:@ { make_spanned_unexpr(start, UnOp::Neg, e) }
                whitespace() start:position!() "~" e:@ { make_spanned_unexpr(start, UnOp::BitNot, e) }
                --
                f:form_factor() { f }
            }

        rule form_factor() -> Form
            = start:position!() h:_form_factor() t:postfix_op()* {
                let mut f = h;
                for op in t {
                    match op {
                        PostfixOp::Call(end, app) => {
                            let param = app.into_iter().map(Box::new).collect();
                            let span = Span::new(start, end);
                            f = Form::new(span, Expr::Apply(Box::new(f), param));
                        }
                        PostfixOp::Field(end, fname) => {
                            let span = Span::new(start, end);
                            f = Form::new(span, Expr::Field(Box::new(f), fname));
                        }
                        PostfixOp::Try(end) => {
                            let span = Span::new(start, end);
                            f = Form::new(span, Expr::Try(Box::new(f)));
                        }
                    }
                }
                f
            }

        rule _form_factor() -> Form
            = spanned_form(<factor_expr()>)
            / parens(<expression()>)

        rule factor_expr() -> Expr
            = ifelse()
            / whileexpr()
            / letin()
            / matchexpr()
            / lambda()
            / unit_or_tuple_expr()
            / struct_init_expr()
            / lit()
            / var()
            / list()
            / block()

        rule unit_or_tuple_expr() -> Expr
            = token(<"(">) token(<")">) { Expr::Lit(Lit::Unit) }
            // First-class operator value / section: `(+)`, `(~)`, `(|>)`, `(<+>)`
            / start:position!() token(<"(">) op:definable_op_str() token(<")">) end:position!() {
                let span = Span::new(start, end);
                if op == "~" {
                    let aid = interner.borrow_mut().intern(".op_arg");
                    let body = Form::new(
                        span,
                        Expr::Unary(
                            UnOp::BitNot,
                            Box::new(Form::new(span, Expr::Var(aid))),
                        ),
                    );
                    Expr::Abs(Lambda {
                        param: vec![VarDecl(aid, Scheme::Slot)],
                        ret_ty: None,
                        body: Box::new(body),
                    })
                } else if let Some(bop) = builtin_binop_from_str(op) {
                    let lid = interner.borrow_mut().intern(".op_lhs");
                    let rid = interner.borrow_mut().intern(".op_rhs");
                    let body = Form::new(
                        span,
                        Expr::Binary(
                            bop,
                            Box::new(Form::new(span, Expr::Var(lid))),
                            Box::new(Form::new(span, Expr::Var(rid))),
                        ),
                    );
                    Expr::Abs(Lambda {
                        param: vec![VarDecl(lid, Scheme::Slot), VarDecl(rid, Scheme::Slot)],
                        ret_ty: None,
                        body: Box::new(body),
                    })
                } else {
                    Expr::Var(interner.borrow_mut().intern(&encode_op_name(op)))
                }
            }
            / token(<"(">) first:expression() token(<",">) rest:(expression() ++ token(<",">)) token(<",">)? token(<")">) {
                let mut elems = vec![Box::new(first)];
                elems.extend(rest.into_iter().map(Box::new));
                Expr::Tuple(elems)
            }

        rule struct_init_expr() -> Expr
            = ctor:upper_identifier_str() token(<"{">) fields:struct_init_field() ++ token(<",">) token(<",">)? token(<"}">) {
                Expr::StructInit(ctor.to_string(), fields)
            }

        rule struct_init_field() -> (Name, E)
            = fname:lower_identifier_str() token(<":">) val:expression() {
                (fname.to_string(), Box::new(val))
            }

        rule ifelse() -> Expr
            = reserved(<"if">) c:parens(<expression()>)
              t:expression() reserved(<"else">) f:expression() {
                  Expr::If(Box::new(c), Box::new(t), Box::new(f))
              }

        rule whileexpr() -> Expr
            = reserved(<"while">) c:parens(<expression()>) b:expression() {
                Expr::While(Box::new(c), Box::new(b))
            }

        rule letin() -> Expr
            = reserved(<"let">) v:variable_declaration() token(<"=">) val:expression()
              reserved(<"in">) e:expression() {
                Expr::Let(v, Box::new(val), Box::new(e))
            }
            // Destructuring let sugar: `let (a, b) = expr in body` => `match expr { (a, b) -> body }`
            / reserved(<"let">) pat:pattern() token(<"=">) val:expression()
              reserved(<"in">) e:expression() {
                let arm = MatchArm {
                    pattern: pat,
                    guard: None,
                    body: Box::new(e),
                };
                Expr::Match(Box::new(val), vec![arm])
            }

        rule matchexpr() -> Expr
            = reserved(<"match">) scrutinee:expression() token(<"{">)
              arms:match_arm() ++ token(<",">) token(<",">)? token(<"}">) {
                Expr::Match(Box::new(scrutinee), arms)
            }

        rule match_arm() -> MatchArm
            = p:pattern() g:match_guard()? token(<"->">) b:expression() {
                MatchArm { pattern: p, guard: g, body: Box::new(b) }
            }

        rule match_guard() -> E
            = reserved(<"if">) cond:parens(<expression()>) { Box::new(cond) }

        // Patterns
        rule pattern() -> Pattern
            = struct_constructor_pattern()
            / constructor_pattern()
            / unit_or_tuple_pattern()
            / parens(<pattern()>)
            / literal_pattern()
            / wildcard_pattern()
            / variable_pattern()

        rule struct_constructor_pattern() -> Pattern
            = n:upper_identifier_str() token(<"{">) fs:struct_pattern_field() ++ token(<",">) token(<",">)? token(<"}">) {
                Pattern::StructConstructor(n.to_string(), fs)
            }

        rule struct_pattern_field() -> (Name, Pattern)
            = fname:lower_identifier_str() token(<":">) pat:pattern() {
                (fname.to_string(), pat)
            }
            / fname:lower_identifier_str() {
                let id = interner.borrow_mut().intern(fname);
                (fname.to_string(), Pattern::Var(id))
            }

        rule constructor_pattern() -> Pattern
            = n:upper_identifier_str() ps:parens(<pattern() ** token(<",">)>) {
                Pattern::Constructor(n.to_string(), ps)
            }
            / n:upper_identifier_str() {
                Pattern::Constructor(n.to_string(), vec![])
            }

        rule unit_or_tuple_pattern() -> Pattern
            = token(<"(">) token(<")">) { Pattern::Lit(Lit::Unit) }
            / token(<"(">) first:pattern() token(<",">) rest:(pattern() ++ token(<",">)) token(<",">)? token(<")">) {
                let mut pats = vec![first];
                pats.extend(rest);
                Pattern::Tuple(pats)
            }

        rule wildcard_pattern() -> Pattern
            = lexeme(<"_">) { Pattern::Wildcard }

        rule variable_pattern() -> Pattern
            = n:lower_identifier_str() {
                Pattern::Var(interner.borrow_mut().intern(n))
            }

        rule literal_pattern() -> Pattern
            = l:lit() {
                match l {
                    Expr::Lit(lit) => Pattern::Lit(lit),
                    _ => unreachable!(),
                }
            }

        rule lambda() -> Expr
            = p:parens(<parameter_sequence()>) ret_ann:(token(<":">) t:type_expression() { t })? token(<"->">) to:expression() {
                Expr::Abs(Lambda { param: p, ret_ty: ret_ann, body: Box::new(to) })
            }

        rule var() -> Expr
            = id:identifier() { Expr::Var(id) }

        rule list() -> Expr
            = token(<"[">) l:expression() ** token(<",">) token(<",">)? token(<"]">) {
                Expr::List(l.into_iter().map(Box::new).collect())
            }

        rule block_sep() = (whitespace_inline() (";" / "\r\n" / "\n"))+

        rule block_stmt() -> BlockItem
            = start:position!() reserved(<"let">) v:variable_declaration() token(<"=">) val:expression() end:position!() {
                BlockItem::LetVar(Span::new(start, end), v, Box::new(val))
            }
            / start:position!() reserved(<"let">) pat:pattern() token(<"=">) val:expression() end:position!() {
                BlockItem::LetPat(Span::new(start, end), pat, Box::new(val))
            }
            / e:expression() {
                BlockItem::Expr(Box::new(e))
            }

        rule block() -> Expr
            = token(<"{">) first:block_stmt() rest:(block_sep() s:block_stmt() { s })* block_sep()? token(<"}">) {?
                let mut items = vec![first];
                items.extend(rest);
                fold_block_items(items)
            }

        rule lit() -> Expr
            = float_lit() / integer_lit() / char_lit() / string_lit() / bool_lit()

        rule float_lit() -> Expr
            = f:float() {?
                f.parse::<f64>()
                    .map(|v| Expr::Lit(Lit::Float(v)))
                    .map_err(|_| "Parse float failed")
            }

        rule integer_lit() -> Expr
            = i:integer() {?
                i.parse::<i32>()
                    .map(|v| Expr::Lit(Lit::Int(v)))
                    .or_else(|_| {
                        i.parse::<f64>()
                            .map(|v| Expr::Lit(Lit::Float(v)))
                            .map_err(|_| "Parse integer failed")
                    })
            }

        rule char_lit() -> Expr
            = s:char_lit_raw() {?
                parse_char_content(s).map(|c| Expr::Lit(Lit::Char(c)))
            }

        rule string_lit() -> Expr
            = s:string() {?
                escape_string(s).map(|s| Expr::Lit(Lit::Str(s)))
            }

        rule bool_lit() -> Expr
            = reserved(<"true">) { Expr::Lit(Lit::Bool(true)) }
            / reserved(<"false">) { Expr::Lit(Lit::Bool(false)) }

        rule postfix_op() -> PostfixOp
            = l:parens(<apply_list()>) end:position!() { PostfixOp::Call(end, l) }
            / token(<".">) fname:identifier() end:position!() {
                PostfixOp::Field(end, interner.borrow().trace(fname).to_string())
            }
            / token(<"?">) !['?' | '>' | '=' | ':'] end:position!() { PostfixOp::Try(end) }

        rule apply_list() -> Vec<Form>
            = elems:expression() ** token(<",">) token(<",">)? { elems }

        rule parameter_sequence() -> Vec<VarDecl>
            = ps:variable_declaration() ** token(<",">) token(<",">)? { ps }

        rule variable_declaration() -> VarDecl
            = n:identifier() token(<":">) t:type_expression() {
                VarDecl(n, Scheme::Mono(t))
            }
            / n:identifier() { VarDecl(n, Scheme::Slot) }
    }
}

pub use self::geisha_parser::*;

pub fn form_contains_try(form: &Form) -> bool {
    match &form.node {
        Expr::Try(_) => true,
        Expr::Lit(_) | Expr::Var(_) | Expr::QualifiedVar(_) => false,
        Expr::Field(e, _) | Expr::Unary(_, e) => form_contains_try(e),
        Expr::Tuple(es) | Expr::List(es) | Expr::Block(es) => es.iter().any(|e| form_contains_try(e)),
        Expr::StructInit(_, fs) => fs.iter().any(|(_, v)| form_contains_try(v)),
        Expr::Apply(c, args) => form_contains_try(c) || args.iter().any(|a| form_contains_try(a)),
        Expr::Abs(lam) => form_contains_try(&lam.body),
        Expr::Binary(_, l, r) | Expr::While(l, r) => form_contains_try(l) || form_contains_try(r),
        Expr::Let(_, v, b) => form_contains_try(v) || form_contains_try(b),
        Expr::If(c, t, f) => form_contains_try(c) || form_contains_try(t) || form_contains_try(f),
        Expr::Match(s, arms) => {
            form_contains_try(s)
                || arms.iter().any(|a| {
                    a.guard.as_ref().map_or(false, |g| form_contains_try(g))
                        || form_contains_try(&a.body)
                })
        }
    }
}

pub fn def_contains_try(def: &Def) -> bool {
    match &def.node {
        Item::Form(f) => form_contains_try(f),
        Item::Instance { methods, .. } => methods.iter().any(|m| form_contains_try(&m.body)),
        _ => false,
    }
}

struct TryDesugarer<'a> {
    interner: &'a mut Interner,
    ok_ctor: String,
    err_ctor: String,
    err_fn_id: Id,
    counter: usize,
}

impl<'a> TryDesugarer<'a> {
    fn fresh_ids(&mut self) -> (Id, Id) {
        self.counter += 1;
        let ok_id = self.interner.intern(&format!(".try_ok_{}", self.counter));
        let err_id = self.interner.intern(&format!(".try_err_{}", self.counter));
        (ok_id, err_id)
    }

    fn wrap_tries(&mut self, binds: Vec<(Id, Span, Form)>, mut cont: Form) -> Form {
        for (ok_id, span, scrutinee) in binds.into_iter().rev() {
            let (_, err_id) = self.fresh_ids();
            let err_arm = MatchArm {
                pattern: Pattern::Constructor(self.err_ctor.clone(), vec![Pattern::Var(err_id)]),
                guard: None,
                body: Box::new(Form::new(
                    span,
                    Expr::Apply(
                        Box::new(Form::new(span, Expr::Var(self.err_fn_id))),
                        vec![Box::new(Form::new(span, Expr::Var(err_id)))],
                    ),
                )),
            };
            let ok_arm = MatchArm {
                pattern: Pattern::Constructor(self.ok_ctor.clone(), vec![Pattern::Var(ok_id)]),
                guard: None,
                body: Box::new(cont),
            };
            let full_span = span.union(&ok_arm.body.tag.pos);
            cont = Form::new(full_span, Expr::Match(Box::new(scrutinee), vec![err_arm, ok_arm]));
        }
        cont
    }

    /// Extract unguarded `Expr::Try` nodes in left-to-right evaluation order within a single value expression
    /// (stopping at nested `Abs`, `Block`, `Let`, `If` branches, `Match` arms, `While`, and short-circuit `&&`/`||` RHS).
    fn extract_value_tries(&mut self, form: Form, binds: &mut Vec<(Id, Span, Form)>) -> Form {
        let Form { node, tag } = form;
        let span = tag.pos;
        let new_node = match node {
            Expr::Try(inner) => {
                let clean_inner = self.extract_value_tries(*inner, binds);
                let (ok_id, _) = self.fresh_ids();
                binds.push((ok_id, span, clean_inner));
                return Form {
                    node: Expr::Var(ok_id),
                    tag,
                };
            }
            Expr::Lit(_) | Expr::Var(_) | Expr::QualifiedVar(_) => node,
            Expr::Field(base, fname) => {
                let b = self.extract_value_tries(*base, binds);
                Expr::Field(Box::new(b), fname)
            }
            Expr::Unary(op, operand) => {
                let o = self.extract_value_tries(*operand, binds);
                Expr::Unary(op, Box::new(o))
            }
            Expr::Binary(op @ (BinOp::And | BinOp::Or), lhs, rhs) => {
                let l = self.extract_value_tries(*lhs, binds);
                // Short-circuit RHS only desugars nested lambdas
                let r = self.desugar_non_result_expr(*rhs);
                Expr::Binary(op, Box::new(l), Box::new(r))
            }
            Expr::Binary(op, lhs, rhs) => {
                let l = self.extract_value_tries(*lhs, binds);
                let r = self.extract_value_tries(*rhs, binds);
                Expr::Binary(op, Box::new(l), Box::new(r))
            }
            Expr::Apply(callee, args) => {
                let c = self.extract_value_tries(*callee, binds);
                let new_args = args
                    .into_iter()
                    .map(|a| Box::new(self.extract_value_tries(*a, binds)))
                    .collect();
                Expr::Apply(Box::new(c), new_args)
            }
            Expr::Tuple(es) => Expr::Tuple(
                es.into_iter()
                    .map(|e| Box::new(self.extract_value_tries(*e, binds)))
                    .collect(),
            ),
            Expr::List(es) => Expr::List(
                es.into_iter()
                    .map(|e| Box::new(self.extract_value_tries(*e, binds)))
                    .collect(),
            ),
            Expr::StructInit(cname, fs) => Expr::StructInit(
                cname,
                fs.into_iter()
                    .map(|(k, v)| (k, Box::new(self.extract_value_tries(*v, binds))))
                    .collect(),
            ),
            Expr::Abs(mut lam) => {
                lam.body = Box::new(self.desugar_result_scope(*lam.body));
                Expr::Abs(lam)
            }
            other => self.desugar_non_result_expr(Form { node: other, tag: tag.clone() }).node,
        };
        Form { node: new_node, tag }
    }

    fn desugar_non_result_expr(&mut self, form: Form) -> Form {
        let Form { node, tag } = form;
        let new_node = match node {
            Expr::Abs(mut lam) => {
                lam.body = Box::new(self.desugar_result_scope(*lam.body));
                Expr::Abs(lam)
            }
            Expr::While(c, b) => Expr::While(
                Box::new(self.desugar_non_result_expr(*c)),
                Box::new(self.desugar_non_result_expr(*b)),
            ),
            Expr::If(c, t, f) => Expr::If(
                Box::new(self.desugar_non_result_expr(*c)),
                Box::new(self.desugar_non_result_expr(*t)),
                Box::new(self.desugar_non_result_expr(*f)),
            ),
            Expr::Let(v, val, body) => Expr::Let(
                v,
                Box::new(self.desugar_non_result_expr(*val)),
                Box::new(self.desugar_non_result_expr(*body)),
            ),
            Expr::Match(s, arms) => Expr::Match(
                Box::new(self.desugar_non_result_expr(*s)),
                arms.into_iter()
                    .map(|mut a| {
                        a.guard = a.guard.map(|g| Box::new(self.desugar_non_result_expr(*g)));
                        a.body = Box::new(self.desugar_non_result_expr(*a.body));
                        a
                    })
                    .collect(),
            ),
            Expr::Block(es) => Expr::Block(
                es.into_iter()
                    .map(|e| Box::new(self.desugar_non_result_expr(*e)))
                    .collect(),
            ),
            Expr::Tuple(es) => Expr::Tuple(
                es.into_iter()
                    .map(|e| Box::new(self.desugar_non_result_expr(*e)))
                    .collect(),
            ),
            Expr::List(es) => Expr::List(
                es.into_iter()
                    .map(|e| Box::new(self.desugar_non_result_expr(*e)))
                    .collect(),
            ),
            Expr::StructInit(c, fs) => Expr::StructInit(
                c,
                fs.into_iter()
                    .map(|(k, v)| (k, Box::new(self.desugar_non_result_expr(*v))))
                    .collect(),
            ),
            Expr::Apply(c, args) => Expr::Apply(
                Box::new(self.desugar_non_result_expr(*c)),
                args.into_iter()
                    .map(|a| Box::new(self.desugar_non_result_expr(*a)))
                    .collect(),
            ),
            Expr::Binary(op, l, r) => Expr::Binary(
                op,
                Box::new(self.desugar_non_result_expr(*l)),
                Box::new(self.desugar_non_result_expr(*r)),
            ),
            Expr::Unary(op, e) => Expr::Unary(op, Box::new(self.desugar_non_result_expr(*e))),
            Expr::Field(e, n) => Expr::Field(Box::new(self.desugar_non_result_expr(*e)), n),
            other => other,
        };
        Form { node: new_node, tag }
    }

    /// Desugar an expression in a `Result`-producing tail position (function body, block tail, `if` branch, `match` arm).
    fn desugar_result_scope(&mut self, form: Form) -> Form {
        if !form_contains_try(&form) {
            return form;
        }
        let Form { node, tag } = form;
        match node {
            Expr::Abs(mut lam) => {
                lam.body = Box::new(self.desugar_result_scope(*lam.body));
                Form {
                    node: Expr::Abs(lam),
                    tag,
                }
            }
            Expr::Let(v, val, body) => {
                let mut binds = Vec::new();
                let clean_val = self.extract_value_tries(*val, &mut binds);
                let clean_body = self.desugar_result_scope(*body);
                let rebuilt = Form {
                    node: Expr::Let(v, Box::new(clean_val), Box::new(clean_body)),
                    tag,
                };
                self.wrap_tries(binds, rebuilt)
            }
            Expr::Block(mut items) => {
                if items.is_empty() {
                    return Form {
                        node: Expr::Block(items),
                        tag,
                    };
                }
                let last = *items.pop().unwrap();
                let mut acc = self.desugar_result_scope(last);
                while let Some(stmt) = items.pop() {
                    let mut binds = Vec::new();
                    let clean_stmt = self.extract_value_tries(*stmt, &mut binds);
                    let bspan = clean_stmt.tag.pos.union(&acc.tag.pos);
                    let seq = match acc.node {
                        Expr::Block(mut tail_items) => {
                            let mut combined = vec![Box::new(clean_stmt)];
                            combined.append(&mut tail_items);
                            Form::new(bspan, Expr::Block(combined))
                        }
                        _ => Form::new(bspan, Expr::Block(vec![Box::new(clean_stmt), Box::new(acc)])),
                    };
                    acc = self.wrap_tries(binds, seq);
                }
                acc.tag.annotate = tag.annotate;
                acc
            }
            Expr::If(cond, then_e, else_e) => {
                let mut binds = Vec::new();
                let clean_cond = self.extract_value_tries(*cond, &mut binds);
                let clean_then = self.desugar_result_scope(*then_e);
                let clean_else = self.desugar_result_scope(*else_e);
                let rebuilt = Form {
                    node: Expr::If(
                        Box::new(clean_cond),
                        Box::new(clean_then),
                        Box::new(clean_else),
                    ),
                    tag,
                };
                self.wrap_tries(binds, rebuilt)
            }
            Expr::Match(scrut, arms) => {
                let mut binds = Vec::new();
                let clean_scrut = self.extract_value_tries(*scrut, &mut binds);
                let clean_arms = arms
                    .into_iter()
                    .map(|mut arm| {
                        arm.guard = arm.guard.map(|g| Box::new(self.desugar_non_result_expr(*g)));
                        arm.body = Box::new(self.desugar_result_scope(*arm.body));
                        arm
                    })
                    .collect();
                let rebuilt = Form {
                    node: Expr::Match(Box::new(clean_scrut), clean_arms),
                    tag,
                };
                self.wrap_tries(binds, rebuilt)
            }
            other => {
                let mut binds = Vec::new();
                let clean = self.extract_value_tries(Form { node: other, tag }, &mut binds);
                self.wrap_tries(binds, clean)
            }
        }
    }
}

pub fn desugar_try_in_def(
    def: &mut Def,
    ok_ctor: &str,
    err_ctor: &str,
    interner: &mut Interner,
) {
    if !def_contains_try(def) {
        return;
    }
    let err_fn_id = interner.intern(err_ctor);
    let mut d = TryDesugarer {
        interner,
        ok_ctor: ok_ctor.to_string(),
        err_ctor: err_ctor.to_string(),
        err_fn_id,
        counter: 0,
    };
    match &mut def.node {
        Item::Form(ref mut f) => {
            let taken = (**f).clone();
            **f = d.desugar_result_scope(taken);
        }
        Item::Instance { ref mut methods, .. } => {
            for m in methods {
                let taken = (*m.body).clone();
                *m.body = d.desugar_result_scope(taken);
            }
        }
        _ => {}
    }
}

pub fn parse(src: &str, interner: &mut Interner) -> Result<Vec<Def>, peg::error::ParseError<peg::str::LineCol>> {
    let cell = RefCell::new(std::mem::take(interner));
    let result = module(src, &cell);
    *interner = cell.into_inner();
    result
}

pub fn parse_type(src: &str, interner: &mut Interner) -> Scheme {
    let cell = RefCell::new(std::mem::take(interner));
    let result = type_scheme(src, &cell);
    *interner = cell.into_inner();
    match result {
        Ok(scm) => scm,
        Err(err) => panic!("Parse type '{}' failed: {:?}", src, err),
    }
}

pub fn parse_expr(src: &str, interner: &mut Interner) -> Result<Form, peg::error::ParseError<peg::str::LineCol>> {
    let cell = RefCell::new(std::mem::take(interner));
    let result = expression(src, &cell);
    *interner = cell.into_inner();
    result
}

pub fn parse_file_module(
    src: &str,
    path: &ModulePath,
    interner: &RefCell<Interner>,
) -> Result<FileModule, peg::error::ParseError<peg::str::LineCol>> {
    file_module(src, interner, path)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn case_parse_arrow_right_associative() {
        let cell = RefCell::new(Interner::new());
        let parsed = type_expression("Int -> String -> Bool", &cell).unwrap();
        assert_eq!(
            parsed,
            Type::Arr(
                vec![Type::Con("Int".to_string())],
                Box::new(Type::Arr(
                    vec![Type::Con("String".to_string())],
                    Box::new(Type::Con("Bool".to_string())),
                ))
            )
        );
    }

    #[test]
    fn case_parse_multi_param_vs_tuple_type() {
        let cell = RefCell::new(Interner::new());
        // 2-param function
        let fn2 = type_expression("(Int, String) -> Bool", &cell).unwrap();
        assert_eq!(
            fn2,
            Type::Arr(
                vec![Type::Con("Int".to_string()), Type::Con("String".to_string())],
                Box::new(Type::Con("Bool".to_string()))
            )
        );

        // 0-param function
        let fn0 = type_expression("() -> Int", &cell).unwrap();
        assert_eq!(fn0, Type::Arr(vec![], Box::new(Type::Con("Int".to_string()))));

        // 1-param function taking a 2-tuple
        let fn_tuple = type_expression("((Int, String)) -> Bool", &cell).unwrap();
        assert_eq!(
            fn_tuple,
            Type::Arr(
                vec![Type::Tuple(vec![
                    Type::Con("Int".to_string()),
                    Type::Con("String".to_string()),
                ])],
                Box::new(Type::Con("Bool".to_string()))
            )
        );

        // Standalone tuple type
        let tup = type_expression("(Int, String)", &cell).unwrap();
        assert_eq!(
            tup,
            Type::Tuple(vec![
                Type::Con("Int".to_string()),
                Type::Con("String".to_string()),
            ])
        );
    }

    #[test]
    fn case_parse_comments_escapes_blocks_and_unary() {
        let src = r#"
// Line comment
/* Multi-line
   comment */
def demo(x: Int): Int = {
    let s = "quote \" and slash \\";
    let b = !false;
    if (b) -x else 0
}
"#;
        let cell = RefCell::new(Interner::new());
        let defs = module(src, &cell).expect("Should parse comments, escapes, blocks, and unary ops");
        assert_eq!(defs.len(), 1);
    }

    #[test]
    fn case_parse_constrained_forall_both_syntaxes() {
        let cell = RefCell::new(Interner::new());
        let s1 = type_scheme("forall k v. (Eq k) => (k, List (k, v)) -> Maybe v", &cell).unwrap();
        if let Scheme::PolyConstrained(vars, cs, _) = s1 {
            assert_eq!(vars, vec!["k".to_string(), "v".to_string()]);
            assert_eq!(cs.len(), 1);
        } else {
            panic!("Expected PolyConstrained");
        }

        // Duplicate constraint on same var deduplicates binder
        let s2 = type_scheme("forall (Eq a, Show a). a -> String", &cell).unwrap();
        if let Scheme::PolyConstrained(vars, cs, _) = s2 {
            assert_eq!(vars, vec!["a".to_string()]);
            assert_eq!(cs.len(), 2);
        } else {
            panic!("Expected PolyConstrained");
        }
    }

    #[test]
    fn case_parse_module_items_and_visibility() {
        let src = "
use math.{add, mul}
pub def main() = add(1, mul(2, 3))
";
        let cell = RefCell::new(Interner::new());
        let path = ModulePath::new(vec![]);
        let fm = file_module(src, &cell, &path).unwrap();
        assert_eq!(fm.items.len(), 2);
    }
}
