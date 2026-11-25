use std::collections::HashMap;

use crate::codegen::target::configure_module_target;
use crate::internal::*;
use crate::types::*;

pub use libllvm::*;

/// Note field order: `module` and `builder` MUST precede `context` so Rust drops them before `LLVMContextDispose`!
#[derive(Debug)]
pub struct LLVMCodegen {
    pub module: LLVMModule,
    pub builder: LLVMBuilder,
    pub context: LLVMContext,
    unique: usize,
    pub adt_registry: AdtRegistry,
}

pub fn is_primitive_type(t: &Type) -> bool {
    match t {
        Type::Con(n) => matches!(
            n.as_str(),
            "Int" | "Int64" | "Float" | "Char" | "String" | "Void" | "Bool" | "Ptr" | "AtomicInt"
        ),
        Type::Void | Type::Var(_) => true,
        _ => false,
    }
}

pub fn is_adt_type(t: &Type) -> bool {
    match t {
        Type::Con(n) => !matches!(
            n.as_str(),
            "Int"
                | "Int64"
                | "Float"
                | "Char"
                | "String"
                | "Void"
                | "Bool"
                | "Ptr"
                | "AtomicInt"
        ),
        Type::Comp(_, _) => {
            let base = get_comp_base_name_static(t);
            !matches!(
                base.as_str(),
                "Int"
                    | "Int64"
                    | "Float"
                    | "Char"
                    | "String"
                    | "Void"
                    | "Bool"
                    | "Ptr"
                    | "Cell"
                    | "MutArray"
                    | "MVar"
                    | "Chan"
                    | "Task"
                    | "Actor"
            )
        }
        _ => false,
    }
}

/// Whether an element type contains zero interior GC heap pointers (eligible for `gc_alloc_atomic` `NOSCAN`).
pub fn is_noscan_elem_type(t: &Type) -> bool {
    match t {
        Type::Void => true,
        Type::Con(n) => matches!(
            n.as_str(),
            "Int" | "Int64" | "Float" | "Char" | "Bool" | "Void" | "Ptr"
        ),
        Type::Tuple(elems) => elems.iter().all(is_noscan_elem_type),
        _ => false,
    }
}

/// Whether a global variable slot of type `t` holds a single GC heap pointer (`void*`)
pub fn is_gc_pointer_type(t: &Type) -> bool {
    match t {
        Type::Arr(..) => true,
        Type::Con(n) => matches!(n.as_str(), "String" | "AtomicInt") || is_adt_type(t),
        Type::Comp(..) => {
            let base = get_comp_base_name_static(t);
            matches!(
                base.as_str(),
                "Cell" | "MutArray" | "MVar" | "Chan" | "Task" | "Actor"
            ) || is_adt_type(t)
        }
        _ => false,
    }
}

fn get_comp_base_name_static(ty: &Type) -> String {
    let mut current = ty;
    while let Type::Comp(base, _) = current {
        current = base.as_ref();
    }
    match current {
        Type::Con(name) => name.clone(),
        _ => String::new(),
    }
}

// ============================================================================
// Module-qualified Name Mangling
// ============================================================================

pub fn mangle_module_name(module_path: &[&str], name: &str) -> String {
    if module_path.is_empty() {
        return name.to_string();
    }
    let mut result = String::from("_G");
    for segment in module_path {
        result.push_str(&segment.len().to_string());
        result.push('M');
        result.push_str(segment);
    }
    result.push_str(&name.len().to_string());
    result.push('N');
    result.push_str(name);
    result
}

pub fn mangle_module_name_with_types(
    module_path: &[&str],
    name: &str,
    type_args: &[Type],
) -> String {
    let mut base = mangle_module_name(module_path, name);
    if !type_args.is_empty() {
        base.push('_');
        for (i, ty) in type_args.iter().enumerate() {
            if i > 0 {
                base.push('_');
            }
            base.push_str(&mangle_type(ty));
        }
    }
    base
}

fn mangle_type(ty: &Type) -> String {
    match ty {
        Type::Con(name) => name.clone(),
        Type::Var(name) => format!("T{}", name),
        Type::Arr(params, to) => {
            let ps: Vec<String> = params.iter().map(mangle_type).collect();
            format!("F{}_{}", ps.join("_"), mangle_type(to))
        }
        Type::Tuple(elems) => {
            let es: Vec<String> = elems.iter().map(mangle_type).collect();
            format!("P{}", es.join("_"))
        }
        Type::Comp(base, arg) => format!("{}{}", mangle_type(base), mangle_type(arg)),
        Type::Void => "V".to_string(),
    }
}

#[allow(dead_code)]
pub fn demangle_module_name(mangled: &str) -> Option<(Vec<String>, String)> {
    if !mangled.starts_with("_G") {
        return Some((vec![], mangled.to_string()));
    }
    let mut chars = mangled[2..].chars().peekable();
    let mut module_path = Vec::new();

    while chars.peek().is_some() {
        let mut len_str = String::new();
        while let Some(&c) = chars.peek() {
            if c.is_ascii_digit() {
                len_str.push(chars.next().unwrap());
            } else {
                break;
            }
        }
        let len: usize = len_str.parse().ok()?;
        let marker = chars.next()?;
        let segment: String = chars.by_ref().take(len).collect();
        if segment.len() != len {
            return None;
        }
        match marker {
            'M' => module_path.push(segment),
            'N' => return Some((module_path, segment)),
            _ => return None,
        }
    }
    None
}

impl LLVMCodegen {
    pub fn new(name: &str, adt_registry: AdtRegistry) -> LLVMCodegen {
        let context = LLVMContext::new();
        let module = LLVMModule::in_ctx(name, &context);
        // Set host target triple & ABI data layout immediately so `LLVMABISizeOfType` is accurate during codegen!
        configure_module_target(&module);
        let builder = LLVMBuilder::in_ctx(&context);
        LLVMCodegen {
            module,
            builder,
            context,
            unique: 0,
            adt_registry,
        }
    }

    pub fn new_symbol_string(&mut self, prefix: &str) -> String {
        let f = format!("{}{}", prefix, self.unique);
        self.unique += 1;
        f
    }

    /// Generic closure struct `{ ptr entry_fn, ptr env }`
    pub fn get_closure_header_type(&self) -> LLVMType {
        let p = self.context.get_ptr_type();
        self.context.get_struct_type(&[p, p], false)
    }

    /// Concrete closure struct `{ ptr entry_fn, { fv0_ty, fv1_ty, ... } }`
    pub fn get_actual_cls_type(&self, fv_tys: &[LLVMType]) -> LLVMType {
        let p = self.context.get_ptr_type();
        let env_struct = self.context.get_struct_type(fv_tys, false);
        self.context.get_struct_type(&[p, env_struct], false)
    }

    pub fn get_main_type(&self) -> LLVMType {
        let i32_ty = self.context.get_int32_type();
        let ptr_ty = self.context.get_ptr_type();
        LLVMContext::get_function_type(&i32_ty, &[i32_ty, ptr_ty], false)
    }

    /// Runtime value representation of a Geisha `Type` (in registers, allocas, tuples, parameters, struct fields).
    /// Note: `Void` is represented as `i8` (`0`) in value contexts so unit values can live in variables/tuples/phis cleanly!
    pub fn get_val_llvm_type(&self, ty: &Type) -> LLVMType {
        match ty {
            Type::Con(n) => match n.as_str() {
                "Int" => self.context.get_int32_type(),
                "Int64" => self.context.get_int64_type(),
                "Float" => self.context.get_double_type(),
                "Char" => self.context.get_int8_type(),
                "Bool" => self.context.get_int1_type(),
                "String" => self.context.get_ptr_type(),
                "Void" => self.context.get_int8_type(),
                // User ADT values are always represented as pointers (`ptr`)
                _ => self.context.get_ptr_type(),
            },
            Type::Void => self.context.get_int8_type(),
            // Closures and parameterized ADTs are pointers (`ptr`)
            Type::Arr(..) | Type::Comp(..) => self.context.get_ptr_type(),
            // First-class tuples are unpacked structs `{ T0, T1, ... }` (proper alignment for GC scanning!)
            Type::Tuple(elems) => {
                let tys: Vec<LLVMType> = elems.iter().map(|t| self.get_val_llvm_type(t)).collect();
                self.context.get_struct_type(&tys, false)
            }
            // Fallback for any residual type variable
            Type::Var(_) => self.context.get_int64_type(),
        }
    }

    /// Build `LLVMFunctionType` for a Geisha function type `(P0, P1, ...) -> R`:
    /// appends `ptr %fvs` as the trailing closure environment parameter, and uses `void` return iff `R == Void`.
    pub fn get_fn_llvm_type(&self, fn_ty: &Type) -> LLVMType {
        match fn_ty {
            Type::Arr(params, ret) => {
                let mut llvm_params: Vec<LLVMType> =
                    params.iter().map(|p| self.get_val_llvm_type(p)).collect();
                llvm_params.push(self.context.get_ptr_type()); // trailing `void *fvs`
                let ret_llvm = if matches!(ret.as_ref(), Type::Void)
                    || matches!(ret.as_ref(), Type::Con(n) if n == "Void")
                {
                    self.context.get_void_type()
                } else {
                    self.get_val_llvm_type(ret)
                };
                LLVMContext::get_function_type(&ret_llvm, &llvm_params, false)
            }
            _ => panic!("Expected function type in get_fn_llvm_type, got {}", fn_ty),
        }
    }

    /// Build `LLVMFunctionType` for a standard platform C ABI `extern` function `(P0, P1, ...) -> R`
    /// (NO trailing `ptr %fvs` closure environment parameter!).
    pub fn get_c_fn_llvm_type(&self, fn_ty: &Type) -> LLVMType {
        match fn_ty {
            Type::Arr(params, ret) => {
                let llvm_params: Vec<LLVMType> =
                    params.iter().map(|p| self.get_val_llvm_type(p)).collect();
                let ret_llvm = if matches!(ret.as_ref(), Type::Void)
                    || matches!(ret.as_ref(), Type::Con(n) if n == "Void")
                {
                    self.context.get_void_type()
                } else {
                    self.get_val_llvm_type(ret)
                };
                LLVMContext::get_function_type(&ret_llvm, &llvm_params, false)
            }
            _ => panic!("Expected function type in get_c_fn_llvm_type, got {}", fn_ty),
        }
    }

    pub fn get_or_add_c_function(&self, c_symbol: &str, fty: &Type) -> LLVMFunction {
        match self.module.get_function(c_symbol) {
            Some(f) => f,
            None => {
                let ty = self.get_c_fn_llvm_type(fty);
                self.module.add_function(c_symbol, &ty)
            }
        }
    }

    pub fn gen_lit(&mut self, lit: &Lit) -> LLVMValue {
        match *lit {
            Lit::Float(f) => self.context.get_double_const(f),
            Lit::Int(i) => self.context.get_int32_const(i),
            Lit::Char(c) => self.context.get_uint8_const(c),
            Lit::Bool(true) => self.context.get_int1_const(1),
            Lit::Bool(false) => self.context.get_int1_const(0),
            Lit::Unit => self.context.get_uint8_const(0),
            Lit::Str(ref s) => {
                let sym = self.new_symbol_string(".str.");
                self.context.create_global_string(&self.module, s, &sym)
            }
        }
    }

    /// Compute ABI-accurate storage struct type `{ i32 tag, [N x word] }` for an ADT instance!
    /// Uses maximum byte size and alignment across ALL variants (`LLVMABISizeOfType` / `LLVMABIAlignmentOfType`).
    pub fn get_adt_storage_llvm_type(&self, adt_name: &str, type_args: &[Type]) -> LLVMType {
        let adt_info = self
            .adt_registry
            .get(adt_name)
            .unwrap_or_else(|| panic!("Unknown ADT type: {}", adt_name));

        let subst: HashMap<String, Type> = adt_info
            .type_params
            .iter()
            .cloned()
            .zip(type_args.iter().cloned())
            .collect();

        let tag_ty = self.context.get_int32_type();
        let mut max_bytes: u64 = 0;
        let mut max_align: u64 = 1;

        for variant in &adt_info.variants {
            if variant.field_types.is_empty() {
                continue;
            }
            let v_struct = self.get_variant_payload_struct_type(adt_info, variant, &subst);
            let sz = self.get_type_size(&v_struct);
            let al = self.get_type_align(&v_struct);
            if sz > max_bytes {
                max_bytes = sz;
            }
            if al > max_align {
                max_align = al;
            }
        }

        if max_bytes == 0 {
            self.context.get_struct_type(&[tag_ty], false)
        } else if max_align >= 8 {
            let words = (max_bytes + 7) / 8;
            let payload_arr = LLVMContext::get_array_type(&self.context.get_int64_type(), words);
            self.context.get_struct_type(&[tag_ty, payload_arr], false)
        } else {
            let words = (max_bytes + 3) / 4;
            let payload_arr = LLVMContext::get_array_type(&self.context.get_int32_type(), words);
            self.context.get_struct_type(&[tag_ty, payload_arr], false)
        }
    }

    pub fn get_variant_payload_struct_type(
        &self,
        _adt_info: &AdtInfo,
        variant: &VariantInfo,
        subst: &HashMap<String, Type>,
    ) -> LLVMType {
        let field_llvm_tys: Vec<LLVMType> = variant
            .field_types
            .iter()
            .map(|ft| {
                let concrete = self.substitute_type(ft, subst);
                self.get_val_llvm_type(&concrete)
            })
            .collect();
        self.context.get_struct_type(&field_llvm_tys, false)
    }

    pub fn substitute_type(&self, ty: &Type, subst: &HashMap<String, Type>) -> Type {
        match ty {
            Type::Var(name) => subst.get(name).cloned().unwrap_or_else(|| ty.clone()),
            Type::Con(_) | Type::Void => ty.clone(),
            Type::Arr(ps, r) => Type::Arr(
                ps.iter().map(|p| self.substitute_type(p, subst)).collect(),
                Box::new(self.substitute_type(r, subst)),
            ),
            Type::Tuple(es) => Type::Tuple(
                es.iter().map(|e| self.substitute_type(e, subst)).collect(),
            ),
            Type::Comp(base, arg) => Type::Comp(
                Box::new(self.substitute_type(base, subst)),
                Box::new(self.substitute_type(arg, subst)),
            ),
        }
    }

    pub fn get_or_add_function(&self, fun_name: &str, fty: &Type) -> LLVMFunction {
        match self.module.get_function(fun_name) {
            Some(f) => f,
            None => {
                let ty = self.get_fn_llvm_type(fty);
                self.module.add_function(fun_name, &ty)
            }
        }
    }

    pub fn declare_gc_init_with_stack(&self) -> LLVMFunction {
        if let Some(f) = self.module.get_function("gc_init_with_stack") {
            return f;
        }
        let size_ty = self.context.get_int64_type();
        let ptr_ty = self.context.get_ptr_type();
        let void_ty = self.context.get_void_type();
        let fn_ty = LLVMContext::get_function_type(&void_ty, &[size_ty, ptr_ty], false);
        self.module.add_function("gc_init_with_stack", &fn_ty)
    }

    pub fn declare_gc_register_root(&self) -> LLVMFunction {
        if let Some(f) = self.module.get_function("gc_register_root") {
            return f;
        }
        let ptr_ty = self.context.get_ptr_type();
        let void_ty = self.context.get_void_type();
        let fn_ty = LLVMContext::get_function_type(&void_ty, &[ptr_ty], false);
        self.module.add_function("gc_register_root", &fn_ty)
    }

    pub fn declare_gc_alloc(&self) -> LLVMFunction {
        if let Some(f) = self.module.get_function("gc_alloc") {
            return f;
        }
        let size_ty = self.context.get_int64_type();
        let ptr_ty = self.context.get_ptr_type();
        let fn_ty = LLVMContext::get_function_type(&ptr_ty, &[size_ty], false);
        self.module.add_function("gc_alloc", &fn_ty)
    }

    pub fn declare_gc_alloc_atomic(&self) -> LLVMFunction {
        if let Some(f) = self.module.get_function("gc_alloc_atomic") {
            return f;
        }
        let size_ty = self.context.get_int64_type();
        let ptr_ty = self.context.get_ptr_type();
        let fn_ty = LLVMContext::get_function_type(&ptr_ty, &[size_ty], false);
        self.module.add_function("gc_alloc_atomic", &fn_ty)
    }

    pub fn declare_gc_stw_requested_global(&self) -> LLVMValue {
        if let Some(g) = self.module.get_global("__gs_gc_stw_requested") {
            return g;
        }
        let i32_ty = self.context.get_int32_type();
        self.module
            .add_external_global(&i32_ty, "__gs_gc_stw_requested")
    }

    pub fn declare_gc_safepoint_slow(&self) -> LLVMFunction {
        if let Some(f) = self.module.get_function("gc_safepoint_slow") {
            return f;
        }
        let void_ty = self.context.get_void_type();
        let fn_ty = LLVMContext::get_function_type(&void_ty, &[], false);
        self.module.add_function("gc_safepoint_slow", &fn_ty)
    }

    pub fn declare_posix_setjmp(&self) -> LLVMFunction {
        if let Some(f) = self.module.get_function("_setjmp") {
            return f;
        }
        let i32_ty = self.context.get_int32_type();
        let ptr_ty = self.context.get_ptr_type();
        let fn_ty = LLVMContext::get_function_type(&i32_ty, &[ptr_ty], false);
        self.module.add_function("_setjmp", &fn_ty)
    }

    pub fn declare_gc_enter_blocking(&self) -> LLVMFunction {
        if let Some(f) = self.module.get_function("gc_enter_blocking") {
            return f;
        }
        let void_ty = self.context.get_void_type();
        let ptr_ty = self.context.get_ptr_type();
        let fn_ty = LLVMContext::get_function_type(&void_ty, &[ptr_ty], false);
        self.module.add_function("gc_enter_blocking", &fn_ty)
    }

    pub fn declare_gc_leave_blocking(&self) -> LLVMFunction {
        if let Some(f) = self.module.get_function("gc_leave_blocking") {
            return f;
        }
        let void_ty = self.context.get_void_type();
        let fn_ty = LLVMContext::get_function_type(&void_ty, &[], false);
        self.module.add_function("gc_leave_blocking", &fn_ty)
    }

    pub fn declare_c_runtime_fn(
        &self,
        sym: &str,
        ret_ty: &LLVMType,
        param_tys: &[LLVMType],
    ) -> LLVMFunction {
        if let Some(f) = self.module.get_function(sym) {
            return f;
        }
        let fn_ty = LLVMContext::get_function_type(ret_ty, param_tys, false);
        self.module.add_function(sym, &fn_ty)
    }

    pub fn declare_gs_str_cmp(&self) -> LLVMFunction {
        if let Some(f) = self.module.get_function("gs_str_cmp") {
            return f;
        }
        let ptr_ty = self.context.get_ptr_type();
        let i32_ty = self.context.get_int32_type();
        let fn_ty = LLVMContext::get_function_type(&i32_ty, &[ptr_ty, ptr_ty], false);
        self.module.add_function("gs_str_cmp", &fn_ty)
    }

    pub fn declare_gs_match_fail(&self) -> LLVMFunction {
        if let Some(f) = self.module.get_function("gs_match_fail") {
            return f;
        }
        let void_ty = self.context.get_void_type();
        let fn_ty = LLVMContext::get_function_type(&void_ty, &[], false);
        self.module.add_function("gs_match_fail", &fn_ty)
    }

    pub fn declare_gs_bounds_fail(&self) -> LLVMFunction {
        if let Some(f) = self.module.get_function("gs_bounds_fail") {
            return f;
        }
        let void_ty = self.context.get_void_type();
        let i32_ty = self.context.get_int32_type();
        let fn_ty = LLVMContext::get_function_type(&void_ty, &[i32_ty, i32_ty], false);
        self.module.add_function("gs_bounds_fail", &fn_ty)
    }

    pub fn declare_libc_strlen(&self) -> LLVMFunction {
        if let Some(f) = self.module.get_function("strlen") {
            return f;
        }
        let i64_ty = self.context.get_int64_type();
        let ptr_ty = self.context.get_ptr_type();
        let fn_ty = LLVMContext::get_function_type(&i64_ty, &[ptr_ty], false);
        self.module.add_function("strlen", &fn_ty)
    }

    pub fn declare_libc_memcpy(&self) -> LLVMFunction {
        if let Some(f) = self.module.get_function("memcpy") {
            return f;
        }
        let ptr_ty = self.context.get_ptr_type();
        let i64_ty = self.context.get_int64_type();
        let fn_ty = LLVMContext::get_function_type(&ptr_ty, &[ptr_ty, ptr_ty, i64_ty], false);
        self.module.add_function("memcpy", &fn_ty)
    }

    pub fn get_type_size(&self, ty: &LLVMType) -> u64 {
        unsafe {
            let target_data = llvm_sys::target::LLVMGetModuleDataLayout(self.module.raw_ptr());
            llvm_sys::target::LLVMABISizeOfType(target_data, ty.raw_ptr())
        }
    }

    pub fn get_type_align(&self, ty: &LLVMType) -> u64 {
        unsafe {
            let target_data = llvm_sys::target::LLVMGetModuleDataLayout(self.module.raw_ptr());
            llvm_sys::target::LLVMABIAlignmentOfType(target_data, ty.raw_ptr()) as u64
        }
    }

    pub fn create_entry_block_alloca(
        &self,
        fun: &LLVMFunction,
        var_name: &str,
        llvm_val_ty: &LLVMType,
    ) -> LLVMValue {
        let builder = LLVMBuilder::in_ctx(&self.context);
        let block = fun.get_entry_basic_block();
        let fi = block.get_first_instr();
        builder.set_position(&block, &fi);
        builder.alloca(llvm_val_ty, var_name)
    }

    pub fn bin_operator(
        &mut self,
        op: BinOp,
        lhs: LLVMValue,
        rhs: LLVMValue,
        operand_ty: &Type,
    ) -> LLVMValue {
        use self::BinOp::*;
        let sym = self.new_symbol_string("op");
        let csym = to_cstring(&sym);

        let type_name = match operand_ty {
            Type::Con(name) => name.as_str(),
            _ => "Int",
        };

        if type_name == "String" {
            if op == Add {
                let strlen_fn = self.declare_libc_strlen();
                let memcpy_fn = self.declare_libc_memcpy();
                let gc_alloc_fn = self.declare_gc_alloc();
                let i8_ty = self.context.get_int8_type();
                let one64 = self.context.get_int64_const(1);
                let zero8 = self.context.get_uint8_const(0);

                let la = self.builder.call(&strlen_fn, &mut [lhs], "cat.la");
                let lb = self.builder.call(&strlen_fn, &mut [rhs], "cat.lb");
                let lab = self.builder.add(&la, &lb, "cat.lab");
                let alloc_sz = self.builder.add(&lab, &one64, "cat.alloc");

                let dst = self.builder.call(&gc_alloc_fn, &mut [alloc_sz], "cat.dst");
                self.builder.call(&memcpy_fn, &mut [dst, lhs, la], "");
                let mut off_a = [la];
                let dst_b = self.builder.gep(&i8_ty, &dst, &mut off_a, "cat.dst_b");
                self.builder.call(&memcpy_fn, &mut [dst_b, rhs, lb], "");
                let mut off_end = [lab];
                let term_ptr = self.builder.gep(&i8_ty, &dst, &mut off_end, "cat.term");
                self.builder.store(&zero8, &term_ptr);
                return dst;
            }

            let cmp_fn = self.declare_gs_str_cmp();
            let cmp_val = self.builder.call(&cmp_fn, &mut [lhs, rhs], "strcmp");
            let zero = self.context.get_int32_const(0);
            let pred = match op {
                Eq => LLVMIntPredicate::LLVMIntEQ,
                Ne => LLVMIntPredicate::LLVMIntNE,
                Lt => LLVMIntPredicate::LLVMIntSLT,
                Le => LLVMIntPredicate::LLVMIntSLE,
                Gt => LLVMIntPredicate::LLVMIntSGT,
                Ge => LLVMIntPredicate::LLVMIntSGE,
                _ => panic!("Unsupported binary operator {:?} for String", op),
            };
            return self.builder.icmp(pred, &cmp_val, &zero, &sym);
        }

        unsafe {
            let b = self.builder.raw_ptr();
            let l = lhs.raw_ptr();
            let r = rhs.raw_ptr();
            let d = csym.as_ptr();

            let raw = match (op, type_name) {
                // Signed 32-bit Int & 64-bit Int64 arithmetic & comparisons
                (Add, "Int") | (Add, "Int64") => LLVMBuildAdd(b, l, r, d),
                (Sub, "Int") | (Sub, "Int64") => LLVMBuildSub(b, l, r, d),
                (Mul, "Int") | (Mul, "Int64") => LLVMBuildMul(b, l, r, d),
                (Div, "Int") | (Div, "Int64") => LLVMBuildSDiv(b, l, r, d),
                (Rem, "Int") | (Rem, "Int64") => LLVMBuildSRem(b, l, r, d),
                (Eq, "Int") | (Eq, "Int64") => LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntEQ, l, r, d),
                (Ne, "Int") | (Ne, "Int64") => LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntNE, l, r, d),
                (Lt, "Int") | (Lt, "Int64") => LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntSLT, l, r, d),
                (Le, "Int") | (Le, "Int64") => LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntSLE, l, r, d),
                (Gt, "Int") | (Gt, "Int64") => LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntSGT, l, r, d),
                (Ge, "Int") | (Ge, "Int64") => LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntSGE, l, r, d),

                // 64-bit Float arithmetic & ordered comparisons ("Float", NOT "Double"!)
                (Add, "Float") => LLVMBuildFAdd(b, l, r, d),
                (Sub, "Float") => LLVMBuildFSub(b, l, r, d),
                (Mul, "Float") => LLVMBuildFMul(b, l, r, d),
                (Div, "Float") => LLVMBuildFDiv(b, l, r, d),
                (Eq, "Float") => LLVMBuildFCmp(b, LLVMRealPredicate::LLVMRealOEQ, l, r, d),
                (Ne, "Float") => LLVMBuildFCmp(b, LLVMRealPredicate::LLVMRealONE, l, r, d),
                (Lt, "Float") => LLVMBuildFCmp(b, LLVMRealPredicate::LLVMRealOLT, l, r, d),
                (Le, "Float") => LLVMBuildFCmp(b, LLVMRealPredicate::LLVMRealOLE, l, r, d),
                (Gt, "Float") => LLVMBuildFCmp(b, LLVMRealPredicate::LLVMRealOGT, l, r, d),
                (Ge, "Float") => LLVMBuildFCmp(b, LLVMRealPredicate::LLVMRealOGE, l, r, d),

                // Bool & Char comparisons + bitwise/eager boolean ops
                (Eq, "Bool") | (Eq, "Char") => {
                    LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntEQ, l, r, d)
                }
                (Ne, "Bool") | (Ne, "Char") => {
                    LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntNE, l, r, d)
                }
                (Lt, "Char") => LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntULT, l, r, d),
                (Le, "Char") => LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntULE, l, r, d),
                (Gt, "Char") => LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntUGT, l, r, d),
                (Ge, "Char") => LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntUGE, l, r, d),
                (And, "Bool") => LLVMBuildAnd(b, l, r, d),
                (Or, "Bool") => LLVMBuildOr(b, l, r, d),

                // Bitwise AND / OR / XOR / shifts for Int, Int64, and Bool
                (BitAnd, "Int") | (BitAnd, "Int64") | (BitAnd, "Bool") => LLVMBuildAnd(b, l, r, d),
                (BitOr, "Int") | (BitOr, "Int64") | (BitOr, "Bool") => LLVMBuildOr(b, l, r, d),
                (BitXor, "Int") | (BitXor, "Int64") | (BitXor, "Bool") => LLVMBuildXor(b, l, r, d),
                (Shl, "Int") => LLVMBuildShl(b, l, r, d),
                (Shr, "Int") => LLVMBuildAShr(b, l, r, d),
                (Ushr, "Int") => LLVMBuildLShr(b, l, r, d),
                (Shl, "Int64") => {
                    let i64_ty = self.context.get_int64_type();
                    let cnt64 = self.builder.zext(&rhs, &i64_ty, "sh.cnt64");
                    LLVMBuildShl(b, l, cnt64.raw_ptr(), d)
                }
                (Shr, "Int64") => {
                    let i64_ty = self.context.get_int64_type();
                    let cnt64 = self.builder.zext(&rhs, &i64_ty, "sh.cnt64");
                    LLVMBuildAShr(b, l, cnt64.raw_ptr(), d)
                }
                (Ushr, "Int64") => {
                    let i64_ty = self.context.get_int64_type();
                    let cnt64 = self.builder.zext(&rhs, &i64_ty, "sh.cnt64");
                    LLVMBuildLShr(b, l, cnt64.raw_ptr(), d)
                }

                other => panic!("Binary operation {:?} not implemented", other),
            };
            LLVMValue::from_ref(raw)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mangle_roundtrip() {
        let modules = vec!["foo", "bar", "baz"];
        let name = "qux";
        let mangled = mangle_module_name(&modules, name);
        let (path, demangled) = demangle_module_name(&mangled).unwrap();
        assert_eq!(path, vec!["foo", "bar", "baz"]);
        assert_eq!(demangled, name);
    }
}
