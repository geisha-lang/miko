use std::char;
use std::ffi::{CString, NulError};
use std::io::{self, BufReader, Read, Write};
use std::ptr;
use std::collections::HashMap;
use std::iter::*;

use std::ops::Deref;

use crate::core::*;
use crate::internal::*;
use crate::types::*;
use crate::utils::*;

pub use libllvm::*;


#[derive(Debug, Clone)]
pub struct LLVMCodegen {
    pub module: LLVMModule,
    pub builder: LLVMBuilder,
    pub context: LLVMContext,
    pub passer: LLVMFunctionPassManager,
    unique: usize,
    pub adt_registry: AdtRegistry,
}

type LLVMOpBuilder<'a> = dyn Fn(LLVMBuilderRef,
                            LLVMValueRef,
                            LLVMValueRef,
                            *const ::libc::c_char)
                            -> LLVMValueRef + 'a;
pub fn get_llvm_op<'a>(op: BinOp, operand_ty: &'a Type) -> Box<LLVMOpBuilder<'a>> {
    use self::BinOp::*;
    use self::Type::*;

    // Get type name, defaulting to "Int" for type variables (polymorphic contexts)
    let type_name = match operand_ty {
        Con(ty_name) => ty_name.as_str(),
        Var(_) => "Int", // Default to Int for unresolved type variables
        _ => "Int",      // Default to Int for other cases
    };

    Box::new(move |builder, lhs, rhs, dest|
        unsafe {
            match (op, type_name) {
                (Add, "Int") => LLVMBuildAdd(builder, lhs, rhs, dest),
                (Add, "Float") => LLVMBuildFAdd(builder, lhs, rhs, dest),
                (Sub, "Int") => LLVMBuildSub(builder, lhs, rhs, dest),
                (Sub, "Float") => LLVMBuildFSub(builder, lhs, rhs, dest),
                (Mul, "Int") => LLVMBuildMul(builder, lhs, rhs, dest),
                (Mul, "Float") => LLVMBuildFMul(builder, lhs, rhs, dest),
                (Eq, "Int") => LLVMBuildICmp(builder, LLVMIntPredicate::LLVMIntEQ, lhs, rhs, dest),
                (Lt, "Int") => LLVMBuildICmp(builder, LLVMIntPredicate::LLVMIntSLT, lhs, rhs, dest),
                (Le, "Int") => LLVMBuildICmp(builder, LLVMIntPredicate::LLVMIntSLE, lhs, rhs, dest),
                (Gt, "Int") => LLVMBuildICmp(builder, LLVMIntPredicate::LLVMIntSGT, lhs, rhs, dest),
                (Ge, "Int") => LLVMBuildICmp(builder, LLVMIntPredicate::LLVMIntSGE, lhs, rhs, dest),
                (Eq, "Double") => LLVMBuildFCmp(builder, LLVMRealPredicate::LLVMRealOEQ, lhs, rhs, dest),
                (Lt, "Double") => LLVMBuildFCmp(builder, LLVMRealPredicate::LLVMRealOLT, lhs, rhs, dest),
                (Le, "Double") => LLVMBuildFCmp(builder, LLVMRealPredicate::LLVMRealOLE, lhs, rhs, dest),
                (Gt, "Double") => LLVMBuildFCmp(builder, LLVMRealPredicate::LLVMRealOGT, lhs, rhs, dest),
                (Ge, "Double") => LLVMBuildFCmp(builder, LLVMRealPredicate::LLVMRealOGE, lhs, rhs, dest),
                (And, "Bool") => LLVMBuildAnd(builder, lhs, rhs, dest),
                (Or, "Bool") => LLVMBuildOr(builder, lhs, rhs, dest),
                (Div, _) => LLVMBuildFDiv(builder, lhs, rhs, dest),
                (Rem, _) => LLVMBuildURem(builder, lhs, rhs, dest),
                // Default to Int operations for type variables
                (Add, _) => LLVMBuildAdd(builder, lhs, rhs, dest),
                (Sub, _) => LLVMBuildSub(builder, lhs, rhs, dest),
                (Mul, _) => LLVMBuildMul(builder, lhs, rhs, dest),
                (Eq, _) => LLVMBuildICmp(builder, LLVMIntPredicate::LLVMIntEQ, lhs, rhs, dest),
                (Lt, _) => LLVMBuildICmp(builder, LLVMIntPredicate::LLVMIntSLT, lhs, rhs, dest),
                (Le, _) => LLVMBuildICmp(builder, LLVMIntPredicate::LLVMIntSLE, lhs, rhs, dest),
                (Gt, _) => LLVMBuildICmp(builder, LLVMIntPredicate::LLVMIntSGT, lhs, rhs, dest),
                (Ge, _) => LLVMBuildICmp(builder, LLVMIntPredicate::LLVMIntSGE, lhs, rhs, dest),
                (And, _) => LLVMBuildAnd(builder, lhs, rhs, dest),
                (Or, _) => LLVMBuildOr(builder, lhs, rhs, dest),
                _ => unimplemented!("Binary operation {:?} not implemented for type {}", op, type_name),
            }
        })
}

pub fn is_primitive_type(t: &Type) -> bool {
    match t {
        Type::Con(n) => {
            match n.as_str() {
                "Int" | "Float" | "Char" | "String" | "Void" | "Bool" => true,
                _ => false,
            }
        }
        // Type variables are treated as primitive (i64) values
        Type::Var(_) => true,
        _ => false,
    }
}

/// Check if a type is an ADT (user-defined algebraic data type)
pub fn is_adt_type(t: &Type) -> bool {
    match t {
        Type::Con(n) => {
            match n.as_str() {
                "Int" | "Float" | "Char" | "String" | "Void" | "Bool" => false,
                _ => true,
            }
        }
        Type::Comp(_, _) => {
            // Parametric ADT like List a or Pair Int String
            // Walk to the base to find the type constructor name
            let base_name = get_comp_base_name_static(t);
            match base_name.as_str() {
                "Int" | "Float" | "Char" | "String" | "Void" | "Bool" => false,
                _ => true,
            }
        }
        _ => false,
    }
}

/// Get the base type constructor name from a Comp chain (static version)
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

/// Mangle a module-qualified name for LLVM symbols.
///
/// Format: `_G{len}M{segment}...{len}N{name}[_{type_suffix}]`
/// - `_G` prefix marks it as a Geisha symbol
/// - `M` followed by length and segment for each module path segment
/// - `N` followed by length and the final name
/// - Optional type suffix for monomorphized functions
///
/// Examples:
/// - `collections.list.length` -> `_G11Mcollections4Mlist6Nlength`
/// - `foo` (no module) -> `foo` (unchanged for backward compat)
pub fn mangle_module_name(module_path: &[&str], name: &str) -> String {
    if module_path.is_empty() {
        // No module path - keep name unchanged for backward compatibility
        // (existing code doesn't use module prefixes)
        return name.to_string();
    }

    let mut result = String::from("_G");

    // Mangle each module segment
    for segment in module_path {
        result.push_str(&segment.len().to_string());
        result.push('M');
        result.push_str(segment);
    }

    // Mangle the final name
    result.push_str(&name.len().to_string());
    result.push('N');
    result.push_str(name);

    result
}

/// Mangle a module-qualified name with type arguments (for monomorphization)
pub fn mangle_module_name_with_types(module_path: &[&str], name: &str, type_args: &[Type]) -> String {
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

/// Mangle a type for use in symbol names
fn mangle_type(ty: &Type) -> String {
    match ty {
        Type::Con(name) => name.clone(),
        Type::Var(name) => format!("T{}", name),
        Type::Arr(from, to) => format!("F{}_{}", mangle_type(from), mangle_type(to)),
        Type::Prod(l, r) => format!("P{}_{}", mangle_type(l), mangle_type(r)),
        Type::Comp(base, arg) => format!("{}{}", mangle_type(base), mangle_type(arg)),
        Type::Void => "V".to_string(),
    }
}

/// Demangle a module-qualified name (useful for debugging)
#[allow(dead_code)]
pub fn demangle_module_name(mangled: &str) -> Option<(Vec<String>, String)> {
    if !mangled.starts_with("_G") {
        // Not a mangled name
        return Some((vec![], mangled.to_string()));
    }

    let mut chars = mangled[2..].chars().peekable();
    let mut module_path = Vec::new();

    while chars.peek().is_some() {
        // Read length
        let mut len_str = String::new();
        while let Some(&c) = chars.peek() {
            if c.is_ascii_digit() {
                len_str.push(chars.next().unwrap());
            } else {
                break;
            }
        }

        let len: usize = len_str.parse().ok()?;

        // Read marker (M for module, N for name)
        let marker = chars.next()?;

        // Read the segment
        let segment: String = chars.by_ref().take(len).collect();
        if segment.len() != len {
            return None;
        }

        match marker {
            'M' => module_path.push(segment),
            'N' => {
                // Found the final name
                return Some((module_path, segment));
            }
            _ => return None,
        }
    }

    None
}

impl LLVMCodegen {
    pub fn new(name: &str, adt_registry: AdtRegistry) -> LLVMCodegen {
        let context = LLVMContext::new();
        let module = LLVMModule::in_ctx(name, &context);
        let builder = LLVMBuilder::in_ctx(&context);
        let passer = LLVMFunctionPassManager::init_for_module(&module);
        LLVMCodegen {
            module,
            context,
            builder,
            passer,
            unique: 0,
            adt_registry,
        }
    }


    pub fn new_symbol(&mut self) -> Result<CString, NulError> {
        let f = String::from("tmp") + self.unique.to_string().as_str();
        self.unique = self.unique + 1;
        CString::new(f)
    }

    pub fn new_symbol_string(&mut self) -> String {
        let f = String::from("tmp") + self.unique.to_string().as_str();
        self.unique = self.unique + 1;
        f
    }

    pub fn get_closure_type(&self) -> LLVMType {
        // closure = [i8*, i8*]
        let mem = vec![self.context.get_int8_type().get_ptr(0),
                       self.context.get_int8_type().get_ptr(0)];
        self.context.get_struct_type(&mem, false)
    }

    pub fn get_actual_cls_type(&self, fv_ty: &Vec<LLVMType>) -> LLVMType {
        let mem = vec![self.context.get_int8_type().get_ptr(0),
                       self.context.get_struct_type(&fv_ty, false)];
        self.context.get_struct_type(&mem, false)
    }

    /// Get a pointer type if the type is not primitive
    pub fn get_llvm_type_or_ptr(&self, ty: &Type) -> LLVMType {
        let ret = self.get_llvm_type(ty);
        if !is_primitive_type(ty) {
            ret.get_ptr(0)
        } else {
            ret
        }
    }

    pub fn get_main_type(&self) -> LLVMType {
        let retty = self.context.get_int32_type();
        LLVMContext::get_function_type(&retty, &vec![], false)
    }

    pub fn get_llvm_type(&self, ty: &Type) -> LLVMType {
        use self::Type::*;
        match ty {
            Con(n) => {
                match n.as_str() {

                    // Primary types
                    "Int" => self.context.get_int32_type(),
                    "Float" => self.context.get_double_type(),
                    "Char" => self.context.get_int8_type(),
                    "Bool" => self.context.get_int1_type(),
                    "String" => self.context.get_int8_type().get_ptr(0),
                    "Void" => self.context.get_void_type(),

                    // User defined types
                    t => self.gen_user_type(t),
                }
            }
            Arr(p, ret) => {
                // Type of parameters and returned value should be pointer if not primitive
                let fvs_ty = self.context.get_int8_type().get_ptr(0);
                let retty = if let Type::Arr(..) = ret.as_ref() {
                    self.get_closure_type().get_ptr(0)
                } else {
                    self.get_llvm_type_or_ptr(ret)
                };
                let psty = match p.as_ref() {
                    Type::Void => vec![],
                    _ => p.prod_to_vec()
                };
                let mut llvm_psty: Vec<_> =
                    psty.into_iter().map(|t| self.get_llvm_type_or_ptr(t)).collect();
                llvm_psty.push(fvs_ty);
                LLVMContext::get_function_type(&retty, &llvm_psty, false)
            }
            Void => self.context.get_void_type(),
            Prod(..) => {
                let tys: Vec<_> = ty.prod_to_vec()
                    .iter()
                    .map(|t| self.get_llvm_type(t))
                    .collect();
                self.context.get_struct_type(&tys, true)
            }
            Comp(_, _) => {
                // For parameterized ADTs like Maybe Int or Pair Int String,
                // collect all type arguments and find the base type constructor
                let type_args = self.collect_comp_type_args(ty);
                let base_name = self.get_comp_base_name(ty);
                self.gen_instantiated_user_type(&base_name, &type_args)
            }

            // Unresolved type variable - use i64 as universal boxed representation
            // This can hold both pointers (via inttoptr) and primitives (via zext/sext)
            Var(..) => self.context.get_int64_type(),
        }
    }

    pub fn gen_lit(&mut self, lit: &Lit) -> LLVMValue {
        use self::Lit::*;
        match lit {
            &Float(f) => self.context.get_double_const(f),
            &Int(i) => self.context.get_int32_const(i),
            &Bool(true) => self.context.get_int1_const(1),
            &Bool(false) => self.context.get_int1_const(0),
            // TODO: String represent
            &Str(ref s) => self.context.get_const_string(s.as_str()),
        }
    }

    pub fn gen_user_type(&self, tyname: &str) -> LLVMType {
        if let Some(adt_info) = self.adt_registry.get(tyname) {
            self.get_adt_llvm_type(adt_info)
        } else {
            panic!("Unknown user type: {}", tyname)
        }
    }

    /// Collect all type arguments from a Comp type chain
    /// E.g., Comp(Comp(Map, k), v) -> [k, v]
    fn collect_comp_type_args(&self, ty: &Type) -> Vec<Type> {
        let mut args = Vec::new();
        let mut current = ty;
        while let Type::Comp(base, arg) = current {
            args.insert(0, arg.as_ref().clone());
            current = base.as_ref();
        }
        args
    }

    /// Get the base type constructor name from a Comp chain
    /// E.g., Comp(Comp(Map, k), v) -> "Map"
    fn get_comp_base_name(&self, ty: &Type) -> String {
        let mut current = ty;
        while let Type::Comp(base, _) = current {
            current = base.as_ref();
        }
        match current {
            Type::Con(name) => name.clone(),
            _ => panic!("Expected type constructor at base of Comp chain, got {:?}", current),
        }
    }

    /// Generate LLVM type for an instantiated ADT (e.g., Maybe Int)
    pub fn gen_instantiated_user_type(&self, tyname: &str, type_args: &[Type]) -> LLVMType {
        if let Some(adt_info) = self.adt_registry.get(tyname) {
            self.get_instantiated_adt_llvm_type(adt_info, type_args)
        } else {
            panic!("Unknown user type: {}", tyname)
        }
    }

    /// Generate LLVM type for an ADT with concrete type arguments
    fn get_instantiated_adt_llvm_type(&self, adt_info: &AdtInfo, type_args: &[Type]) -> LLVMType {
        let tag_ty = self.context.get_int32_type();

        // Build substitution from type parameters to concrete types
        let subst: std::collections::HashMap<String, Type> = adt_info.type_params.iter()
            .cloned()
            .zip(type_args.iter().cloned())
            .collect();


        // Find variant with most fields for payload sizing
        let max_fields = adt_info.variants.iter()
            .filter(|v| !v.field_types.is_empty())
            .max_by_key(|v| v.field_types.len());

        match max_fields {
            Some(variant) => {
                // Substitute type parameters in field types
                let instantiated_field_types: Vec<Type> = variant.field_types.iter()
                    .map(|t| self.substitute_type(t, &subst))
                    .collect();
                let payload_ty = self.get_instantiated_payload_llvm_type(&instantiated_field_types);
                self.context.get_struct_type(&vec![tag_ty, payload_ty], false)
            }
            None => {
                // All unit variants
                self.context.get_struct_type(&vec![tag_ty], false)
            }
        }
    }

    /// Substitute type parameters in a type
    fn substitute_type(&self, ty: &Type, subst: &std::collections::HashMap<String, Type>) -> Type {
        match ty {
            Type::Var(name) => {
                if let Some(concrete) = subst.get(name) {
                    concrete.clone()
                } else {
                    ty.clone()
                }
            }
            Type::Con(_) | Type::Void => ty.clone(),
            Type::Arr(p, r) => Type::Arr(
                Box::new(self.substitute_type(p, subst)),
                Box::new(self.substitute_type(r, subst)),
            ),
            Type::Prod(l, r) => Type::Prod(
                Box::new(self.substitute_type(l, subst)),
                Box::new(self.substitute_type(r, subst)),
            ),
            Type::Comp(base, arg) => Type::Comp(
                Box::new(self.substitute_type(base, subst)),
                Box::new(self.substitute_type(arg, subst)),
            ),
        }
    }

    /// Get LLVM type for an instantiated payload (with concrete types, no type variables)
    fn get_instantiated_payload_llvm_type(&self, field_types: &[Type]) -> LLVMType {
        let llvm_types: Vec<LLVMType> = field_types.iter()
            .map(|t| self.get_instantiated_field_type(t))
            .collect();
        self.context.get_struct_type(&llvm_types, false)
    }

    fn get_adt_llvm_type(&self, adt_info: &AdtInfo) -> LLVMType {
        let tag_ty = self.context.get_int32_type();

        // Find variant with most fields for payload sizing
        let max_fields = adt_info.variants.iter()
            .filter(|v| !v.field_types.is_empty())
            .max_by_key(|v| v.field_types.len());

        match max_fields {
            Some(variant) => {
                let payload_ty = self.get_payload_llvm_type(&variant.field_types);
                self.context.get_struct_type(&vec![tag_ty, payload_ty], false)
            }
            None => {
                // All unit variants
                self.context.get_struct_type(&vec![tag_ty], false)
            }
        }
    }

    fn get_payload_llvm_type(&self, field_types: &[Type]) -> LLVMType {
        let llvm_types: Vec<LLVMType> = field_types.iter()
            .map(|t| self.get_field_llvm_type(t))
            .collect();
        self.context.get_struct_type(&llvm_types, false)
    }

    fn get_field_llvm_type(&self, ty: &Type) -> LLVMType {
        match ty {
            Type::Con(name) => match name.as_str() {
                "Int" => self.context.get_int32_type(),
                "Float" => self.context.get_double_type(),
                "Char" => self.context.get_int8_type(),
                "Bool" => self.context.get_int1_type(),
                "String" => self.context.get_int8_type().get_ptr(0),
                "Void" => self.context.get_void_type(),
                // Other ADTs: use opaque pointer to avoid infinite recursion
                // ADT values are always stored/passed as pointers
                _ => self.context.get_int8_type().get_ptr(0),
            },
            Type::Arr(..) => self.get_closure_type().get_ptr(0),
            Type::Comp(_base, _) => {
                // Polymorphic type like List a - use opaque pointer
                // ADT values are always stored/passed as pointers
                self.context.get_int8_type().get_ptr(0)
            }
            // Type variable: use i64 as a universal representation
            // that can hold both pointers (via inttoptr) and primitives (via zext/sext)
            Type::Var(_) => self.context.get_int64_type(),
            Type::Prod(..) => {
                let tys: Vec<_> = ty.prod_to_vec().iter()
                    .map(|t| self.get_field_llvm_type(t))
                    .collect();
                self.context.get_struct_type(&tys, false)
            }
            Type::Void => self.context.get_void_type(),
        }
    }

    /// Get the i64 type for storing polymorphic values
    pub fn get_int64_type(&self) -> LLVMType {
        self.context.get_int64_type()
    }

    /// Get LLVM type for a concrete instantiated field type.
    /// Unlike get_field_llvm_type, this handles type variables with a warning
    /// since they should have been instantiated by the type inference phase.
    pub fn get_instantiated_field_type(&self, ty: &Type) -> LLVMType {
        match ty {
            Type::Con(name) => match name.as_str() {
                "Int" => self.context.get_int32_type(),
                "Float" => self.context.get_double_type(),
                "Char" => self.context.get_int8_type(),
                "Bool" => self.context.get_int1_type(),
                "String" => self.context.get_int8_type().get_ptr(0),
                "Void" => self.context.get_void_type(),
                // ADT pointer
                _ => self.context.get_int8_type().get_ptr(0),
            },
            Type::Comp(_, _) => {
                // Polymorphic ADT type - use opaque pointer
                self.context.get_int8_type().get_ptr(0)
            }
            Type::Arr(..) => self.get_closure_type().get_ptr(0),
            Type::Var(_v) => {
                // Type variables in polymorphic functions use i64 as universal representation
                // This is expected when monomorphization hasn't specialized the function
                // Debug: eprintln!("Warning: unresolved type var {} at codegen", v);
                self.context.get_int64_type()
            }
            Type::Prod(..) => {
                let tys: Vec<_> = ty.prod_to_vec().iter()
                    .map(|t| self.get_instantiated_field_type(t))
                    .collect();
                self.context.get_struct_type(&tys, false)
            }
            Type::Void => self.context.get_void_type(),
        }
    }

    /// Get the LLVM type for an ADT (Algebraic Data Type)
    /// ADTs are represented as a tagged union: { i32 tag, payload }
    /// where payload is the largest variant's fields or an opaque byte array
    pub fn get_adt_type(&self, max_payload_size: usize) -> LLVMType {
        // For now, use a simple representation: { i32 tag, [max_payload_size x i8] }
        // This is a simple approach; a more sophisticated one would compute exact union sizes
        let tag_ty = self.context.get_int32_type();
        if max_payload_size == 0 {
            // Unit-only ADT
            self.context.get_struct_type(&vec![tag_ty], false)
        } else {
            // ADT with payload - use an array of bytes as a generic payload
            let payload_ty = unsafe {
                LLVMType::from_ref(llvm_sys::core::LLVMArrayType(
                    self.context.get_int8_type().raw_ptr(),
                    max_payload_size as u32
                ))
            };
            self.context.get_struct_type(&vec![tag_ty, payload_ty], false)
        }
    }

    /// Create a struct type for a specific ADT variant's payload
    pub fn get_variant_payload_type(&self, field_types: &[&Type]) -> LLVMType {
        if field_types.is_empty() {
            // No payload - return void type
            self.context.get_void_type()
        } else {
            let llvm_types: Vec<LLVMType> = field_types.iter()
                .map(|t| self.get_llvm_type(t))
                .collect();
            self.context.get_struct_type(&llvm_types, false)
        }
    }

    pub fn get_or_add_function(&self, fun_name: &str, fty: &Type) -> LLVMFunction {
        match self.module.get_function(fun_name) {
            Some(f) => f,
            None => {
                let ty = self.get_llvm_type(fty);
                self.module.add_function(fun_name, &ty)
            }
        }
    }

    /// Declare gc_init(size_t) -> void
    pub fn declare_gc_init(&self) -> LLVMFunction {
        if let Some(f) = self.module.get_function("gc_init") {
            return f;
        }
        let size_ty = self.context.get_int64_type(); // size_t
        let void_ty = self.context.get_void_type();
        let fn_ty = LLVMContext::get_function_type(&void_ty, &vec![size_ty], false);
        self.module.add_function("gc_init", &fn_ty)
    }

    /// Declare gc_alloc(size_t) -> i8*
    pub fn declare_gc_alloc(&self) -> LLVMFunction {
        if let Some(f) = self.module.get_function("gc_alloc") {
            return f;
        }
        let size_ty = self.context.get_int64_type(); // size_t
        let ptr_ty = self.context.get_int8_type().get_ptr(0); // i8*
        let fn_ty = LLVMContext::get_function_type(&ptr_ty, &vec![size_ty], false);
        self.module.add_function("gc_alloc", &fn_ty)
    }

    /// Get the size in bytes of an LLVM type
    pub fn get_type_size(&self, ty: &LLVMType) -> u64 {
        unsafe {
            let target_data = llvm_sys::target::LLVMGetModuleDataLayout(self.module.raw_ptr());
            llvm_sys::target::LLVMABISizeOfType(target_data, ty.raw_ptr())
        }
    }


    pub fn create_entry_block_alloca(&self,
                                     fun: &LLVMFunction,
                                     var_name: &str,
                                     ty: &Type)
                                     -> LLVMValue {
        let builder = LLVMBuilder::in_ctx(&self.context);
        let block = fun.get_entry_basic_block();
        let fi = block.get_first_instr();
        let llvm_ty = match ty {
            Type::Arr(..) => self.get_closure_type().get_ptr(0),
            _ if is_adt_type(ty) => self.get_llvm_type(ty).get_ptr(0),
            _ => self.get_llvm_type(ty)
        };
        builder.set_position(&block, &fi);
        builder.alloca(&llvm_ty, var_name)
    }

    pub fn bin_operator(&mut self,
                        op: BinOp,
                        lhs: LLVMValue,
                        rhs: LLVMValue,
                        operand_ty: &Type)
                        -> LLVMValue {
        let fun = get_llvm_op(op, operand_ty);
        LLVMValue::from_ref(fun(self.builder.raw_ptr(),
                                lhs.raw_ptr(),
                                rhs.raw_ptr(),
                                self.new_symbol().unwrap().into_raw()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mangle_no_module() {
        // Without module path, name should be unchanged
        let result = mangle_module_name(&[], "foo");
        assert_eq!(result, "foo");
    }

    #[test]
    fn test_mangle_single_module() {
        // Single module path
        let result = mangle_module_name(&["utils"], "clamp");
        assert_eq!(result, "_G5Mutils5Nclamp");
    }

    #[test]
    fn test_mangle_multiple_modules() {
        // Multiple module segments
        let result = mangle_module_name(&["collections", "list"], "length");
        assert_eq!(result, "_G11Mcollections4Mlist6Nlength");
    }

    #[test]
    fn test_demangle_simple() {
        // Test demangling a simple module path
        let mangled = "_G5Mutils5Nclamp";
        let result = demangle_module_name(mangled);
        assert!(result.is_some());
        let (path, name) = result.unwrap();
        assert_eq!(path, vec!["utils"]);
        assert_eq!(name, "clamp");
    }

    #[test]
    fn test_demangle_multiple_modules() {
        // Test demangling multiple module segments
        let mangled = "_G11Mcollections4Mlist6Nlength";
        let result = demangle_module_name(mangled);
        assert!(result.is_some());
        let (path, name) = result.unwrap();
        assert_eq!(path, vec!["collections", "list"]);
        assert_eq!(name, "length");
    }

    #[test]
    fn test_demangle_non_mangled() {
        // Non-mangled names should return as-is
        let result = demangle_module_name("simple_name");
        assert!(result.is_some());
        let (path, name) = result.unwrap();
        assert!(path.is_empty());
        assert_eq!(name, "simple_name");
    }

    #[test]
    fn test_mangle_roundtrip() {
        // Verify mangle/demangle roundtrip
        let modules = vec!["foo", "bar", "baz"];
        let name = "qux";
        let mangled = mangle_module_name(&modules, name);
        let demangled = demangle_module_name(&mangled);
        assert!(demangled.is_some());
        let (path, demangled_name) = demangled.unwrap();
        assert_eq!(path, modules.iter().map(|s| s.to_string()).collect::<Vec<_>>());
        assert_eq!(demangled_name, name);
    }

    #[test]
    fn test_mangle_with_types() {
        // Test mangling with type arguments
        let result = mangle_module_name_with_types(
            &["collections"],
            "map",
            &[Type::Con("Int".to_string()), Type::Con("String".to_string())]
        );
        assert_eq!(result, "_G11Mcollections3Nmap_Int_String");
    }
}
