use remusys_ir::typing::{
    context::TypeContext,
    id::ValTypeID,
    types::{ArrayTypeRef, FloatTypeKind, FuncTypeRef},
};
use remusys_lang::typing::AstType;

#[derive(Debug)]
pub(super) enum TypeInfo {
    Trivial(ValTypeID),
    FixArray(ArrayTypeRef /* pointee */),
    DynArray(ValTypeID /* element */),
    Func(FuncTypeRef /* pointee */),
}

impl TypeInfo {
    pub fn get_value_type_id(&self) -> ValTypeID {
        match self {
            TypeInfo::Trivial(val_type) => val_type.clone(),
            TypeInfo::FixArray(_) => ValTypeID::Ptr,
            TypeInfo::DynArray(_) => ValTypeID::Ptr,
            TypeInfo::Func(_) => ValTypeID::Ptr,
        }
    }
    pub fn get_alloca_data_type(&self) -> Option<ValTypeID> {
        match self {
            TypeInfo::Trivial(ValTypeID::Void) => None,
            TypeInfo::Trivial(val_type) => Some(val_type.clone()),
            TypeInfo::FixArray(array_type) => Some(ValTypeID::Array(array_type.clone())),
            TypeInfo::DynArray(_) => None,
            TypeInfo::Func(_) => None, // Functions are not data types
        }
    }
    pub fn can_alloca(&self) -> bool {
        self.get_alloca_data_type().is_some()
    }
    pub fn is_value_semantic(&self) -> bool {
        match self {
            TypeInfo::Trivial(ValTypeID::Void) => false,
            TypeInfo::Trivial(_) => true,

            // SysY 和 C 一样, 不论什么数组类型都会退化成指针, 因此是引用语义.
            TypeInfo::FixArray(_) => false,
            TypeInfo::DynArray(_) => false,
            TypeInfo::Func(_) => false, // Functions are not value semantic
        }
    }
    pub fn is_pointer_semantic(&self) -> bool {
        match self {
            TypeInfo::Trivial(ValTypeID::Void) => false,
            TypeInfo::Trivial(_) => false,
            TypeInfo::FixArray(_) => true,
            TypeInfo::DynArray(_) => true,
            TypeInfo::Func(_) => true, // Functions are pointer semantic
        }
    }

    pub fn new(ast_type: &AstType, type_ctx: &TypeContext) -> Self {
        match ast_type {
            AstType::Void => TypeInfo::Trivial(ValTypeID::Void),
            AstType::Bool => TypeInfo::Trivial(ValTypeID::new_boolean()),
            AstType::Int => TypeInfo::Trivial(ValTypeID::Int(32)),
            AstType::Float => TypeInfo::Trivial(ValTypeID::Float(FloatTypeKind::Ieee32)),
            AstType::Str => TypeInfo::DynArray(ValTypeID::Int(8)),
            AstType::FixedArray(farr) => {
                let elem_type_info = TypeInfo::new(&farr.elemty, type_ctx);
                let length = farr.nelems as usize;
                let elem_type = match elem_type_info.get_alloca_data_type() {
                    Some(val_type) => val_type,
                    None => panic!("Fixed array element type must be allocatable"),
                };
                let ir_type = type_ctx.make_array_type(length, elem_type);
                TypeInfo::FixArray(ir_type)
            },
            AstType::DynArray(ast_type) => {
                let elem_type_info = TypeInfo::new(ast_type, type_ctx);
                let elem_type = match elem_type_info.get_alloca_data_type() {
                    Some(val_type) => val_type,
                    None => panic!("Dynamic array element type must be allocatable"),
                };
                TypeInfo::DynArray(elem_type)
            },
        }
    }
}
