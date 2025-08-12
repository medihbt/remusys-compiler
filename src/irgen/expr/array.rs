use super::*;
use remusys_ir::{
    base::{APInt, INullableValue},
    ir::ISubValueSSA,
    typing::TypeContext,
};

pub(super) struct ArrayInitState {
    /// `len()` 表示当前维度要解析的是哪个元素, `capacity()` 表示当前维度的表达式数量
    ir_building: Vec<IRValue>,

    /// AST 层级, 0 表示最外层, 1 表示第一维, 2 表示第二维, 以此类推
    ast_layer: usize,

    /// 当前状态下数组表达式的最终索引. 这个索引一般是跳着走的, 每一跳的大小是下一级
    /// 数组类型的最终元素个数。
    /// 例如: 对于类型为 `[[[i32; 3], 2]; 4]` 的数组初始化表达式:
    ///
    /// - `ast_layer = 0` 时, `ast_final_index_curr` 每一跳的大小是 6 = 3 * 2.
    /// - `ast_layer = 1` 时, `ast_final_index_curr` 每一跳的大小是 3.
    /// - `ast_layer = 2` 时, 理论上 `ast_final_index_curr` 每一跳的大小是 1;
    ///   但实际上在 `ast_layer = 2` 时就检测到元素类型不是数组了,
    ///   会进入其他的表达式求值分支, 所以 `ast_final_index_curr` 在 `ast_layer = 2` 时
    ///   不会被使用。
    ast_final_index_curr: usize,

    /// 当前状态下数组表达式从生到死的最终索引范围.
    /// 例如: 对于类型为 `[[[i32; 3], 2]; 4]` 的数组初始化表达式:
    ///
    /// - `ast_layer = 0` 时, `ast_final_index_range` 是 `0..24` (24 = 3 * 2 * 4).
    /// - `ast_layer = 1` 时, `ast_final_index_range` 是 `0..6`*step (6 = 3 * 2).
    /// - `ast_layer = 2` 时, `ast_final_index_range` 是 `0..3`*step (3 = 3).
    ///   但实际上在 `ast_layer = 2` 时就检测到元素类型不是数组了,
    ///   会进入其他的表达式求值分支, 所以 `ast_final_index_range` 在 `ast_layer = 2` 时
    ///   不会被使用.
    ast_final_index_range: Range<usize>,
}
// type ArrayInitStateStack = Vec<ArrayInitState>;
pub(super) struct ArrayInitStateStack<'a> {
    stats: Vec<ArrayInitState>,
    types: Box<[IRTypeID]>,
    array: &'a ArrayInitList,
}

impl<'a> ArrayInitStateStack<'a> {
    pub fn from_array(array: &'a ArrayInitList, type_ctx: &TypeContext) -> Self {
        let mut stats = Vec::with_capacity(array.n_dimensions() + 1);
        stats.push(ArrayInitState {
            ir_building: Vec::with_capacity(array.dimensions[0]),
            ast_layer: 0,
            ast_final_index_curr: 0,
            ast_final_index_range: 0..array.n_final_elements(),
        });
        let types = array
            .type_levels
            .iter()
            .map(|ty| TypeInfo::new(ty, type_ctx).get_alloca_data_type().unwrap());
        Self {
            stats,
            types: Box::from_iter(types),
            array,
        }
    }
    pub fn push(&mut self, state: ArrayInitState) {
        self.stats.push(state);
    }
    pub fn pop(&mut self) -> Option<ArrayInitState> {
        self.stats.pop()
    }

    pub fn build_ir_const(mut self, module: &IRModule) -> IRExprRef {
        let mut ret = IRExprRef::new_null();
        while let Some(stat) = self.pop() {
            let ArrayInitState {
                mut ir_building,
                ast_layer,
                ast_final_index_curr,
                ast_final_index_range,
            } = stat;

            if ret.is_nonnull() {
                // 如果当前状态已经有了 IR 表达式, 则将其添加到当前层级的构建中
                ir_building.push(IRValue::ConstExpr(ret));
            }

            let this_level_dimension = self.array.dimensions[ast_layer];
            let this_level_build_idx = ir_building.len();
            let this_level_type = match self.types[ast_layer] {
                IRTypeID::Array(ref arr_ty) => arr_ty.clone(),
                _ => panic!(
                    "Expected an array type at layer {}, got: {:?}",
                    ast_layer, self.types[ast_layer]
                ),
            };
            assert_ne!(
                this_level_dimension, 0,
                "Array initialization state should not be empty"
            );
            ret = if ast_layer + 1 == self.array.n_dimensions() {
                // 最后一层的数组元素类型是基本类型
                let elem_type = self.types.last().unwrap();
                for idx in ast_final_index_range {
                    let ast_exp = &self.array.final_elems[idx];
                    let ir_const = Self::ast_data_make_const(ast_exp, elem_type.clone());
                    ir_building.push(ir_const);
                }
                let ir_expr_data =
                    IRExprData::Array(IRArrayExpr::from_vec(this_level_type, ir_building));
                IRExprRef::from_alloc(&mut module.borrow_allocs_mut().exprs, ir_expr_data)
            } else if this_level_build_idx == this_level_dimension {
                // 当前层级的数组表达式已经构建完毕, 需要将其转换为 IR 表达式
                let ir_expr_data =
                    IRExprData::Array(IRArrayExpr::from_vec(this_level_type, ir_building));
                IRExprRef::from_alloc(&mut module.borrow_allocs_mut().exprs, ir_expr_data)
            } else if this_level_build_idx < this_level_dimension {
                // 当前层级的数组表达式还未构建完毕, 需要继续处理下一层级
                let child_layer = ast_layer + 1;
                let child_layer_dimension = self.array.dimensions[child_layer];
                let child_layer_step = self.array.n_final_elems[child_layer];
                let ast_final_index_next = ast_final_index_curr + child_layer_step;
                let child_layer_range = Range {
                    start: ast_final_index_curr,
                    end: ast_final_index_next,
                };
                // 更新自己的状态并压入栈中
                self.push(ArrayInitState {
                    ir_building,
                    ast_layer,
                    ast_final_index_curr: ast_final_index_next,
                    ast_final_index_range,
                });
                // 压入下一层级的状态
                self.push(ArrayInitState {
                    ir_building: Vec::with_capacity(child_layer_dimension),
                    ast_layer: child_layer,
                    ast_final_index_curr,
                    ast_final_index_range: child_layer_range,
                });
                IRExprRef::new_null() // 继续处理下一层级
            } else {
                panic!(
                    "Array initialization state element overflow at layer {}, expected {} elements, got {}",
                    ast_layer, this_level_dimension, this_level_build_idx
                );
            }
        }
        ret
    }

    fn ast_data_make_const(ast_exp: &AstExpr, type_req: IRTypeID) -> IRValue {
        assert!(
            matches!(type_req, IRTypeID::Int(_) | IRTypeID::Float(_)),
            "Expected a numeric type for constant expression, got: {:?}",
            type_req
        );

        match ast_exp {
            AstExpr::None => match type_req {
                IRTypeID::Int(nbits) => APInt::new(0, nbits).into(),
                IRTypeID::Float(kind) => IRConstData::Float(kind, 0.0).into_ir(),

                IRTypeID::Ptr
                | IRTypeID::Array(_)
                | IRTypeID::Struct(_)
                | IRTypeID::StructAlias(_) => IRValue::new_zero(type_req),

                IRTypeID::Void | IRTypeID::Func(_) => {
                    panic!("Cannot create constant expression for un-instantiable type")
                }
            },
            AstExpr::Literal(AstLiteral::Int(i)) => match type_req {
                IRTypeID::Int(nbits) => APInt::new(*i, nbits).into(),
                _ => panic!(
                    "Expected an integer type for constant expression, got: {:?}",
                    type_req
                ),
            },
            AstExpr::Literal(AstLiteral::Float(f)) => match type_req {
                IRTypeID::Float(kind) => IRConstData::Float(kind, *f as f64).into_ir(),
                _ => panic!(
                    "Expected a float type for constant expression, got: {:?}",
                    type_req
                ),
            },
            _ => {
                panic!(
                    "Expected a literal expression for constant initialization, got: {:?}",
                    ast_exp
                );
            }
        }
    }
}

#[cfg(test)]
mod testing {
    use std::rc::Rc;

    use remusys_ir::{
        ir::{ConstExprRef, IRWriter, ISubValueSSA},
        typing::ArchInfo,
    };
    use remusys_lang::typing::{AstType, FixedArrayType};

    use super::*;

    #[test]
    fn build_array_init_const() {
        let type_ctx = TypeContext::new_rc(ArchInfo::new_host());
        // Example: `let arr: [[i32; 3]; 2] = [[1, 2, 3], [4, 5, 6]]`
        let intty = AstType::Int;
        let aint3ty = AstType::FixedArray(Rc::new(FixedArrayType {
            elemty: intty.clone(),
            nelems: 3,
        }));
        let aaint32ty = AstType::FixedArray(Rc::new(FixedArrayType {
            elemty: aint3ty.clone(),
            nelems: 2,
        }));
        let aaaint321ty = AstType::FixedArray(Rc::new(FixedArrayType {
            elemty: aaint32ty.clone(),
            nelems: 1,
        }));
        let arrexp = ArrayInitList {
            final_elems: vec![
                AstExpr::Literal(AstLiteral::Int(1)),
                AstExpr::Literal(AstLiteral::Int(2)),
                AstExpr::Literal(AstLiteral::Int(3)),
                AstExpr::Literal(AstLiteral::Int(4)),
                AstExpr::Literal(AstLiteral::Int(5)),
                AstExpr::Literal(AstLiteral::Int(6)),
            ]
            .into_boxed_slice(),
            type_levels: vec![aaaint321ty, aaint32ty, aint3ty, intty].into_boxed_slice(),
            dimensions: vec![1, 2, 3, 1].into_boxed_slice(),
            n_final_elems: vec![6, 6, 3, 1].into_boxed_slice(),
        };
        let module = IRModule::new("test".to_string(), type_ctx);
        let expr_ref = translate_array_init_list_const(&module, &arrexp);
        write_ir_expr(&module, &mut std::io::stdout(), expr_ref);
    }

    fn write_ir_expr(module: &IRModule, out: &mut dyn std::io::Write, expr_ref: ConstExprRef) {
        let writer = IRWriter::from_module(out, module);
        expr_ref.fmt_ir(&writer).unwrap();
    }
}
