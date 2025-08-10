use std::{collections::HashMap, hash::Hash, ops::Deref, rc::Rc};

use remusys_ir::{
    base::{APInt, INullableValue, SlabRef},
    ir::{
        BlockRef as IRBlockRef, CmpCond, ConstData, FuncRef, GlobalData, GlobalRef, IRBuilder,
        IRBuilderFocus, ISubGlobal, ISubValueSSA, Linkage, Module as IRModule, Opcode, ValueSSA,
        inst::{InstData, InstRef},
    },
    typing::{
        context::{PlatformPolicy, TypeContext},
        id::ValTypeID,
        types::FloatTypeKind,
    },
};
use remusys_lang::{
    ast::{
        AstModule,
        expr::{
            Expr as AstExpr,
            binop::BinExp,
            call::Call as AstCall,
            index::ArrayIndex,
            literal::Literal,
            unaryop::{ImplicitCast, UnaryExp},
        },
        operator::Operator,
        stmt::{
            Stmt,
            block::Block as AstBlock,
            decl::{Function as AstFunc, VarDecl, Variable},
            ifstmt::IfStmt,
            whilestmt::WhileStmt,
        },
    },
    typing::AstType,
    util::MultiLevelIndex,
};
use symbol::SymbolMap;
use typing::TypeInfo;

pub mod expr;
pub mod symbol;
pub mod typing;

#[derive(Debug, Clone)]
struct WhileLoopRef(Rc<WhileStmt>);
impl Hash for WhileLoopRef {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Rc::as_ptr(&self.0).hash(state);
    }
}
impl PartialEq for WhileLoopRef {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}
impl Eq for WhileLoopRef {}
impl Deref for WhileLoopRef {
    type Target = WhileStmt;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

struct WhileLoopInfo {
    entry_block: IRBlockRef,
    exit_block: IRBlockRef,
}

struct FunctionState {
    ir_func: GlobalRef,
    func_name: String,
    inst_before_last_alloca: InstRef,
    curr_block: IRBlockRef,
    while_loop_map: HashMap<WhileLoopRef, WhileLoopInfo>,
}

pub struct IRTranslator {
    pub ir_builder: IRBuilder,
    pub type_ctx: Rc<TypeContext>,
    pub symbols: SymbolMap,
    pub strings: HashMap<String, GlobalRef>,
    ast_intrinsic_funcs: AstIntrinsicFuncs,
    ir_stdlib_funcs: IRStdLibFuncs,
}

struct AstIntrinsicFuncs {
    start_time: GlobalRef,
    stop_time: GlobalRef,
}

pub struct IRStdLibFuncs {
    pub memset: GlobalRef,
    pub memcpy: GlobalRef,
}

impl Default for AstIntrinsicFuncs {
    fn default() -> Self {
        Self {
            start_time: GlobalRef::new_null(),
            stop_time: GlobalRef::new_null(),
        }
    }
}

impl IRTranslator {
    pub fn new(ast_module: &AstModule, platform_policy: PlatformPolicy) -> Self {
        let type_ctx = TypeContext::new_rc(platform_policy);
        let ir_builder = IRBuilder::new(IRModule::new(ast_module.file.clone(), type_ctx.clone()));
        let symbols = SymbolMap::new();

        let mut ret = Self {
            ir_builder,
            type_ctx,
            symbols,
            strings: HashMap::new(),
            ast_intrinsic_funcs: AstIntrinsicFuncs::default(),
            ir_stdlib_funcs: IRStdLibFuncs {
                memset: GlobalRef::new_null(),
                memcpy: GlobalRef::new_null(),
            },
        };
        ret.add_missing_timer_functions();
        ret
    }

    /// 选择 ARM64 赛道.
    pub fn new_competition(ast_module: &AstModule) -> Self {
        Self::new(
            ast_module,
            PlatformPolicy {
                ptr_nbits: 64,
                reg_nbits: 64,
            },
        )
    }

    fn add_missing_timer_functions(&mut self) {
        // type: Fn(i32) -> ()
        let timer_func_ty =
            self.type_ctx
                .make_func_type(&[ValTypeID::Int(32)], ValTypeID::Void, false);

        // void _sysy_starttime(int lineno);
        let starttime_func = self
            .ir_builder
            .declare_function("_sysy_starttime", timer_func_ty)
            .unwrap();

        // void _sysy_stoptime(int lineno);
        let stop_time_func = self
            .ir_builder
            .declare_function("_sysy_stoptime", timer_func_ty)
            .unwrap();

        self.ast_intrinsic_funcs.start_time = starttime_func;
        self.ast_intrinsic_funcs.stop_time = stop_time_func;
    }
}

impl IRTranslator {
    pub fn translate(mut self, ast_module: &AstModule) -> Rc<IRModule> {
        // 添加标准库函数
        self.add_ir_extra_stdlib();

        for gdef in ast_module.global_defs.iter() {
            self.translate_global_def(gdef);
        }
        Rc::new(self.ir_builder.module)
    }

    /// 在 IR 层额外添加下面的标准库函数:
    ///
    /// * `memset`: `void memset(void *ptr, int value, int size);`
    /// * `memcpy`: `void memcpy(void *dest, const void *src, int size);`
    fn add_ir_extra_stdlib(&mut self) {
        let memset_func = {
            let memset_arg_ty = [
                ValTypeID::Ptr,     // ptr
                ValTypeID::Int(32), // value
                ValTypeID::Int(32), // size
            ];
            let memset_func_ty =
                self.type_ctx
                    .make_func_type(&memset_arg_ty, ValTypeID::Void, false);
            self.ir_builder
                .declare_function("memset", memset_func_ty)
                .expect("Failed to declare memset function")
        };
        let memcpy_func = {
            let memcpy_arg_ty = [
                ValTypeID::Ptr,     // dest
                ValTypeID::Ptr,     // src
                ValTypeID::Int(32), // size
            ];
            let memcpy_func_ty =
                self.type_ctx
                    .make_func_type(&memcpy_arg_ty, ValTypeID::Void, false);
            self.ir_builder
                .declare_function("memcpy", memcpy_func_ty)
                .expect("Failed to declare memcpy function")
        };
        self.ir_stdlib_funcs = IRStdLibFuncs {
            memset: memset_func,
            memcpy: memcpy_func,
        };
    }

    fn translate_global_def(&mut self, global_decl: &Stmt) {
        match global_decl {
            Stmt::VarDecl(var_decl) => {
                self.translate_global_vardecl(var_decl, None);
            }
            Stmt::FuncDecl(ast_func) => self.translate_func_decl(ast_func),
            Stmt::UnresolvedVarDecl(_) => panic!(
                "IR generator input should be a normalized SST, but got an unresolved variable declaration"
            ),
            _ => panic!("Statement NOT a global declaration"),
        }
    }
    fn translate_global_vardecl(&mut self, global_vars: &VarDecl, in_function: Option<&str>) {
        for var in global_vars.defs.iter() {
            let name = if let Some(func_name) = in_function {
                format!("_static.{}.{}", func_name, var.name)
            } else {
                var.name.clone()
            };
            let var_typeinfo = TypeInfo::new(&var.var_type, &self.type_ctx);
            let var_content_ty = var_typeinfo
                .get_alloca_data_type()
                .expect("Global variable type must be allocatable");
            let ir_global = {
                let initval = if let AstExpr::None = &var.initval {
                    ValueSSA::ConstData(ConstData::Zero(var_content_ty.clone()))
                } else {
                    // Translate the initializer expression
                    self.translate_expr_force_const(&var.initval, var_content_ty.clone())
                };
                let ir_val = self
                    .ir_builder
                    .define_var(name.as_str(), var.is_const(), var_content_ty, initval)
                    .expect("Failed to define global variable");
                let allocs = self.ir_builder.module.borrow_allocs();
                let GlobalData::Var(v) = ir_val.to_data(&allocs.globals) else {
                    panic!("Expected a global variable, but got: {:?}", ir_val);
                };
                v.set_linkage(Linkage::Private);
                ir_val
            }
            .unwrap();
            // Insert the global variable into the symbol map
            self.symbols.insert_variable(
                var.clone(),
                ValueSSA::Global(ir_global),
                // TypeInfo::LValue(var_content_ty),
                match var_content_ty {
                    ValTypeID::Ptr
                    | ValTypeID::Int(_)
                    | ValTypeID::Float(_)
                    | ValTypeID::Struct(_)
                    | ValTypeID::StructAlias(_) => TypeInfo::LValue(var_content_ty),
                    ValTypeID::Array(arr) => TypeInfo::FixArray(arr),
                    ValTypeID::Void => panic!("Global variable {name} cannot be of type Void"),
                    ValTypeID::Func(_) => {
                        panic!("Global variable {name} cannot be of type Function")
                    }
                },
            );
        }
    }
    fn translate_func_decl(&mut self, ast_func: &Rc<AstFunc>) {
        if ast_func.is_intrinsic() {
            // Intrinsic function, no need to define it
            return;
        }
        let name = ast_func.name.as_str();
        let ret_ty = {
            let ret_typeinfo = TypeInfo::new(&ast_func.ret_type, &self.type_ctx);
            match ret_typeinfo {
                TypeInfo::RValue(val_type_id) => val_type_id,
                _ => panic!(
                    "Global variable type must be a trivial type, but got: {:?}",
                    ret_typeinfo
                ),
            }
        };
        let arg_ty = ast_func
            .resolved_args
            .iter()
            .map(|arg| {
                let arg_ast_ty = &arg.var_type;
                let arg_typeinfo = TypeInfo::new(arg_ast_ty, &self.type_ctx);
                arg_typeinfo.get_value_type_id()
            })
            .collect::<Box<_>>();
        let func_ty = self
            .type_ctx
            .make_func_type(arg_ty.as_ref(), ret_ty, ast_func.is_vararg);

        if ast_func.is_extern() {
            // Extern function, just declare it
            let func_ref = self
                .ir_builder
                .declare_function(name, func_ty)
                .expect("Failed to declare extern function");
            // Insert the function into the symbol map
            self.symbols
                .insert_function(Rc::clone(ast_func), func_ref, func_ty);
        } else {
            let func_ref = self
                .ir_builder
                .define_function_with_unreachable(name, func_ty)
                .expect(format!("Failed to define function `{}`", name).as_str());
            if name != "main" {
                let allocs = self.ir_builder.module.borrow_allocs();
                let func = FuncRef(func_ref).to_data(&allocs.globals);
                func.set_linkage(Linkage::Private);
            }
            // Insert the function into the symbol map
            self.symbols
                .insert_function(Rc::clone(ast_func), func_ref, func_ty);

            // translate the function body
            let body = ast_func.body.borrow();
            let body = body.as_ref().expect("Function body should not be None");
            self.translate_function_define(ast_func, func_ref, body);
        }
    }

    fn translate_expr_force_const(&mut self, expr: &AstExpr, type_req: ValTypeID) -> ValueSSA {
        match expr {
            AstExpr::None => ValueSSA::ConstData(ConstData::Zero(type_req)),
            AstExpr::Literal(Literal::Int(i)) => match type_req {
                ValTypeID::Int(1) => APInt::from(*i != 0).into(),
                ValTypeID::Int(bits) => APInt::new(*i, bits).into(),
                _ => APInt::from(*i).into(),
            },
            AstExpr::Literal(Literal::Float(f)) => {
                ValueSSA::ConstData(ConstData::Float(FloatTypeKind::Ieee32, *f as f64))
            }
            AstExpr::String(str) => {
                let str_ref =
                    expr::translate_string_literal(&mut self.ir_builder, &mut self.strings, str);
                ValueSSA::Global(str_ref)
            }
            AstExpr::ArrayInitList(arr) => {
                let arr_expr_ref =
                    expr::translate_array_init_list_const(&self.ir_builder.module, arr);
                ValueSSA::ConstExpr(arr_expr_ref)
            }
            _ => panic!("Expected a constant expression, but got: {:?}", expr),
        }
    }

    fn header_add_alloca_inst(
        &mut self,
        func_stat: &mut FunctionState,
        data_type: ValTypeID,
        align_log2: u8,
    ) -> InstRef {
        let prev_focus = self.ir_builder.get_focus_full();
        self.ir_builder
            .set_focus(IRBuilderFocus::Inst(func_stat.inst_before_last_alloca));
        let alloca = self
            .ir_builder
            .add_alloca_inst(data_type, align_log2)
            .unwrap();
        // Restore the focus to the previous block
        self.ir_builder.focus = prev_focus;
        // Update the state with the new alloca instruction
        func_stat.inst_before_last_alloca = alloca;
        alloca
    }
}

/// Translate the function body and statements.
impl IRTranslator {
    fn translate_function_define(
        &mut self,
        ast_func: &Rc<AstFunc>,
        func_ref: GlobalRef,
        body: &AstBlock,
    ) {
        eprintln!("Translating function definition: {}", ast_func.name);
        let entry_block = self.ir_builder.get_focus_full().block;
        let inst_before_last_alloca = entry_block
            .to_data(&self.ir_builder.module.borrow_allocs().blocks)
            .phi_end;
        let mut func_stat = FunctionState {
            ir_func: func_ref,
            func_name: ast_func.name.clone(),
            inst_before_last_alloca,
            curr_block: entry_block,
            while_loop_map: HashMap::new(),
        };
        self.ir_builder
            .set_focus(IRBuilderFocus::Block(entry_block));

        // 调整结束语句: 如果函数返回类型是 void 则把末尾的换成 `ret void` 指令
        if let AstType::Void = ast_func.ret_type {
            self.ir_builder.focus_set_return(ValueSSA::None).unwrap();
        }
        // Add the function header
        self.translate_funcdef_args(&mut func_stat, ast_func);
        // Translate the function body block
        // Function body owns the whole basic block uniquely, so it is the end of
        // statement list.
        self.translate_block(&mut func_stat, body, true);
    }

    fn translate_funcdef_args(&mut self, func_stat: &mut FunctionState, ast_func: &AstFunc) {
        let block_entry = self.ir_builder.get_focus_full().block;
        self.ir_builder
            .set_focus(IRBuilderFocus::Block(block_entry));

        // translate the arguments

        for (i, arg) in ast_func.resolved_args.iter().enumerate() {
            let arg_typeinfo = TypeInfo::new(&arg.var_type, &self.type_ctx);
            let arg_ir_value =
                ValueSSA::FuncArg(self.ir_builder.get_focus_full().function, i as u32);

            // 值语义的参数类型 (int, float 就这两个) 需要 push 到栈上
            let (arg_symbol_value, arg_tyinfo) = if arg_typeinfo.is_value_semantic() {
                let arg_ir_type = arg_typeinfo.get_alloca_data_type().unwrap();
                let arg_alloca = self.header_add_alloca_inst(func_stat, arg_ir_type, 2);
                self.ir_builder
                    .add_store_inst(ValueSSA::Inst(arg_alloca), arg_ir_value, 4)
                    .unwrap();
                (ValueSSA::Inst(arg_alloca), TypeInfo::LValue(arg_ir_type))
            } else {
                // 引用语义的参数类型 (数组, 函数) 直接使用 SSA 值即可
                (arg_ir_value, arg_typeinfo)
            };

            // Insert the argument into the symbol map
            self.symbols
                .insert_variable(arg.clone(), arg_symbol_value, arg_tyinfo);
        }
    }

    fn translate_block(
        &mut self,
        func_stat: &mut FunctionState,
        ast_block: &AstBlock,
        is_block_end: bool, // if it is the end of its parent block
    ) {
        // 为了减少基本块拆分次数, AST 代码块通常不会单独 split 一块基本块出来处理.
        // 如果自己不是父语句中所有基本块的末尾块, 那它的子语句没有一个是末尾语句;
        // 否则就可以考虑末尾语句的问题了.
        for (idx, stmt) in ast_block.stmts.iter().enumerate() {
            let is_block_end = if is_block_end {
                idx == ast_block.stmts.len() - 1
            } else {
                false
            };
            self.translate_stmt(func_stat, stmt, is_block_end);
        }
    }

    fn translate_stmt(&mut self, func_stat: &mut FunctionState, stmt: &Stmt, is_block_end: bool) {
        match stmt {
            Stmt::Block(block) => self.translate_block(func_stat, &block, is_block_end),
            Stmt::VarDecl(local) => {
                if local.is_const {
                    self.translate_global_vardecl(&local, Some(func_stat.func_name.as_str()));
                } else {
                    for i in local.defs.iter() {
                        self.translate_local_var(func_stat, i);
                    }
                }
            }
            Stmt::If(if_stmt) => self.translate_if_stmt(func_stat, if_stmt),
            Stmt::While(while_stmt) => self.translate_while_stmt(func_stat, while_stmt),
            Stmt::ExprStmt(expr) => {
                self.translate_alu_expr(func_stat, &expr.expr.borrow());
            }
            Stmt::BreakTo(loop_target) => {
                let loop_info = func_stat
                    .while_loop_map
                    .get(&WhileLoopRef(loop_target.upgrade().unwrap()))
                    .unwrap();
                let exit_block = loop_info.exit_block;
                let next_block = self.control_stmt_make_next(func_stat, is_block_end);
                self.ir_builder.focus_set_jump_to(exit_block).unwrap();
                if !is_block_end {
                    self.set_focus_block(func_stat, next_block);
                }
            }
            Stmt::ContinueTo(loop_target) => {
                let loop_info = func_stat
                    .while_loop_map
                    .get(&WhileLoopRef(loop_target.upgrade().unwrap()))
                    .unwrap();
                let entry_block = loop_info.entry_block;
                let next_block = self.control_stmt_make_next(func_stat, is_block_end);
                self.ir_builder.focus_set_jump_to(entry_block).unwrap();
                if !is_block_end {
                    self.set_focus_block(func_stat, next_block);
                }
            }
            Stmt::Return(expr) => {
                let (retval, _) = self.translate_alu_expr(func_stat, expr);
                let next_block = self.control_stmt_make_next(func_stat, is_block_end);
                self.ir_builder
                    .focus_set_return(retval)
                    .expect("Failed to set return value");
                if !is_block_end {
                    self.set_focus_block(func_stat, next_block);
                }
            }

            Stmt::FuncDecl(_) => {
                panic!(
                    "Function declaration should not be in function body: {:?}",
                    func_stat.ir_func
                );
            }
            Stmt::Break | Stmt::Continue | Stmt::UnresolvedVarDecl(_) => {
                panic!(
                    "AST-only un-normalized statement cannot be translated: {:?}",
                    stmt
                );
            }
            Stmt::None => { /* Do nothing for empty statements */ }
        }
    }

    fn control_stmt_make_next(
        &mut self,
        func_stat: &mut FunctionState,
        is_block_end: bool,
    ) -> IRBlockRef {
        let curr_block = func_stat.curr_block;
        if is_block_end {
            curr_block
        } else {
            self.ir_builder
                .split_current_block_from_terminator()
                .unwrap()
        }
    }
    fn set_focus_block(&mut self, func_stat: &mut FunctionState, focus: IRBlockRef) {
        self.ir_builder.set_focus(IRBuilderFocus::Block(focus));
        func_stat.curr_block = focus;
    }

    fn translate_local_var(&mut self, func_stat: &mut FunctionState, var: &Rc<Variable>) {
        let var_typeinfo = TypeInfo::new(&var.var_type, &self.type_ctx);
        let var_content_ty = var_typeinfo
            .get_alloca_data_type()
            .expect("Local variable type must be allocatable");
        let alloca_inst = self.header_add_alloca_inst(func_stat, var_content_ty, 2);

        // Insert the variable into the symbol map
        self.symbols.insert_variable(
            Rc::clone(var),
            ValueSSA::Inst(alloca_inst),
            match var_content_ty {
                ValTypeID::Ptr
                | ValTypeID::Int(_)
                | ValTypeID::Float(_)
                | ValTypeID::Struct(_)
                | ValTypeID::StructAlias(_) => TypeInfo::LValue(var_content_ty),
                ValTypeID::Array(arr) => TypeInfo::FixArray(arr),
                ValTypeID::Void => panic!("Local variable {} cannot be of type Void", var.name),
                ValTypeID::Func(_) => {
                    panic!("Local variable {} cannot be of type Function", var.name)
                }
            },
        );

        // Initialize initval
        match &var.initval {
            AstExpr::None => {}
            AstExpr::ArrayInitList(arr_list) => {
                let var_content_ty = match var_content_ty {
                    ValTypeID::Array(arr) => arr,
                    _ => panic!("Expected a fixed array type for array initializer"),
                };
                let level0_ty = var_content_ty.get_element_type(&self.type_ctx);

                if arr_list.is_zero_initializer() {
                    // If the initializer is a zero initializer, we can use memset
                    log::debug!("array list {arr_list:#?}");
                    self.ir_builder
                        .add_call_inst(
                            self.ir_stdlib_funcs.memset,
                            [
                                ValueSSA::Inst(alloca_inst),
                                APInt::from(0u32).into(),
                                APInt::from(arr_list.get_size_bytes()).into(),
                            ]
                            .iter(),
                        )
                        .expect("Failed to call memset for zero initialization");
                    return;
                }

                let mut mlindex =
                    MultiLevelIndex::from_slice(&arr_list.dimensions[0..arr_list.n_dimensions()]);
                for final_idx in 0..arr_list.final_elems.len() {
                    let source = arr_list
                        .final_elems
                        .get(final_idx)
                        .expect("Array initializer should have enough elements");
                    let (source, _) = self.translate_alu_expr(func_stat, source);

                    let indices: Vec<ValueSSA> = mlindex
                        .curr
                        .iter()
                        .map(|&i| APInt::from(i).into())
                        .collect();
                    let gep = self
                        .ir_builder
                        .add_indexptr_inst(level0_ty, 4, 4, ValueSSA::Inst(alloca_inst), &indices)
                        .unwrap();
                    self.ir_builder
                        .add_store_inst(ValueSSA::Inst(gep), source, 4)
                        .expect("Failed to store array initializer value");
                    mlindex.inc_dim(0);
                }
            }

            AstExpr::Assign(_) | AstExpr::IntrinsicTimeStart(_) | AstExpr::IntrinsicTimeEnd(_) => {
                panic!("Encountered `void` expression")
            }
            AstExpr::RawInitList(_) => panic!(
                "Discovered an AST-only raw initializer expression in local variable `{}`",
                var.name
            ),

            _ => {
                // Translate the initializer expression
                let (initval, initval_type_info) = self.translate_alu_expr(func_stat, &var.initval);
                if !matches!(initval_type_info, TypeInfo::RValue(_)) {
                    panic!(
                        "Initializer expression for variable `{}` must be an rvalue, but got: {:?}",
                        var.name, initval_type_info
                    );
                }
                // Store the initial value into the alloca instruction
                self.ir_builder
                    .add_store_inst(ValueSSA::Inst(alloca_inst), initval, 4)
                    .unwrap();
            }
        }
    }
}

/// Translate ALU expressions.
impl IRTranslator {
    fn translate_alu_expr(
        &mut self,
        func_stat: &mut FunctionState,
        expr: &AstExpr,
    ) -> (ValueSSA, TypeInfo) {
        match expr {
            AstExpr::None => (ValueSSA::None, TypeInfo::RValue(ValTypeID::Void)),
            AstExpr::Literal(Literal::Int(i)) => {
                (APInt::from(*i).into(), TypeInfo::RValue(ValTypeID::Int(32)))
            }
            AstExpr::Literal(Literal::Float(f)) => (
                ValueSSA::ConstData(ConstData::Float(FloatTypeKind::Ieee32, *f as f64)),
                TypeInfo::RValue(ValTypeID::Float(FloatTypeKind::Ieee32)),
            ),
            AstExpr::String(s) => {
                let str_ref =
                    expr::translate_string_literal(&mut self.ir_builder, &mut self.strings, s);
                (
                    ValueSSA::Global(str_ref),
                    TypeInfo::DynArray(ValTypeID::Int(8)),
                )
            }
            AstExpr::Ident(ident) => {
                // 在规范化的 SST 中, 标识符这种左值要参与数学运算的话,
                // 一定要经过一步 `LValueToRValue` 的转换. 因此这里
                // 取到标识符指针的时候就不要再加一个 `load` 指令了.
                self.symbols.get_ident(ident).unwrap_or_else(|| {
                    panic!("Identifier `{}` not found in symbol map", ident.get_name())
                })
            }
            AstExpr::ArrayIndex(idx) => self.translate_alu_array_index(func_stat, idx),
            AstExpr::BinOP(bin_exp) => self.translate_alu_binop(func_stat, bin_exp),
            AstExpr::CmpOP(cmp_exp) => self.translate_cmp_binop(func_stat, cmp_exp),
            AstExpr::UnaryOP(unary) => self.translate_unaryop(func_stat, unary),
            AstExpr::ImplicitCast(cast) => self.translate_alu_implicit_cast(func_stat, cast),
            AstExpr::Call(call) => self.translate_alu_call(func_stat, call),
            AstExpr::Assign(assign) => {
                // Translate the left-hand side expression
                let (lhs, _) = self.translate_alu_expr(func_stat, &assign.lhs);
                // Translate the right-hand side expression
                let (rhs, _) = self.translate_alu_expr(func_stat, &assign.rhs);

                // Perform the assignment operation
                let inst = self.ir_builder.add_store_inst(lhs, rhs, 4).unwrap();
                (ValueSSA::Inst(inst), TypeInfo::RValue(ValTypeID::Void))
            }

            AstExpr::IntrinsicTimeStart(lineno) => {
                let lineno_value = APInt::from(*lineno).into();
                let call_inst = self
                    .ir_builder
                    .add_call_inst(self.ast_intrinsic_funcs.start_time, [lineno_value].iter())
                    .unwrap();
                (ValueSSA::Inst(call_inst), TypeInfo::RValue(ValTypeID::Void))
            }
            AstExpr::IntrinsicTimeEnd(lineno) => {
                let lineno_value = APInt::from(*lineno).into();
                let call_inst = self
                    .ir_builder
                    .add_call_inst(self.ast_intrinsic_funcs.stop_time, [lineno_value].iter())
                    .unwrap();
                (ValueSSA::Inst(call_inst), TypeInfo::RValue(ValTypeID::Void))
            }

            AstExpr::ArrayInitList(_) => {
                panic!("Array initializer should not be in an ALU expression context")
            }
            AstExpr::ShortCircuit(_) => {
                panic!("Short-circuit expression should not be in an ALU expression context")
            }
            AstExpr::RawInitList(_) => {
                panic!("Discovered an AST-only raw initializer expression")
            }
        }
    }

    fn translate_alu_array_index(
        &mut self,
        func_stat: &mut FunctionState,
        idx: &ArrayIndex,
    ) -> (ValueSSA, TypeInfo) {
        let (array, arrty_info) = self.symbols.get_ident(&idx.indexee).unwrap();
        let indices = idx
            .indices
            .iter()
            .map(|i| self.translate_alu_expr(func_stat, i).0)
            .collect::<Vec<_>>();
        let level0_ty = match arrty_info {
            TypeInfo::DynArray(elem_ty) => elem_ty,
            TypeInfo::FixArray(arr_ty) => arr_ty.get_element_type(&self.type_ctx),
            _ => panic!("Expected an array type, but got: {:?}", arrty_info),
        };
        let gep_ref = self
            .ir_builder
            .add_indexptr_inst(level0_ty, 4, 4, array, &indices)
            .unwrap();
        let elemty = if let InstData::GEP(gep) =
            gep_ref.to_data(&self.ir_builder.module.borrow_allocs().insts)
        {
            gep.last_unpacked_ty
        } else {
            panic!(
                "Expected an index pointer instruction, but got: {:?}",
                gep_ref
            );
        };
        (ValueSSA::Inst(gep_ref), TypeInfo::LValue(elemty))
    }

    fn translate_alu_binop(
        &mut self,
        func_stat: &mut FunctionState,
        bin_exp: &BinExp,
    ) -> (ValueSSA, TypeInfo) {
        let (lhs, lhs_ty) = self.translate_alu_expr(func_stat, &bin_exp.lhs);
        let (rhs, rhs_ty) = self.translate_alu_expr(func_stat, &bin_exp.rhs);

        // Ensure the types are compatible
        if lhs_ty != rhs_ty {
            panic!(
                "Incompatible types for binary operation: {:?} and {:?}",
                lhs_ty, rhs_ty
            );
        }
        let is_float = match lhs_ty {
            TypeInfo::RValue(ValTypeID::Float(FloatTypeKind::Ieee32)) => true,
            TypeInfo::RValue(ValTypeID::Int(32)) => false,
            _ => panic!("Unsupported type for binary operation: {:?}", lhs_ty),
        };

        #[rustfmt::skip]
        let opcode = match bin_exp.op {
            Operator::Add => if is_float { Opcode::Fadd } else { Opcode::Add },
            Operator::Sub => if is_float { Opcode::Fsub } else { Opcode::Sub },
            Operator::Mul => if is_float { Opcode::Fmul } else { Opcode::Mul },
            Operator::Div => if is_float { Opcode::Fdiv } else { Opcode::Sdiv },
            Operator::Mod => if is_float { Opcode::Frem } else { Opcode::Srem },
            _ => panic!("Unsupported operator for binary operation: {:?}", bin_exp.op),
        };
        let bin_inst = self.ir_builder.add_binop_inst(opcode, lhs, rhs).unwrap();
        (
            ValueSSA::Inst(bin_inst),
            TypeInfo::RValue(lhs_ty.get_value_type_id()),
        )
    }

    fn translate_cmp_binop(
        &mut self,
        func_stat: &mut FunctionState,
        cmp_exp: &BinExp,
    ) -> (ValueSSA, TypeInfo) {
        let (lhs, lhs_ty) = self.translate_alu_expr(func_stat, &cmp_exp.lhs);
        let (rhs, rhs_ty) = self.translate_alu_expr(func_stat, &cmp_exp.rhs);

        // Ensure the types are compatible
        if lhs_ty != rhs_ty {
            panic!(
                "Incompatible types for comparison: {:?} and {:?}",
                lhs_ty, rhs_ty
            );
        }
        let is_float = match lhs_ty {
            TypeInfo::RValue(ValTypeID::Float(FloatTypeKind::Ieee32)) => true,
            TypeInfo::RValue(ValTypeID::Int(32)) => false,
            _ => panic!("Unsupported type for comparison: {:?}", lhs_ty),
        };

        #[rustfmt::skip]
        let mut cond = match cmp_exp.op {
            Operator::Eq => CmpCond::EQ,
            Operator::Ne => CmpCond::NE,
            Operator::Gt => CmpCond::GT,
            Operator::Ge => CmpCond::GE,
            Operator::Lt => CmpCond::LT,
            Operator::Le => CmpCond::LE,
            _ => panic!("Unsupported operator for comparison: {:?}", cmp_exp.op),
        };
        cond.insert(CmpCond::SIGNED_ORDERED);
        if is_float {
            cond.switch_to_float();
        }

        let cmp_inst = self
            .ir_builder
            .add_cmp_inst(cond, lhs, rhs)
            .expect("Failed to add comparison instruction");
        (
            ValueSSA::Inst(cmp_inst),
            TypeInfo::RValue(ValTypeID::Int(1)), // Comparison results are always boolean (int 1)
        )
    }

    fn translate_unaryop(
        &mut self,
        func_stat: &mut FunctionState,
        unary_exp: &UnaryExp,
    ) -> (ValueSSA, TypeInfo) {
        let (operand, operand_ty) = self.translate_alu_expr(func_stat, &unary_exp.expr);

        let (is_float, is_bool, operand_ty) = match operand_ty {
            TypeInfo::RValue(ValTypeID::Float(FloatTypeKind::Ieee32)) => {
                (true, false, ValTypeID::Float(FloatTypeKind::Ieee32))
            }
            TypeInfo::RValue(ValTypeID::Int(32)) => (false, false, ValTypeID::Int(32)),
            TypeInfo::RValue(ValTypeID::Int(1)) => (false, true, ValTypeID::Int(1)),
            _ => panic!("Unsupported type for unary operation: {:?}", operand_ty),
        };
        match unary_exp.op {
            Operator::Sub | Operator::Neg => {
                let opcode = if is_float { Opcode::Fsub } else { Opcode::Sub };
                let zero_value = if is_float {
                    ConstData::Float(FloatTypeKind::Ieee32, 0.0).into_ir()
                } else {
                    APInt::from(0u32).into()
                };
                let negsub_inst = self
                    .ir_builder
                    .add_binop_inst(opcode, zero_value, operand)
                    .expect("Failed to add unary negation/subtraction instruction");
                (ValueSSA::Inst(negsub_inst), TypeInfo::RValue(operand_ty))
            }
            Operator::LogicalNot => {
                let false_val = APInt::from(false).into();
                let true_val = APInt::from(true).into();
                if is_bool {
                    // Logical NOT for boolean values
                    let select_inst = self
                        .ir_builder
                        .add_select_inst(operand, false_val, true_val)
                        .expect("Failed to add logical NOT instruction");
                    (
                        ValueSSA::Inst(select_inst),
                        TypeInfo::RValue(ValTypeID::new_boolean()),
                    )
                } else {
                    let zero_value = if is_float {
                        ConstData::Float(FloatTypeKind::Ieee32, 0.0).into_ir()
                    } else {
                        APInt::from(0u32).into()
                    };
                    // Logical NOT means equal to zero
                    let cmp_inst = self
                        .ir_builder
                        .add_cmp_inst(CmpCond::EQ, operand, zero_value)
                        .expect("Failed to add logical NOT instruction");
                    (
                        ValueSSA::Inst(cmp_inst),
                        TypeInfo::RValue(ValTypeID::Int(1)), // Result is a boolean (int 1)
                    )
                }
            }
            _ => panic!(
                "Unsupported unary operator: {:?} in expression: {:?}",
                unary_exp.op, unary_exp.expr
            ),
        }
    }

    fn translate_alu_call(
        &mut self,
        func_stat: &mut FunctionState,
        call: &AstCall,
    ) -> (ValueSSA, TypeInfo) {
        let (func_symbol, func_ty) = self.symbols.get_ident(&call.name).unwrap_or_else(|| {
            panic!(
                "Function `{}` not found in symbol map",
                call.name.get_name()
            )
        });
        let func_symbol = match func_symbol {
            ValueSSA::Global(gref) => gref,
            _ => panic!("Expected a function symbol, but got: {:?}", func_symbol),
        };
        let func_ty = match func_ty {
            TypeInfo::Func(ft) => ft,
            _ => panic!("Expected a function type, but got: {:?}", func_ty),
        };
        // Translate the arguments
        let args = call
            .args
            .iter()
            .map(|arg| {
                let (arg_value, _) = self.translate_alu_expr(func_stat, arg);
                arg_value
            })
            .collect::<Vec<_>>();

        // Add the call instruction
        let call_inst = self
            .ir_builder
            .add_call_inst(func_symbol, args.iter())
            .expect("Failed to add call instruction");
        (
            ValueSSA::Inst(call_inst),
            TypeInfo::RValue(func_ty.get_return_type(&self.type_ctx)),
        )
    }

    fn translate_alu_implicit_cast(
        &mut self,
        func_stat: &mut FunctionState,
        cast: &ImplicitCast,
    ) -> (ValueSSA, TypeInfo) {
        let (operand, operand_ty) = self.translate_alu_expr(func_stat, &cast.expr);
        let (inst, ret_info) = match cast.kind {
            Operator::ItoF => (
                self.ir_builder
                    .add_cast_inst(
                        Opcode::Sitofp,
                        ValTypeID::Float(FloatTypeKind::Ieee32),
                        operand,
                    )
                    .unwrap(),
                TypeInfo::RValue(ValTypeID::Float(FloatTypeKind::Ieee32)),
            ),
            Operator::FtoI => (
                self.ir_builder
                    .add_cast_inst(Opcode::Fptosi, ValTypeID::Int(32), operand)
                    .unwrap(),
                TypeInfo::RValue(ValTypeID::Int(32)),
            ),
            Operator::BoolToInt => (
                self.ir_builder
                    .add_cast_inst(Opcode::Zext, ValTypeID::Int(32), operand)
                    .unwrap(),
                TypeInfo::RValue(ValTypeID::Int(32)),
            ),
            Operator::BoolToFloat => (
                self.ir_builder
                    .add_cast_inst(
                        Opcode::Uitofp,
                        ValTypeID::Float(FloatTypeKind::Ieee32),
                        operand,
                    )
                    .unwrap(),
                TypeInfo::RValue(ValTypeID::Float(FloatTypeKind::Ieee32)),
            ),
            Operator::LValueToRValue => {
                let load_ptr_target_ty = operand_ty.get_alloca_data_type().unwrap();
                (
                    self.ir_builder
                        .add_load_inst(load_ptr_target_ty, 4, operand)
                        .unwrap(),
                    TypeInfo::RValue(load_ptr_target_ty),
                )
            }
            _ => panic!(
                "Unsupported implicit cast operator: {:?} in expression: {:?}",
                cast.kind, cast.expr
            ),
        };
        (ValueSSA::Inst(inst), ret_info)
    }

    fn translate_if_stmt(&mut self, func_stat: &mut FunctionState, if_stmt: &IfStmt) {
        let if_exit_block = self
            .ir_builder
            .split_current_block_from_terminator()
            .expect("Failed to split current block for if statement");
        if let Some(else_stmt) = &if_stmt.else_stmt {
            let if_else_block = self
                .ir_builder
                .split_current_block_from_terminator()
                .expect("Failed to split current block for if statement \"else\"");
            let if_then_block = self
                .ir_builder
                .split_current_block_from_terminator()
                .expect("Failed to split current block for if statement body");
            let if_cond_block = self.ir_builder.get_focus_full().block;

            self.ir_builder
                .set_focus(IRBuilderFocus::Block(if_then_block));
            self.ir_builder
                .focus_set_jump_to(if_exit_block)
                .expect("Failed to jump to 'if-exit'");
            self.ir_builder
                .set_focus(IRBuilderFocus::Block(if_cond_block));

            self.translate_cond_exprs(func_stat, &if_stmt.cond, if_then_block, if_else_block);

            self.set_focus_block(func_stat, if_then_block);
            self.translate_stmt(func_stat, &if_stmt.then_stmt, true);

            self.set_focus_block(func_stat, if_else_block);
            self.translate_stmt(func_stat, else_stmt, true);
        } else {
            let if_then_block = self
                .ir_builder
                .split_current_block_from_terminator()
                .expect("Failed to split current block for if statement body");

            // current focus is `if condition block`
            self.translate_cond_exprs(func_stat, &if_stmt.cond, if_then_block, if_exit_block);

            self.set_focus_block(func_stat, if_then_block);
            self.translate_stmt(func_stat, &if_stmt.then_stmt, true);
        }
        self.set_focus_block(func_stat, if_exit_block);
    }

    fn translate_while_stmt(&mut self, func_stat: &mut FunctionState, while_stmt: &Rc<WhileStmt>) {
        let while_exit_block = self
            .ir_builder
            .split_current_block_from_terminator()
            .expect("error in splitting while exit block");
        let while_body_block = self
            .ir_builder
            .split_current_block_from_terminator()
            .expect("error in splitting while body block");
        let while_cond_block = self
            .ir_builder
            .split_current_block_from_terminator()
            .expect("error in splitting while cond block");

        func_stat.while_loop_map.insert(
            WhileLoopRef(while_stmt.clone()),
            WhileLoopInfo {
                entry_block: while_cond_block,
                exit_block: while_exit_block,
            },
        );

        self.ir_builder
            .set_focus(IRBuilderFocus::Block(while_body_block));
        self.ir_builder
            .focus_set_jump_to(while_cond_block)
            .expect("error in setting while loop-back jumping");

        self.set_focus_block(func_stat, while_cond_block);
        self.translate_cond_exprs(
            func_stat,
            &while_stmt.cond,
            while_body_block,
            while_exit_block,
        );
        self.set_focus_block(func_stat, while_body_block);
        self.translate_stmt(func_stat, &while_stmt.body.borrow(), true);
        self.set_focus_block(func_stat, while_exit_block);
    }

    fn translate_cond_exprs(
        &mut self,
        func_stat: &mut FunctionState,
        cond: &AstExpr,
        if_true: IRBlockRef,
        if_false: IRBlockRef,
    ) {
        if let AstExpr::ShortCircuit(logic) = cond {
            self.translate_short_circuit(func_stat, logic, if_true, if_false);
        } else {
            let (cond_ir, cond_ty) = self.translate_alu_expr(func_stat, cond);
            let cond_ir = match cond_ty {
                TypeInfo::RValue(ValTypeID::Int(1)) => cond_ir,
                TypeInfo::RValue(ValTypeID::Int(_)) => {
                    // 因为 Remusys-SST 没有布尔常量, 正规化语法树时有可能会得到一个 int 常量.
                    // 这里特殊处理之.
                    let Some(i) = cond_ir.as_int() else {
                        panic!("Expected a boolean condition, but got: {:?}", cond_ty);
                    };
                    APInt::from(i != 0).into()
                }
                // if 表达式返回的如果不是布尔或者整数常量, 那就意味着正规化没做好.
                _ => panic!(
                    "Expected a boolean condition expression, but got: {:?}",
                    cond_ty
                ),
            };
            self.ir_builder
                .focus_set_branch_to(cond_ir, if_true, if_false)
                .expect("Error setting branch in control statements");
        }
    }
    /// 短路表达式和其他表达式都不一样的是, 它们会造成控制流的变化。 其他表达式
    /// 翻译成的指令都可以挤在一个基本块里, 而短路表达式需要分成两个基本块。
    fn translate_short_circuit(
        &mut self,
        func_stat: &mut FunctionState,
        logic: &BinExp,
        if_true: IRBlockRef,
        if_false: IRBlockRef,
    ) {
        let rhs_block = self
            .ir_builder
            .split_current_block_from_terminator()
            .expect("Error splitting `rhs` block from short circuit");
        match logic.op {
            Operator::LogicalAnd => {
                self.translate_cond_exprs(func_stat, &logic.lhs, rhs_block, if_false)
            }
            Operator::LogicalOr => {
                self.translate_cond_exprs(func_stat, &logic.lhs, if_true, rhs_block)
            }
            _ => panic!("expected logical operator but got {:?}", logic.op),
        }
        self.set_focus_block(func_stat, rhs_block);
        self.translate_cond_exprs(func_stat, &logic.rhs, if_true, if_false);
    }
}

#[cfg(test)]
mod testing {
    use std::path::PathBuf;

    use remusys_ir::ir::write_ir_module;
    use remusys_lang::{ast::print::AstPrinter, normalize::AstNormalizer, parser::parse_sysy_file};

    use super::*;

    #[test]
    fn test_translator() {
        let input_dir = "target/test-functional/sysy";
        let sst_dir = "target/test-functional/sst";
        let ir_dir = "target/test-functional/ir";

        // if input directory does not exist, notify and return
        if !PathBuf::from(input_dir).exists() {
            println!("Input directory `{}` does not exist", input_dir);
            return;
        }

        // create output directories if they do not exist
        std::fs::create_dir_all(sst_dir).unwrap();
        std::fs::create_dir_all(ir_dir).unwrap();

        // Iterate over all files in the input directory
        let test_thread = std::thread::Builder::new()
            .name("IRTranslatorTest".to_string())
            .stack_size(64 * 1024 * 1024) // 64 MB stack size
            .spawn(move || {
                let mut input_paths = Vec::new();
                for entry in std::fs::read_dir(input_dir).unwrap() {
                    let entry = entry.unwrap();
                    let path = entry.path();
                    if path.is_file() {
                        if let Some(ext) = path.extension() {
                            if ext == "sy" || ext == "sysy" {
                                input_paths.push(path);
                            }
                        }
                    }
                }
                input_paths.sort();
                for input_path in input_paths {
                    let input_path_str = input_path.to_str().unwrap();
                    let file_name = input_path.file_stem().unwrap().to_str().unwrap();
                    let ast_path = format!("{}/{}.sst", sst_dir, file_name);
                    let ir_path = format!("{}/{}.ll", ir_dir, file_name);

                    // Run the translator test
                    thrd_test_translator(input_path_str, &ast_path, &ir_path);
                }
            })
            .unwrap();
        // Wait for the thread to finish
        test_thread.join().unwrap();
        println!("IR translation tests completed successfully.");
    }

    fn thrd_test_translator(input_path: &str, ast_path: &str, ir_path: &str) {
        eprintln!("Translating: {}", input_path);
        let ast_module = parse_sysy_file(input_path);
        let sst_module = AstNormalizer::new(&ast_module).normalize();

        eprintln!("Writing AST to: {}", ast_path);
        let sst_file = std::fs::File::create(ast_path).unwrap();
        let mut sst_writer = std::io::BufWriter::new(sst_file);
        AstPrinter::new(&sst_module, &mut sst_writer).print_module();

        eprintln!("Translating to IR: {}", ir_path);
        let ir_module = IRTranslator::new_competition(&sst_module).translate(&sst_module);

        eprintln!("Writing IR to: {}", ir_path);
        let ir_file = std::fs::File::create(ir_path).unwrap();
        let mut ir_writer = std::io::BufWriter::new(ir_file);
        write_ir_module(&ir_module, &mut ir_writer);
    }
}
