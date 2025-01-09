use std::collections::VecDeque;
use std::sync::Arc;

use egglog::ast::{
    Action, Command, Expr, GenericCommand, GenericExpr, Literal, Schema, Symbol, Variant,
};
use egglog::sort::{I64Sort, Sort, StringSort};
use itertools::Itertools;
use llhd::ir::prelude::*;
use llhd::ir::ValueData;
use llhd::table::TableKey;
use llhd::{IntValue, TimeValue, Type, TypeKind};
use rayon::iter::ParallelIterator;

use crate::llhd::LLHDUtils;
use crate::llhd_egraph::datatype::unit_root_variant_symbol;
use crate::llhd_egraph::egglog_names::*;
use crate::llhd_egraph::inst::*;
use egglog_program::egraph::egglog_names::*;
use egglog_program::*;

#[derive(Debug, Clone, Default)]
pub struct LLHDEgglogFacts(EgglogCommandList);

impl LLHDEgglogFacts {
    pub fn from_module(module: &Module) -> Self {
        Self(
            module
                .par_units()
                .map(|unit| GenericCommand::Action(from_unit(&unit)))
                .collect(),
        )
    }

    pub fn from_unit(unit: &Unit) -> Self {
        Self(vec![GenericCommand::Action(from_unit(unit))])
    }
}

impl From<LLHDEgglogFacts> for EgglogCommandList {
    fn from(llhd_facts: LLHDEgglogFacts) -> Self {
        llhd_facts.0
    }
}

impl From<LLHDEgglogFacts> for EgglogFacts {
    fn from(llhd_facts: LLHDEgglogFacts) -> Self {
        Self::default().add_facts(<LLHDEgglogFacts as Into<EgglogCommandList>>::into(
            llhd_facts,
        ))
    }
}

type ExprFIFO = VecDeque<Expr>;
type ValueStack = VecDeque<Value>;
type IntValueStack = VecDeque<IntValue>;
type TimeValueStack = VecDeque<TimeValue>;
type LLHDTypeFIFO = VecDeque<Type>;

const UNIT_LET_STMT_PREFIX: &str = "unit_";

pub(crate) fn unit_symbol(unit: Unit<'_>) -> Symbol {
    let mut unit_name = unit.name().to_string().replace(&['@', '%', ','][..], "");
    unit_name.insert_str(0, UNIT_LET_STMT_PREFIX);
    Symbol::new(unit_name)
}

fn from_unit(unit: &Unit<'_>) -> Action {
    let unit_id = unit.id();
    let unit_kind = unit.kind();
    let unit_name = unit.name();
    let unit_sig = unit.sig().clone();
    let unit_id_expr = Expr::Lit(
        DUMMY_SPAN.clone(),
        Literal::Int(
            i64::try_from(unit_id.index())
                .expect("Out-of-bound value for usize -> i64 conversion."),
        ),
    );
    let unit_kind_symbol = match unit_kind {
        UnitKind::Entity => Symbol::new(LLHD_UNIT_ENTITY_FIELD),
        UnitKind::Function => Symbol::new(LLHD_UNIT_FUNCTION_FIELD),
        UnitKind::Process => Symbol::new(LLHD_UNIT_PROCESS_FIELD),
    };
    let unit_kind_expr = Expr::Call(DUMMY_SPAN.clone(), unit_kind_symbol, vec![]);
    let unit_name_expr = Expr::Lit(
        DUMMY_SPAN.clone(),
        Literal::String(Symbol::new(unit_name.to_string())),
    );
    let unit_input_args_expr = unit_sig
        .inputs()
        .map(|arg_id| {
            let arg_ty = unit_sig.arg_type(arg_id);
            value_def_expr(arg_ty, arg_id)
        })
        .collect_vec();
    let unit_output_args_expr = unit_sig
        .outputs()
        .map(|arg_id| {
            let arg_ty = unit_sig.arg_type(arg_id);
            value_def_expr(arg_ty, arg_id)
        })
        .collect_vec();
    let unit_input_sig_expr = Expr::Call(
        DUMMY_SPAN.clone(),
        Symbol::new(EGGLOG_VEC_OF_OP),
        unit_input_args_expr,
    );
    let unit_output_sig_expr = Expr::Call(
        DUMMY_SPAN.clone(),
        Symbol::new(EGGLOG_VEC_OF_OP),
        unit_output_args_expr,
    );

    let root_inst_ids = LLHDUtils::root_unit_inst(unit);
    let mut root_inst_exprs: Vec<Expr> = Default::default();
    for (_unit_id, root_inst_id) in root_inst_ids.iter().rev() {
        let mut root_inst_ty = Arc::<TypeKind>::new(TypeKind::VoidType);
        if let Some(root_inst_value) = unit.get_inst_result(*root_inst_id) {
            if let ValueData::Inst { ty, .. } = &unit[root_inst_value] {
                root_inst_ty = ty.clone();
            }
            match &unit[root_inst_value] {
                ValueData::Inst { ty, .. } => root_inst_ty = ty.clone(),
                ValueData::Arg { ty, .. } => root_inst_ty = ty.clone(),
                ValueData::Placeholder { ty } => root_inst_ty = ty.clone(),
                ValueData::Invalid => {}
            }
        }

        let root_inst_data = &unit[*root_inst_id];
        root_inst_exprs.push(inst_expr(unit, *root_inst_id, root_inst_ty, root_inst_data));
    }
    let unit_ctx_expr = Expr::Call(
        DUMMY_SPAN.clone(),
        Symbol::new(EGGLOG_VEC_OF_OP),
        root_inst_exprs,
    );
    let unit_expr_vec = vec![
        unit_id_expr,
        unit_kind_expr,
        unit_name_expr,
        unit_input_sig_expr,
        unit_output_sig_expr,
        unit_ctx_expr,
    ];

    let unit_expr = GenericExpr::Call(
        DUMMY_SPAN.clone(),
        unit_root_variant_symbol(),
        unit_expr_vec,
    );
    Action::Let(DUMMY_SPAN.clone(), unit_symbol(*unit), unit_expr)
}

fn process_expr(expr: &Expr, expr_fifo: &mut ExprFIFO) {
    match expr {
        GenericExpr::Lit(_span, literal) => {
            expr_fifo.push_front(Expr::Lit(DUMMY_SPAN.clone(), literal.to_owned()));
        }
        GenericExpr::Var(_span, symbol) => {
            expr_fifo.push_front(Expr::Var(DUMMY_SPAN.clone(), symbol.to_owned()));
        }
        GenericExpr::Call(_, symbol, dependencies) => {
            if opcode::get_symbol_opcode(symbol).is_some() {
                expr_fifo.push_front(Expr::Call(DUMMY_SPAN.clone(), symbol.to_owned(), vec![]));
                for dep in dependencies.iter().skip(2) {
                    process_expr(dep, expr_fifo);
                }
            } else if Symbol::new(LLHD_TYPE_INT_FIELD) == *symbol {
            } else {
                for dep in dependencies.iter() {
                    process_expr(dep, expr_fifo);
                }
            }
        }
    }
}

fn process_expr_fifo(
    expr_fifo: ExprFIFO,
    value_stack: &mut ValueStack,
    _int_value_stack: &mut IntValueStack,
    time_value_stack: &mut TimeValueStack,
    unit_builder: &mut UnitBuilder,
) {
    for expr in expr_fifo {
        match expr {
            GenericExpr::Lit(_span, literal) => match literal {
                Literal::Int(_value) => {
                    value_stack.push_back(literal_llhd_value(&literal));
                }
                Literal::String(_value) => {
                    time_value_stack.push_back(expr_time_value(&literal));
                }
                _ => {
                    panic!("Unhandled GenericExpr::Literal => {:?}", literal)
                }
            },
            GenericExpr::Var(_span, symbol) => {
                panic!("Unhandled ExprFIFO GenericExpr::Var Symbol => {:?}", symbol)
            }
            GenericExpr::Call(_, symbol, _dependencies) => {
                if let Some(opcode) = opcode::get_symbol_opcode(&symbol) {
                    match opcode {
                        Opcode::Or => {
                            let arg1_value = value_stack.pop_back().expect(
                                "Or arg2 Stack empty despite still trying to process operation.",
                            );
                            let arg2_value = value_stack.pop_back().expect(
                                "Or arg1 Stack empty despite still trying to process operation.",
                            );
                            value_stack.push_back(unit_builder.ins().or(arg1_value, arg2_value));
                        }
                        Opcode::And => {
                            let arg1_value = value_stack.pop_back().expect(
                                "And arg2 Stack empty despite still trying to process operation.",
                            );
                            let arg2_value = value_stack.pop_back().expect(
                                "And arg1 Stack empty despite still trying to process operation.",
                            );
                            value_stack.push_back(unit_builder.ins().and(arg1_value, arg2_value));
                        }
                        Opcode::ConstTime => {
                            let arg1_value = time_value_stack
                                .pop_back()
                                .expect("ConstTime arg1 Stack empty despite still trying to process operation.");
                            value_stack.push_back(unit_builder.ins().const_time(arg1_value));
                        }
                        Opcode::Drv => {
                            let arg1_value = value_stack.pop_back().expect(
                                "Drv arg3 Stack empty despite still trying to process operation.",
                            );
                            let arg2_value = value_stack.pop_back().expect(
                                "Drv arg2 Stack empty despite still trying to process operation.",
                            );
                            let arg3_value = value_stack.pop_back().expect(
                                "Drv arg1 Stack empty despite still trying to process operation.",
                            );
                            let _ = unit_builder.ins().drv(arg1_value, arg2_value, arg3_value);
                        }
                        _ => {
                            panic!("Unhandled opcode => {:?}", opcode);
                        }
                    }
                } else {
                    panic!("Unhandled symbol => {:?}", symbol)
                }
            }
        }
    }
}

fn process_arg_expr(expr: &Expr, type_expr_fifo: &mut LLHDTypeFIFO) {
    match expr {
        GenericExpr::Call(_, type_symbol, type_args) => {
            if *type_symbol == Symbol::new(LLHD_TYPE_VOID_FIELD) {
                type_expr_fifo.push_back(llhd::void_ty());
            } else if *type_symbol == Symbol::new(LLHD_TYPE_INT_FIELD) {
                if let GenericExpr::Lit(_, int_literal_value) = &type_args[0] {
                    match int_literal_value {
                        Literal::Int(iid) => {
                            type_expr_fifo.push_back(llhd::int_ty(
                                usize::try_from(*iid)
                                    .expect("Failure to convert egglog Int to usize."),
                            ));
                        }
                        _ => {}
                    }
                };
            } else if *type_symbol == Symbol::new(LLHD_TYPE_SIGNAL_FIELD) {
                process_arg_expr(&type_args[0], type_expr_fifo);
                let signal_info_expr = type_expr_fifo
                    .pop_back()
                    .expect("Stack empty despite still trying to process operation.");
                type_expr_fifo.push_back(llhd::signal_ty(signal_info_expr));
            }
        }
        _ => {}
    }
}

pub(crate) fn expr_to_unit_info(unit_expr: Expr) -> (UnitKind, UnitName, Signature) {
    let default_unit_info = (UnitKind::Entity, UnitName::Anonymous(0), Signature::new());
    let mut unit_info = default_unit_info.clone();
    match unit_expr {
        GenericExpr::Lit(_span, literal) => match literal {
            Literal::Int(iid) => (
                default_unit_info.0,
                UnitName::anonymous(
                    iid.try_into()
                        .expect("Failure to convert egglog Int to u32."),
                ),
                default_unit_info.2,
            ),
            Literal::Float(_fid) => default_unit_info,
            Literal::String(uname) => (
                default_unit_info.0,
                UnitName::Global(uname.to_string()),
                default_unit_info.2,
            ),
            Literal::Bool(_bid) => default_unit_info,
            Literal::Unit => default_unit_info,
        },
        GenericExpr::Var(_span, symbol) => (
            default_unit_info.0,
            UnitName::Global(symbol.to_string()),
            default_unit_info.2,
        ),
        GenericExpr::Call(_, symbol, info_exprs) => {
            if symbol == Symbol::new(LLHD_UNIT_FIELD) {
                if 5 < info_exprs.len() {
                    if let GenericExpr::Call(_, unit_sort_name, unit_kind_func) = &info_exprs[1] {
                        if *unit_sort_name == Symbol::new(LLHD_UNIT_KIND_DATATYPE) {
                            if let GenericExpr::Var(_, unit_kind_symbol) = &unit_kind_func[0] {
                                if *unit_kind_symbol == Symbol::new(LLHD_UNIT_ENTITY_FIELD) {
                                    unit_info.0 = UnitKind::Entity;
                                } else if *unit_kind_symbol == Symbol::new(LLHD_UNIT_FUNCTION_FIELD)
                                {
                                    unit_info.0 = UnitKind::Function;
                                } else if *unit_kind_symbol == Symbol::new(LLHD_UNIT_PROCESS_FIELD)
                                {
                                    unit_info.0 = UnitKind::Process;
                                }
                            };
                        }
                    };
                    if let GenericExpr::Lit(_, unit_name) = &info_exprs[2] {
                        let unit_name_no_prefix =
                            unit_name.to_string().replace(&['@', '%', ','][..], "");
                        unit_info.1 = UnitName::global(unit_name_no_prefix);
                    };

                    let mut input_args_expr_fifo: LLHDTypeFIFO = Default::default();
                    if let GenericExpr::Call(_, vec_of_sort_symbol, vec_args) = &info_exprs[3] {
                        if *vec_of_sort_symbol == Symbol::new(EGGLOG_VEC_OF_OP) {
                            for vec_arg in vec_args {
                                if let GenericExpr::Call(_, value_decl_symbol, arg_info) = vec_arg {
                                    if *value_decl_symbol == Symbol::new(LLHD_VALUE_FIELD) {
                                        if 2 == arg_info.len() {
                                            process_arg_expr(
                                                &arg_info[0],
                                                &mut input_args_expr_fifo,
                                            );
                                        }
                                    }
                                }
                            }
                        }
                    };
                    let mut output_args_expr_fifo: LLHDTypeFIFO = Default::default();
                    if let GenericExpr::Call(_, vec_of_sort_symbol, vec_args) = &info_exprs[4] {
                        if *vec_of_sort_symbol == Symbol::new(EGGLOG_VEC_OF_OP) {
                            for vec_arg in vec_args {
                                if let GenericExpr::Call(_, value_decl_symbol, arg_info) = vec_arg {
                                    if *value_decl_symbol == Symbol::new(LLHD_VALUE_FIELD) {
                                        if 2 == arg_info.len() {
                                            process_arg_expr(
                                                &arg_info[0],
                                                &mut output_args_expr_fifo,
                                            );
                                        }
                                    }
                                }
                            }
                        }
                    };
                    for unit_arg_expr in input_args_expr_fifo {
                        unit_info.2.add_input(unit_arg_expr);
                    }
                    for unit_arg_expr in output_args_expr_fifo {
                        unit_info.2.add_output(unit_arg_expr);
                    }
                    unit_info
                } else {
                    default_unit_info
                }
            } else {
                default_unit_info
            }
        }
    }
}

pub(crate) fn expr_to_unit_data(
    unit_expr: Expr,
    unit_kind: UnitKind,
    unit_name: UnitName,
    unit_sig: Signature,
) -> UnitData {
    let mut unit_data = UnitData::new(unit_kind, unit_name, unit_sig);
    let mut unit_builder = UnitBuilder::new_anonymous(&mut unit_data);
    let mut expr_fifo: ExprFIFO = Default::default();

    process_expr(&unit_expr, &mut expr_fifo);

    let mut value_stack: ValueStack = Default::default();
    let mut int_value_stack: IntValueStack = Default::default();
    let mut time_value_stack: TimeValueStack = Default::default();

    process_expr_fifo(
        expr_fifo,
        &mut value_stack,
        &mut int_value_stack,
        &mut time_value_stack,
        &mut unit_builder,
    );

    unit_data
}

fn type_sort() -> Command {
    Command::Sort(DUMMY_SPAN.clone(), Symbol::new(LLHD_TYPE_DATATYPE), None)
}

fn vec_ty_sort() -> Command {
    let ty_sort_symbol = Symbol::new(LLHD_VEC_TYPE_DATATYPE);
    let symbol_vec = Symbol::new(EGGLOG_VEC_SORT);
    let ty_sort = Symbol::new(LLHD_TYPE_DATATYPE);
    let ty_expr = Expr::Var(DUMMY_SPAN.clone(), ty_sort);
    Command::Sort(
        DUMMY_SPAN.clone(),
        ty_sort_symbol,
        Some((symbol_vec, vec![ty_expr])),
    )
}

fn type_functions() -> EgglogCommandList {
    let void_function = GenericCommand::Constructor {
        name: Symbol::new(LLHD_TYPE_VOID_FIELD),
        schema: Schema {
            input: vec![],
            output: Symbol::new(LLHD_TYPE_DATATYPE),
        },
        span: DUMMY_SPAN.clone(),
        cost: None,
        unextractable: false,
    };
    let time_function = GenericCommand::Constructor {
        name: Symbol::new(LLHD_TYPE_TIME_FIELD),
        schema: Schema {
            input: vec![],
            output: Symbol::new(LLHD_TYPE_DATATYPE),
        },
        span: DUMMY_SPAN.clone(),
        cost: None,
        unextractable: false,
    };
    let i64_sort = I64Sort;
    let int_function = GenericCommand::Constructor {
        name: Symbol::new(LLHD_TYPE_INT_FIELD),
        schema: Schema {
            input: vec![i64_sort.name()],
            output: Symbol::new(LLHD_TYPE_DATATYPE),
        },
        span: DUMMY_SPAN.clone(),
        cost: None,
        unextractable: false,
    };
    let enum_function = GenericCommand::Constructor {
        name: Symbol::new(LLHD_TYPE_ENUM_FIELD),
        schema: Schema {
            input: vec![i64_sort.name()],
            output: Symbol::new(LLHD_TYPE_DATATYPE),
        },
        span: DUMMY_SPAN.clone(),
        cost: None,
        unextractable: false,
    };
    let pointer_function = GenericCommand::Constructor {
        name: Symbol::new(LLHD_TYPE_POINTER_FIELD),
        schema: Schema {
            input: vec![LLHD_TYPE_DATATYPE.into()],
            output: Symbol::new(LLHD_TYPE_DATATYPE),
        },
        span: DUMMY_SPAN.clone(),
        cost: None,
        unextractable: false,
    };
    let signal_function = GenericCommand::Constructor {
        name: Symbol::new(LLHD_TYPE_SIGNAL_FIELD),
        schema: Schema {
            input: vec![LLHD_TYPE_DATATYPE.into()],
            output: Symbol::new(LLHD_TYPE_DATATYPE),
        },
        span: DUMMY_SPAN.clone(),
        cost: None,
        unextractable: false,
    };
    let array_function = GenericCommand::Constructor {
        name: Symbol::new(LLHD_TYPE_ARRAY_FIELD),
        schema: Schema {
            input: vec![i64_sort.name(), LLHD_TYPE_DATATYPE.into()],
            output: Symbol::new(LLHD_TYPE_DATATYPE),
        },
        span: DUMMY_SPAN.clone(),
        cost: None,
        unextractable: false,
    };
    let struct_function = GenericCommand::Constructor {
        name: Symbol::new(LLHD_TYPE_STRUCT_FIELD),
        schema: Schema {
            input: vec![LLHD_VEC_TYPE_DATATYPE.into()],
            output: Symbol::new(LLHD_TYPE_DATATYPE),
        },
        span: DUMMY_SPAN.clone(),
        cost: None,
        unextractable: false,
    };
    let func_function = GenericCommand::Constructor {
        name: Symbol::new(LLHD_TYPE_FUNC_FIELD),
        schema: Schema {
            input: vec![LLHD_VEC_TYPE_DATATYPE.into(), LLHD_TYPE_DATATYPE.into()],
            output: Symbol::new(LLHD_TYPE_DATATYPE),
        },
        span: DUMMY_SPAN.clone(),
        cost: None,
        unextractable: false,
    };
    let entity_function = GenericCommand::Constructor {
        name: Symbol::new(LLHD_TYPE_ENTITY_FIELD),
        schema: Schema {
            input: vec![LLHD_VEC_TYPE_DATATYPE.into(), LLHD_VEC_TYPE_DATATYPE.into()],
            output: Symbol::new(LLHD_TYPE_DATATYPE),
        },
        span: DUMMY_SPAN.clone(),
        cost: None,
        unextractable: false,
    };
    vec![
        void_function,
        time_function,
        int_function,
        enum_function,
        pointer_function,
        signal_function,
        array_function,
        struct_function,
        func_function,
        entity_function,
    ]
}

fn unit_kind_sort() -> Command {
    let entity_variant = Variant {
        span: DUMMY_SPAN.clone(),
        name: Symbol::new(LLHD_UNIT_ENTITY_FIELD),
        types: vec![],
        cost: None,
    };
    let function_variant = Variant {
        span: DUMMY_SPAN.clone(),
        name: Symbol::new(LLHD_UNIT_FUNCTION_FIELD),
        types: vec![],
        cost: None,
    };
    let process_variant = Variant {
        span: DUMMY_SPAN.clone(),
        name: Symbol::new(LLHD_UNIT_PROCESS_FIELD),
        types: vec![],
        cost: None,
    };
    let symbol = Symbol::new(LLHD_UNIT_KIND_DATATYPE);
    Command::Datatype {
        span: DUMMY_SPAN.clone(),
        name: symbol,
        variants: vec![entity_variant, function_variant, process_variant],
    }
}

fn value() -> Command {
    let i64_sort = I64Sort;
    let ty_datatype = Symbol::new(LLHD_TYPE_DATATYPE);
    let value_variant = Variant {
        span: DUMMY_SPAN.clone(),
        name: Symbol::new(LLHD_VALUE_FIELD),
        types: vec![ty_datatype, i64_sort.name()],
        cost: None,
    };
    let symbol = Symbol::new(LLHD_VALUE_DATATYPE);
    Command::Datatype {
        span: DUMMY_SPAN.clone(),
        name: symbol,
        variants: vec![value_variant],
    }
}

fn int_value() -> Command {
    let i64_sort = I64Sort;
    let int_value_variant = Variant {
        span: DUMMY_SPAN.clone(),
        name: Symbol::new(LLHD_INT_VALUE_FIELD),
        types: vec![i64_sort.name()],
        cost: None,
    };
    let symbol = Symbol::new(LLHD_INT_VALUE_DATATYPE);
    Command::Datatype {
        span: DUMMY_SPAN.clone(),
        name: symbol,
        variants: vec![int_value_variant],
    }
}

fn time_value() -> Command {
    let i64_sort = I64Sort;
    let time_value_variant = Variant {
        span: DUMMY_SPAN.clone(),
        name: Symbol::new(LLHD_TIME_VALUE_FIELD),
        types: vec![i64_sort.name()],
        cost: None,
    };
    let symbol = Symbol::new(LLHD_TIME_VALUE_DATATYPE);
    Command::Datatype {
        span: DUMMY_SPAN.clone(),
        name: symbol,
        variants: vec![time_value_variant],
    }
}

fn reg_mode() -> Command {
    let symbol = Symbol::new(LLHD_REGMODE_DATATYPE);
    Command::Datatype {
        span: DUMMY_SPAN.clone(),
        name: symbol,
        variants: vec![
            Variant {
                span: DUMMY_SPAN.clone(),
                name: Symbol::new(LLHD_REGMODE_FIELD_LOW),
                types: vec![],
                cost: None,
            },
            Variant {
                span: DUMMY_SPAN.clone(),
                name: Symbol::new(LLHD_REGMODE_FIELD_HIGH),
                types: vec![],
                cost: None,
            },
            Variant {
                span: DUMMY_SPAN.clone(),
                name: Symbol::new(LLHD_REGMODE_FIELD_RISE),
                types: vec![],
                cost: None,
            },
            Variant {
                span: DUMMY_SPAN.clone(),
                name: Symbol::new(LLHD_REGMODE_FIELD_FALL),
                types: vec![],
                cost: None,
            },
            Variant {
                span: DUMMY_SPAN.clone(),
                name: Symbol::new(LLHD_REGMODE_FIELD_BOTH),
                types: vec![],
                cost: None,
            },
        ],
    }
}

fn vec_value_sort() -> Command {
    let vec_sort_symbol = Symbol::new(LLHD_VEC_VALUE_DATATYPE);
    let symbol_vec = Symbol::new(EGGLOG_VEC_SORT);
    let value_sort = Symbol::new(LLHD_VALUE_DATATYPE);
    let value_expr = Expr::Var(DUMMY_SPAN.clone(), value_sort);
    Command::Sort(
        DUMMY_SPAN.clone(),
        vec_sort_symbol,
        Some((symbol_vec, vec![value_expr])),
    )
}

fn vec_regmode_sort() -> Command {
    let vec_sort_symbol = Symbol::new(LLHD_VEC_REGMODE_DATATYPE);
    let symbol_vec = Symbol::new(EGGLOG_VEC_SORT);
    let regmode_datatype = Symbol::new(LLHD_REGMODE_DATATYPE);
    let regmode_expr = Expr::Var(DUMMY_SPAN.clone(), regmode_datatype);
    Command::Sort(
        DUMMY_SPAN.clone(),
        vec_sort_symbol,
        Some((symbol_vec, vec![regmode_expr])),
    )
}

fn block() -> Command {
    let i64_sort = I64Sort;
    let block_variant = Variant {
        span: DUMMY_SPAN.clone(),
        name: Symbol::new(LLHD_BLOCK_FIELD),
        types: vec![i64_sort.name()],
        cost: None,
    };
    let symbol = Symbol::new(LLHD_BLOCK_DATATYPE);
    Command::Datatype {
        span: DUMMY_SPAN.clone(),
        name: symbol,
        variants: vec![block_variant],
    }
}

fn vec_block() -> Command {
    let vec_sort_symbol = Symbol::new(LLHD_VEC_BLOCK_DATATYPE);
    let symbol_vec = Symbol::new(EGGLOG_VEC_SORT);
    let vec_block_datatype = Symbol::new(LLHD_BLOCK_DATATYPE);
    let vec_block_expr = Expr::Var(DUMMY_SPAN.clone(), vec_block_datatype);
    Command::Sort(
        DUMMY_SPAN.clone(),
        vec_sort_symbol,
        Some((symbol_vec, vec![vec_block_expr])),
    )
}

fn ext_unit() -> Command {
    let i64_sort = I64Sort;
    let ext_unit_variant = Variant {
        span: DUMMY_SPAN.clone(),
        name: Symbol::new(LLHD_EXT_UNIT_FIELD),
        types: vec![i64_sort.name()],
        cost: None,
    };
    let symbol = Symbol::new(LLHD_EXT_UNIT_DATATYPE);
    Command::Datatype {
        span: DUMMY_SPAN.clone(),
        name: symbol,
        variants: vec![ext_unit_variant],
    }
}

fn unit_ctx() -> Command {
    let llhd_dfg_ctx_symbol = Symbol::new(LLHD_DFG_CTX_DATATYPE);
    let symbol_vec = Symbol::new(EGGLOG_VEC_SORT);
    let llhd_dfg_datatype = Symbol::new(LLHD_DFG_DATATYPE);
    let llhd_dfg_datatype_expr = Expr::Var(DUMMY_SPAN.clone(), llhd_dfg_datatype);
    Command::Sort(
        DUMMY_SPAN.clone(),
        llhd_dfg_ctx_symbol,
        Some((symbol_vec, vec![llhd_dfg_datatype_expr])),
    )
}

fn unit_def() -> Command {
    let i64_sort = I64Sort;
    let string_sort = StringSort;
    let unit_variant = Variant {
        span: DUMMY_SPAN.clone(),
        name: Symbol::new(LLHD_UNIT_FIELD),
        types: vec![
            i64_sort.name(),
            LLHD_UNIT_KIND_DATATYPE.into(),
            string_sort.name(),
            LLHD_VEC_VALUE_DATATYPE.into(),
            LLHD_VEC_VALUE_DATATYPE.into(),
            LLHD_DFG_CTX_DATATYPE.into(),
        ],
        cost: None,
    };
    let unit_decl_variant = Variant {
        span: DUMMY_SPAN.clone(),
        name: Symbol::new(LLHD_UNIT_DECL_FIELD),
        types: vec![
            i64_sort.name(),
            LLHD_UNIT_KIND_DATATYPE.into(),
            string_sort.name(),
            LLHD_VEC_VALUE_DATATYPE.into(),
            LLHD_VEC_VALUE_DATATYPE.into(),
        ],
        cost: None,
    };
    let symbol = Symbol::new(LLHD_UNIT_DFG_DATATYPE);
    Command::Datatype {
        span: DUMMY_SPAN.clone(),
        name: symbol,
        variants: vec![unit_variant, unit_decl_variant],
    }
}

pub(in crate::llhd_egraph) fn unit_types() -> EgglogCommandList {
    let mut llhd_types = vec![type_sort(), vec_ty_sort()];
    llhd_types.extend(type_functions());
    llhd_types.extend([
        unit_kind_sort(),
        value(),
        vec_value_sort(),
        block(),
        vec_block(),
        ext_unit(),
        time_value(),
        reg_mode(),
        vec_regmode_sort(),
    ]);
    llhd_types
}

pub(in crate::llhd_egraph) fn dfg() -> EgglogCommandList {
    vec![unit_ctx(), unit_def()]
}

#[cfg(test)]
mod tests;
