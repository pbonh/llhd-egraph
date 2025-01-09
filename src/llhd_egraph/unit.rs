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
mod tests {
    use egglog::ast::{
        GenericAction, GenericCommand, GenericExpr, GenericRunConfig, GenericSchedule, Symbol,
    };
    use egglog::{EGraph, TermDag};
    use itertools::Itertools;
    use llhd::ir::InstData;
    use llhd::table::TableKey;

    use super::*;
    use crate::llhd_egraph::datatype::LLHDEgglogSorts;

    #[test]
    fn build_egglog_program_from_unit() {
        let unit_data = utilities::build_entity_alpha(UnitName::anonymous(0));
        let unit = Unit::new(UnitId::new(0), &unit_data);
        let egglog_facts = LLHDEgglogFacts::from_unit(&unit);
        assert_eq!(
            1,
            egglog_facts.0.len(),
            "There should be 1 fact in program."
        );
        if let GenericCommand::Action(let_action) = &egglog_facts.0[0] {
            if let GenericAction::Let(_dummy, let_stmt_symbol, _let_stmt) = let_action {
                assert_eq!(
                    "unit_0",
                    let_stmt_symbol.to_string(),
                    "Let Stmt should match UnitName"
                );
            };
        };
    }

    #[test]
    #[ignore]
    fn llhd_egglog_dfg_expression_tree1() {
        let unit_data = utilities::build_entity_alpha(UnitName::anonymous(0));
        let unit = Unit::new(UnitId::new(0), &unit_data);
        let insts = LLHDUtils::iterate_unit_insts(&unit).collect_vec();
        let _value_refs = LLHDUtils::iterate_unit_value_defs(&unit).collect_vec();

        let const_int_1_inst = insts[0];
        let const_int_1_inst_data = &unit[const_int_1_inst.1];
        assert_eq!(
            Opcode::ConstInt,
            const_int_1_inst_data.opcode(),
            "Inst should be Const Int."
        );
        let const_int_2_inst = insts[1];
        let const_int_2_inst_data = &unit[const_int_2_inst.1];
        assert_eq!(
            Opcode::ConstInt,
            const_int_2_inst_data.opcode(),
            "Inst should be Const Int."
        );
        let add1_inst = insts[2];
        let add1_inst_data = &unit[add1_inst.1];
        assert_eq!(Opcode::Add, add1_inst_data.opcode(), "Inst should be Add.");
        let prb1_inst = insts[3];
        let prb1_inst_data = &unit[prb1_inst.1];
        assert_eq!(Opcode::Prb, prb1_inst_data.opcode(), "Inst should be Prb.");
        let add2_inst = insts[4];
        let add2_inst_data = &unit[add2_inst.1];
        assert_eq!(Opcode::Add, add2_inst_data.opcode(), "Inst should be Add.");

        let egglog_expr = from_unit(&unit);
        let expected_str = utilities::trim_expr_whitespace(indoc::indoc! {"
            (let unit_0 (LLHDUnit
                0
                (Entity )
                \"%0\"
                (vec-of (Value (Signal (IntTy 1)) 0) (Value (Signal (IntTy 1)) 1) (Value (Signal (IntTy 1)) 2))
                (vec-of (Value (Signal (IntTy 32)) 3))
                (Add 5 (IntTy 1)
                    (Add 3 (IntTy 1)
                        (ConstInt 1 (IntTy 1) \"i1 0\")
                        (ConstInt 2 (IntTy 1) \"i1 1\"))
                    (Prb 4 (IntTy 1) (ValueRef (Value (Signal (IntTy 1)) 2))))))
        "});
        assert_eq!(
            expected_str,
            egglog_expr.to_string(),
            "Generated LLHD Egglog expression doesn't match expected value."
        );
    }

    #[test]
    fn llhd_egglog_dfg_expression_tree2() {
        let module = utilities::load_llhd_module("2and_1or.llhd");
        let units = LLHDUtils::iterate_unit_ids(&module).collect_vec();
        let unit = module.unit(*units.first().unwrap());
        let insts = LLHDUtils::iterate_unit_insts(&unit).collect_vec();
        let _value_refs = LLHDUtils::iterate_unit_value_defs(&unit).collect_vec();

        let const_time_inst = insts[0];
        let const_time_inst_data = &unit[const_time_inst.1];
        assert_eq!(
            Opcode::ConstTime,
            const_time_inst_data.opcode(),
            "Inst should be Const Time."
        );
        let and1_inst = insts[1];
        let and1_inst_data = &unit[and1_inst.1];
        assert_eq!(Opcode::And, and1_inst_data.opcode(), "Inst should be And.");
        let and2_inst = insts[2];
        let and2_inst_data = &unit[and2_inst.1];
        assert_eq!(Opcode::And, and2_inst_data.opcode(), "Inst should be And.");
        let or1_inst = insts[3];
        let or1_inst_data = &unit[or1_inst.1];
        assert_eq!(Opcode::Or, or1_inst_data.opcode(), "Inst should be Or.");
        let drv_inst = insts[4];
        let drv_inst_data = &unit[drv_inst.1];
        assert_eq!(Opcode::Drv, drv_inst_data.opcode(), "Inst should be Drv.");

        let egglog_expr = from_unit(&unit);
        let expected_str = utilities::trim_expr_whitespace(indoc::indoc! {"
            (let unit_test_entity (LLHDUnit
                0
                (Entity )
                \"@test_entity\"
                (vec-of (Value (IntTy 1) 0) (Value (IntTy 1) 1) (Value (IntTy 1) 2) (Value (IntTy 1) 3))
                (vec-of (Value (Signal (IntTy 1)) 4)) (vec-of
                (Drv 5 (Void )
                    (ValueRef (Value (Signal (IntTy 1)) 4))
                    (Or 4 (IntTy 1)
                        (And 2 (IntTy 1)
                            (ValueRef (Value (IntTy 1) 0))
                            (ValueRef (Value (IntTy 1) 1)))
                        (And 3 (IntTy 1)
                            (ValueRef (Value (IntTy 1) 2))
                            (ValueRef (Value (IntTy 1) 3))))
                    (ConstTime 1 (Time ) \"0s 1e\")))))
        "});
        assert_eq!(
            expected_str,
            egglog_expr.to_string(),
            "Generated LLHD Egglog expression doesn't match expected value."
        );
    }

    #[test]
    fn llhd_egglog_dfg_expression_tree3() {
        let module = utilities::load_llhd_module("1ent_absorption_div_extract.before.llhd");
        let units = LLHDUtils::iterate_unit_ids(&module).collect_vec();
        let unit = module.unit(*units.first().unwrap());
        let insts = LLHDUtils::iterate_unit_insts(&unit).collect_vec();
        let _value_refs = LLHDUtils::iterate_unit_value_defs(&unit).collect_vec();

        let const_time1_inst = insts[0];
        let const_time1_inst_data = &unit[const_time1_inst.1];
        assert_eq!(
            Opcode::ConstTime,
            const_time1_inst_data.opcode(),
            "Inst should be Const Time."
        );
        let and1_inst = insts[1];
        let and1_inst_data = &unit[and1_inst.1];
        assert_eq!(Opcode::And, and1_inst_data.opcode(), "Inst should be And.");
        let and2_inst = insts[2];
        let and2_inst_data = &unit[and2_inst.1];
        assert_eq!(Opcode::And, and2_inst_data.opcode(), "Inst should be And.");
        let and3_inst = insts[3];
        let and3_inst_data = &unit[and3_inst.1];
        assert_eq!(Opcode::And, and3_inst_data.opcode(), "Inst should be And.");
        let or1_inst = insts[4];
        let or1_inst_data = &unit[or1_inst.1];
        assert_eq!(Opcode::Or, or1_inst_data.opcode(), "Inst should be Or.");
        let drv1_inst = insts[5];
        let drv1_inst_data = &unit[drv1_inst.1];
        assert_eq!(Opcode::Drv, drv1_inst_data.opcode(), "Inst should be Drv.");
        let const_time2_inst = insts[6];
        let const_time2_inst_data = &unit[const_time2_inst.1];
        assert_eq!(
            Opcode::ConstTime,
            const_time2_inst_data.opcode(),
            "Inst should be Const Time."
        );
        let and4_inst = insts[7];
        let and4_inst_data = &unit[and4_inst.1];
        assert_eq!(Opcode::And, and4_inst_data.opcode(), "Inst should be And.");
        let and5_inst = insts[8];
        let and5_inst_data = &unit[and5_inst.1];
        assert_eq!(Opcode::And, and5_inst_data.opcode(), "Inst should be And.");
        let or2_inst = insts[9];
        let or2_inst_data = &unit[or2_inst.1];
        assert_eq!(Opcode::Or, or2_inst_data.opcode(), "Inst should be Or.");
        let drv2_inst = insts[10];
        let drv2_inst_data = &unit[drv2_inst.1];
        assert_eq!(Opcode::Drv, drv2_inst_data.opcode(), "Inst should be Drv.");

        let egglog_expr = from_unit(&unit);
        let expected_str = utilities::trim_expr_whitespace(indoc::indoc! {"
            (let unit_test_entity (LLHDUnit
                0
                (Entity )
                \"@test_entity\"
                (vec-of (Value (IntTy 1) 0) (Value (IntTy 1) 1) (Value (IntTy 1) 2) (Value (IntTy 1) 3) (Value (IntTy 1) 4) (Value (IntTy 1) 5) (Value (IntTy 1) 6))
                (vec-of (Value (Signal (IntTy 1)) 7) (Value (Signal (IntTy 1)) 8)) (vec-of
                (Drv 11 (Void )
                    (ValueRef (Value (Signal (IntTy 1)) 8))
                    (Or 10 (IntTy 1)
                        (And 8 (IntTy 1)
                            (ValueRef (Value (IntTy 1) 4))
                            (ValueRef (Value (IntTy 1) 5)))
                        (And 9 (IntTy 1)
                            (ValueRef (Value (IntTy 1) 6))
                            (ValueRef (Value (IntTy 1) 5))))
                    (ConstTime 7 (Time ) \"0s 1e\"))
                (Drv 6 (Void )
                    (ValueRef (Value (Signal (IntTy 1)) 7))
                    (Or 5 (IntTy 1)
                        (And 2 (IntTy 1)
                            (ValueRef (Value (IntTy 1) 0))
                            (ValueRef (Value (IntTy 1) 2)))
                        (And 4 (IntTy 1)
                            (And 3 (IntTy 1)
                                (ValueRef (Value (IntTy 1) 0))
                                (ValueRef (Value (IntTy 1) 2)))
                            (ValueRef (Value (IntTy 1) 2))))
                    (ConstTime 1 (Time ) \"0s 1e\"))
            )))
        "});
        assert_eq!(
            expected_str,
            egglog_expr.to_string(),
            "Generated LLHD Egglog expression doesn't match expected value."
        );
    }

    #[test]
    fn llhd_egglog_dfg_expression_tree_dual_output() {
        let module = utilities::load_llhd_module("dual_outputs_single_dfg.llhd");
        let units = LLHDUtils::iterate_unit_ids(&module).collect_vec();
        let unit = module.unit(*units.first().unwrap());
        let egglog_expr = from_unit(&unit);
        let expected_str = utilities::trim_expr_whitespace(indoc::indoc! {"
            (let unit_test_entity (LLHDUnit
                0 (Entity )
                \"@test_entity\"
                (vec-of (Value (IntTy 1) 0) (Value (IntTy 1) 1))
                (vec-of (Value (Signal (IntTy 1)) 2) (Value (Signal (IntTy 1)) 3))
                (vec-of
                    (Drv 4 (Void )
                        (ValueRef (Value (Signal (IntTy 1)) 3))
                        (And 2 (IntTy 1)
                            (ValueRef (Value (IntTy 1) 0))
                            (ValueRef (Value (IntTy 1) 1)))
                        (ConstTime 1 (Time ) \"0s 1e\"))
                    (Drv 3 (Void )
                        (ValueRef (Value (Signal (IntTy 1)) 2))
                        (And 2 (IntTy 1)
                            (ValueRef (Value (IntTy 1) 0))
                            (ValueRef (Value (IntTy 1) 1)))
                        (ConstTime 1 (Time ) \"0s 1e\")))
            ))
        "});
        assert_eq!(
            expected_str,
            egglog_expr.to_string(),
            "Generated LLHD Egglog expression doesn't match expected value."
        );
    }

    // #[test]
    // fn llhd_egglog_unit_from_expr() {
    //     let unit_str = utilities::trim_expr_whitespace(indoc::indoc! {"
    //         (let unit_test_entity (LLHDUnit
    //             0
    //             (Entity)
    //             \"@test_entity\"
    //             (vec-of (Value (IntTy 1) 0) (Value (IntTy 1) 1) (Value (IntTy 1) 2) (Value (IntTy 1) 3))
    //             (vec-of (Value (Signal (IntTy 1)) 4))
    //             (Drv 5 (Void)
    //                 (ValueRef (Value (Signal (IntTy 1)) 4))
    //                 (Or 4 (IntTy 1)
    //                     (And 2 (IntTy 1)
    //                         (ValueRef (Value (IntTy 1) 0))
    //                         (ValueRef (Value (IntTy 1) 1)))
    //                     (And 3 (IntTy 1)
    //                         (ValueRef (Value (IntTy 1) 2))
    //                         (ValueRef (Value (IntTy 1) 3))))
    //                 (ConstTime 1 (Time) \"0s 1e\"))))
    //     "});
    //         expr_to_unit_data(
    //             extracted_expr,
    //             unit_kind_extract,
    //             unit_name_extract,
    //             unit_sig_extract,
    //         )
    // }

    #[test]
    fn llhd_rewrite_egglog_program() {
        let mut test_module = utilities::load_llhd_module("2and_1or_common.llhd");
        let test_unit_id = LLHDUtils::iterate_unit_ids(&test_module).collect_vec()[0];
        let rewrite_module = |module: &Module| {
            let llhd_dfg_sort = LLHDEgglogSorts::llhd_dfg();
            let mut egraph = EGraph::default();
            let _egraph_msgs_datatypes = egraph.run_program(llhd_dfg_sort.into());
            let _egraph_msgs_rules =
                utilities::load_egraph_rewrite_rules("llhd_div_extract.egg", &mut egraph);
            assert_eq!(
                0,
                egraph.num_tuples(),
                "There should be 0 facts initially in the egraph."
            );

            let module_facts = LLHDEgglogFacts::from_module(module);
            if let Err(egraph_run_facts_err) = egraph.run_program(module_facts.into()) {
                panic!(
                    "EGraph failed to add facts. ERROR: {:?}",
                    egraph_run_facts_err
                );
            }
            assert!(
                egraph
                    .get_overall_run_report()
                    .num_matches_per_rule
                    .values()
                    .next()
                    .is_none(),
                "There should be no matches yet, as the rule schedule hasn't run yet."
            );

            assert_eq!(
                20,
                egraph.num_tuples(),
                "There should be 20 facts remaining in the egraph."
            );

            let div_extract_ruleset_symbol = Symbol::new("div-ext");
            let div_extract_schedule = GenericRunConfig::<Symbol, Symbol> {
                ruleset: div_extract_ruleset_symbol,
                until: None,
            };
            let schedule_cmd = GenericCommand::RunSchedule(GenericSchedule::Run(
                DUMMY_SPAN.clone(),
                div_extract_schedule,
            ));
            let egraph_run_schedule = egraph.run_program(vec![schedule_cmd]);
            assert!(
                egraph_run_schedule.is_ok(),
                "EGraph failed to run schedule."
            );
            assert_eq!(
                22,
                egraph.num_tuples(),
                "There should be 22 facts remaining in the egraph(new 'And', new 'Or' nodes)."
            );

            let egraph_run_rules_matches = egraph
                .get_overall_run_report()
                .num_matches_per_rule
                .values()
                .next()
                .unwrap();
            assert_eq!(
                1, *egraph_run_rules_matches,
                "There should be 1 match for divisor extraction rewrite rule."
            );

            let test_entity_symbol = Symbol::new("unit_test_entity");
            let extract_cmd = GenericCommand::QueryExtract {
                span: DUMMY_SPAN.clone(),
                variants: 0,
                expr: GenericExpr::Var(DUMMY_SPAN.clone(), test_entity_symbol),
            };
            if let Err(egraph_extract_expr_msg) = egraph.run_program(vec![extract_cmd]) {
                panic!(
                    "EGraph failed to extract expression. ERROR: {:?}",
                    egraph_extract_expr_msg
                );
            }

            let mut extracted_termdag = TermDag::default();
            let (unit_sort, test_unit_symbol_value) = egraph
                .eval_expr(&GenericExpr::Var(DUMMY_SPAN.clone(), test_entity_symbol))
                .unwrap();
            let (_unit_cost, unit_term) =
                egraph.extract(test_unit_symbol_value, &mut extracted_termdag, &unit_sort);
            let extracted_expr = extracted_termdag.term_to_expr(&unit_term, DUMMY_SPAN.clone());
            assert!(
                matches!(extracted_expr, GenericExpr::Call { .. }),
                "Top level expression should be a call."
            );
            let expected_str = utilities::trim_expr_whitespace(indoc::indoc! {"
                (LLHDUnit 0 (Entity ) \"@test_entity\"
                    (vec-of (Value (IntTy 1) 0) (Value (IntTy 1) 1) (Value (IntTy 1) 2))
                    (vec-of (Value (Signal (IntTy 1)) 3)) (vec-of
                    (Drv 5 (Void )
                        (ValueRef (Value (Signal (IntTy 1)) 3))
                        (And 4 (IntTy 1)
                            (Or 2 (IntTy 1)
                                (ValueRef (Value (IntTy 1) 0))
                                (ValueRef (Value (IntTy 1) 2)))
                            (ValueRef (Value (IntTy 1) 1)))
                        (ConstTime 1 (Time ) \"0s 1e\"))))
            "});
            assert_eq!(expected_str, extracted_expr.to_string());
            let (unit_kind_extract, unit_name_extract, unit_sig_extract) =
                expr_to_unit_info(extracted_expr.clone());
            assert!(matches!(unit_kind_extract, UnitKind::Entity));
            if let UnitName::Global(uname) = unit_name_extract.clone() {
                assert_eq!(uname, "\"test_entity\"");
            } else {
                panic!("UnitName is of incorrect type, should be UnitName::Global");
            }
            let unit_sig_extract_inputs = unit_sig_extract.inputs().collect_vec();
            assert_eq!(3, unit_sig_extract_inputs.len());
            let input_arg1_ty = unit_sig_extract.arg_type(unit_sig_extract_inputs[0]);
            let input_arg2_ty = unit_sig_extract.arg_type(unit_sig_extract_inputs[1]);
            let input_arg3_ty = unit_sig_extract.arg_type(unit_sig_extract_inputs[2]);
            assert_eq!(*input_arg1_ty, *llhd::ty::int_ty(1));
            assert_eq!(*input_arg2_ty, *llhd::ty::int_ty(1));
            assert_eq!(*input_arg3_ty, *llhd::ty::int_ty(1));
            let unit_sig_extract_outputs = unit_sig_extract.outputs().collect_vec();
            assert_eq!(1, unit_sig_extract_outputs.len());
            let output_arg1_ty = unit_sig_extract.arg_type(unit_sig_extract_outputs[0]);
            assert_eq!(*output_arg1_ty, *llhd::ty::signal_ty(llhd::ty::int_ty(1)));
            expr_to_unit_data(
                extracted_expr,
                unit_kind_extract,
                unit_name_extract,
                unit_sig_extract,
            )
        };
        test_module[test_unit_id] = rewrite_module(&test_module);
        let new_unit_data = test_module.unit(test_unit_id);
        let new_unit_insts = new_unit_data.all_insts().collect_vec();
        assert_eq!(
            5,
            new_unit_insts.len(),
            "There should be 5 Insts in rewritten Unit."
        );
        let inst_const_time_id = new_unit_insts[0];
        let inst_const_time_data = new_unit_data[inst_const_time_id].clone();
        assert_eq!(
            Opcode::ConstTime,
            inst_const_time_data.opcode(),
            "First Inst should be `const time`."
        );
        let inst_or1_id = new_unit_insts[1];
        let inst_or1_data = new_unit_data[inst_or1_id].clone();
        assert_eq!(
            Opcode::Or,
            inst_or1_data.opcode(),
            "Second Inst should be `or`."
        );
        let inst_and1_id = new_unit_insts[2];
        let inst_and1_data = new_unit_data[inst_and1_id].clone();
        assert_eq!(
            Opcode::And,
            inst_and1_data.opcode(),
            "Third Inst should be `And`."
        );
        let inst_drv1_id = new_unit_insts[3];
        let inst_drv1_data = new_unit_data[inst_drv1_id].clone();
        assert_eq!(
            Opcode::Drv,
            inst_drv1_data.opcode(),
            "Fourth Inst should be `drv`."
        );
        let inst_null_id = new_unit_insts[4];
        let inst_null_data = new_unit_data[inst_null_id].clone();
        assert!(
            matches!(inst_null_data, InstData::Nullary { .. }),
            "Fifth Inst should be Null instruction(doesn't actually exist)."
        );
    }

    #[test]
    fn llhd_egglog_value_datatypes() {
        let value_datatype = value();
        let expected_str = "(datatype LLHDValue (Value LLHDTy i64))".to_owned();
        assert_eq!(
            expected_str,
            value_datatype.to_string(),
            "Datatype should be named 'LLHDValue' and should have 1 field named (Value i64)."
        );
        let int_value_datatype = int_value();
        let int_expected_str = "(datatype LLHDIntValue (IntValue i64))".to_owned();
        assert_eq!(
            int_expected_str,
            int_value_datatype.to_string(),
            "Datatype should be named 'LLHDIntValue' and should have 1 field named (IntValue i64)."
        );
        let time_value_datatype = time_value();
        let time_expected_str = "(datatype LLHDTimeValue (TimeValue i64))".to_owned();
        assert_eq!(
            time_expected_str,
            time_value_datatype.to_string(),
            "Datatype should be named 'LLHDTimeValue' and should have 1 field named (TimeValue \
             i64)."
        );
        let reg_mode_datatype = reg_mode();
        let reg_mode_expected_str = utilities::trim_expr_whitespace(indoc::indoc! {"
            (datatype LLHDRegMode
                (Low)
                (High)
                (Rise)
                (Fall)
                (Both))
        "});
        assert_eq!(
            reg_mode_expected_str,
            reg_mode_datatype.to_string(),
            "Datatype should be named 'LLHDRegMode' and should have 5 field names."
        );
    }

    #[test]
    fn llhd_egglog_vec_sort() {
        let vec_sort = vec_value_sort();
        let expected_str = "(sort LLHDVecValue (Vec LLHDValue))".to_owned();
        assert_eq!(
            expected_str,
            vec_sort.to_string(),
            "Sort should be named 'LLHDVecValue' and should have 1 field named (Vec i64)."
        );
        let vec_regmode_sort = vec_regmode_sort();
        let vec_regmode_expected_str = "(sort LLHDVecRegMode (Vec LLHDRegMode))".to_owned();
        assert_eq!(
            vec_regmode_expected_str,
            vec_regmode_sort.to_string(),
            "Sort should be named 'LLHDVecRegMode' and should have 1 field named (Vec \
             LLHDRegMode)."
        );
    }

    #[test]
    fn llhd_egglog_block_datatypes() {
        let block_datatype = block();
        let expected_str = "(datatype LLHDBlock (Block i64))".to_owned();
        assert_eq!(
            expected_str,
            block_datatype.to_string(),
            "Datatype should be named 'LLHDBlock' and should have 1 field named (Block i64)."
        );
    }

    #[test]
    fn llhd_egglog_vec_block_sort() {
        let block_datatype = vec_block();
        let expected_str = "(sort LLHDVecBlock (Vec LLHDBlock))".to_owned();
        assert_eq!(
            expected_str,
            block_datatype.to_string(),
            "Datatype should be named 'LLHDVecBlock' and should have 1 field named (Vec \
             LLHDBlock)."
        );
    }

    #[test]
    fn llhd_egglog_ext_unit_datatypes() {
        let ext_unit_datatype = ext_unit();
        let expected_str = "(datatype LLHDExtUnit (ExtUnit i64))".to_owned();
        assert_eq!(
            expected_str,
            ext_unit_datatype.to_string(),
            "Datatype should be named 'LLHDExtUnit' and should have 1 field named (ExtUnit i64)."
        );
    }
}
