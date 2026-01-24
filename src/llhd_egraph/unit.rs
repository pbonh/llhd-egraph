use std::collections::{HashSet, VecDeque};
use std::sync::Arc;

use egglog::ast::{
    Action, Command, Expr, GenericCommand, GenericExpr, Literal, Schema, Symbol, Variant,
};
use egglog::sort::{I64Sort, Sort, StringSort};
use itertools::Itertools;
use llhd::ir::prelude::*;
use llhd::ir::{ExtUnit, InstData, Opcode, ValueData};
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

type ExprWithIDFIFO = VecDeque<(Option<Inst>, Expr)>;
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
    let unit_expr_variant = if unit_kind == UnitKind::Entity {
        unit_root_variant_symbol()
    } else {
        Symbol::new(LLHD_UNIT_WITH_CFG_FIELD)
    };
    let mut unit_expr_vec = vec![
        unit_id_expr,
        unit_kind_expr,
        unit_name_expr,
        unit_input_sig_expr,
        unit_output_sig_expr,
        unit_ctx_expr,
    ];
    if unit_kind != UnitKind::Entity {
        unit_expr_vec.push(cfg_skeleton_expr(unit));
    }

    let unit_expr = GenericExpr::Call(DUMMY_SPAN.clone(), unit_expr_variant, unit_expr_vec);
    Action::Let(DUMMY_SPAN.clone(), unit_symbol(*unit), unit_expr)
}

fn cfg_skeleton_expr(unit: &Unit<'_>) -> Expr {
    let block_skeletons = unit
        .blocks()
        .map(|bb| block_skeleton_expr(unit, bb))
        .collect_vec();
    let vec_expr = Expr::Call(
        DUMMY_SPAN.clone(),
        Symbol::new(EGGLOG_VEC_OF_OP),
        block_skeletons,
    );
    GenericExpr::Call(
        DUMMY_SPAN.clone(),
        Symbol::new(LLHD_CFG_SKELETON_FIELD),
        vec![vec_expr],
    )
}

fn block_skeleton_expr(unit: &Unit<'_>, bb: Block) -> Expr {
    let block_expr = block_expr(bb);
    let value_defs = unit
        .insts(bb)
        .filter_map(|inst| unit.get_inst_result(inst))
        .map(|value| value_def_expr(unit.value_type(value), value))
        .collect_vec();
    let value_vec_expr = Expr::Call(
        DUMMY_SPAN.clone(),
        Symbol::new(EGGLOG_VEC_OF_OP),
        value_defs,
    );
    let effect_vec_expr = Expr::Call(
        DUMMY_SPAN.clone(),
        Symbol::new(EGGLOG_VEC_OF_OP),
        unit.insts(bb)
            .filter_map(|inst| effect_expr(unit, inst, &unit[inst]))
            .collect_vec(),
    );
    let terminator_expr = terminator_expr(unit, unit.terminator(bb), &unit[unit.terminator(bb)]);
    GenericExpr::Call(
        DUMMY_SPAN.clone(),
        Symbol::new(LLHD_BLOCK_SKELETON_FIELD),
        vec![block_expr, value_vec_expr, effect_vec_expr, terminator_expr],
    )
}

fn block_expr(bb: Block) -> Expr {
    let block_id = Literal::Int(
        i64::try_from(bb.index()).expect("Out-of-bound value for usize -> i64 conversion."),
    );
    let block_id_expr = Expr::Lit(DUMMY_SPAN.clone(), block_id);
    GenericExpr::Call(
        DUMMY_SPAN.clone(),
        Symbol::new(LLHD_BLOCK_FIELD),
        vec![block_id_expr],
    )
}

fn ext_unit_expr(ext_unit: ExtUnit) -> Expr {
    let unit_id = Literal::Int(
        i64::try_from(ext_unit.index()).expect("Out-of-bound value for usize -> i64 conversion."),
    );
    let unit_id_expr = Expr::Lit(DUMMY_SPAN.clone(), unit_id);
    GenericExpr::Call(
        DUMMY_SPAN.clone(),
        Symbol::new(LLHD_EXT_UNIT_FIELD),
        vec![unit_id_expr],
    )
}

fn vec_value_expr(unit: &Unit<'_>, values: impl IntoIterator<Item = Value>) -> Expr {
    let value_defs = values
        .into_iter()
        .map(|value| value_def_expr(unit.value_type(value), value))
        .collect_vec();
    GenericExpr::Call(
        DUMMY_SPAN.clone(),
        Symbol::new(EGGLOG_VEC_OF_OP),
        value_defs,
    )
}

fn dfg_value_expr(unit: &Unit<'_>, value: Value) -> Expr {
    match &unit[value] {
        ValueData::Inst { ty, inst } => inst_expr(unit, *inst, ty.clone(), &unit[*inst]),
        ValueData::Arg { ty, arg } => value_ref_expr(ty.clone(), *arg),
        ValueData::Placeholder { ty } => value_def_expr(ty.clone(), value),
        ValueData::Invalid => panic!("Invalid value in CFG skeleton."),
    }
}

fn effect_expr(unit: &Unit<'_>, inst: Inst, inst_data: &InstData) -> Option<Expr> {
    let opcode = inst_data.opcode();
    match inst_data {
        InstData::Unary { args, .. } => match opcode {
            Opcode::Sig => Some(effect_unary(
                unit.inst_type(inst),
                args[0],
                LLHD_EFFECT_FIELD_SIG,
                unit,
            )),
            Opcode::Var => Some(effect_unary(
                unit.value_type(args[0]),
                args[0],
                LLHD_EFFECT_FIELD_VAR,
                unit,
            )),
            Opcode::Ld => Some(effect_unary(
                unit.inst_type(inst),
                args[0],
                LLHD_EFFECT_FIELD_LD,
                unit,
            )),
            Opcode::RetValue => None,
            _ => None,
        },
        InstData::Binary { args, .. } => match opcode {
            Opcode::St => Some(effect_binary(
                unit.value_type(args[1]),
                args[0],
                args[1],
                LLHD_EFFECT_FIELD_ST,
                unit,
            )),
            _ => None,
        },
        InstData::Ternary { args, .. } => match opcode {
            Opcode::Drv => Some(effect_ternary(
                unit.value_type(args[0]),
                args[0],
                args[1],
                args[2],
                LLHD_EFFECT_FIELD_DRV,
                unit,
            )),
            _ => None,
        },
        InstData::Quaternary { args, .. } => match opcode {
            Opcode::DrvCond => Some(effect_quaternary(
                unit.value_type(args[0]),
                args[0],
                args[1],
                args[2],
                args[3],
                LLHD_EFFECT_FIELD_DRVCOND,
                unit,
            )),
            _ => None,
        },
        InstData::Call {
            unit: ext_unit,
            ins,
            args,
            ..
        } => match opcode {
            Opcode::Call => Some(effect_call(
                unit.inst_type(inst),
                *ext_unit,
                *ins,
                args,
                LLHD_EFFECT_FIELD_CALL,
                unit,
            )),
            Opcode::Inst => Some(effect_call(
                unit.inst_type(inst),
                *ext_unit,
                *ins,
                args,
                LLHD_EFFECT_FIELD_INST,
                unit,
            )),
            _ => None,
        },
        _ => None,
    }
}

fn effect_unary(effect_ty: Type, arg: Value, effect_field: &str, unit: &Unit<'_>) -> Expr {
    let ty_expr = ty_expr(&effect_ty);
    let dfg_expr = dfg_value_expr(unit, arg);
    GenericExpr::Call(
        DUMMY_SPAN.clone(),
        Symbol::new(effect_field),
        vec![ty_expr, dfg_expr],
    )
}

fn effect_binary(
    effect_ty: Type,
    arg0: Value,
    arg1: Value,
    effect_field: &str,
    unit: &Unit<'_>,
) -> Expr {
    let ty_expr = ty_expr(&effect_ty);
    let dfg_expr0 = dfg_value_expr(unit, arg0);
    let dfg_expr1 = dfg_value_expr(unit, arg1);
    GenericExpr::Call(
        DUMMY_SPAN.clone(),
        Symbol::new(effect_field),
        vec![ty_expr, dfg_expr0, dfg_expr1],
    )
}

fn effect_ternary(
    effect_ty: Type,
    arg0: Value,
    arg1: Value,
    arg2: Value,
    effect_field: &str,
    unit: &Unit<'_>,
) -> Expr {
    let ty_expr = ty_expr(&effect_ty);
    let dfg_expr0 = dfg_value_expr(unit, arg0);
    let dfg_expr1 = dfg_value_expr(unit, arg1);
    let dfg_expr2 = dfg_value_expr(unit, arg2);
    GenericExpr::Call(
        DUMMY_SPAN.clone(),
        Symbol::new(effect_field),
        vec![ty_expr, dfg_expr0, dfg_expr1, dfg_expr2],
    )
}

fn effect_quaternary(
    effect_ty: Type,
    arg0: Value,
    arg1: Value,
    arg2: Value,
    arg3: Value,
    effect_field: &str,
    unit: &Unit<'_>,
) -> Expr {
    let ty_expr = ty_expr(&effect_ty);
    let dfg_expr0 = dfg_value_expr(unit, arg0);
    let dfg_expr1 = dfg_value_expr(unit, arg1);
    let dfg_expr2 = dfg_value_expr(unit, arg2);
    let dfg_expr3 = dfg_value_expr(unit, arg3);
    GenericExpr::Call(
        DUMMY_SPAN.clone(),
        Symbol::new(effect_field),
        vec![ty_expr, dfg_expr0, dfg_expr1, dfg_expr2, dfg_expr3],
    )
}

fn effect_call(
    effect_ty: Type,
    ext_unit: ExtUnit,
    ins: u16,
    args: &[Value],
    effect_field: &str,
    unit: &Unit<'_>,
) -> Expr {
    let ty_expr = ty_expr(&effect_ty);
    let ext_expr = ext_unit_expr(ext_unit);
    let ins_expr = Expr::Lit(DUMMY_SPAN.clone(), Literal::Int(i64::from(ins)));
    let args_expr = vec_value_expr(unit, args.iter().copied());
    GenericExpr::Call(
        DUMMY_SPAN.clone(),
        Symbol::new(effect_field),
        vec![ty_expr, ext_expr, ins_expr, args_expr],
    )
}

fn terminator_expr(unit: &Unit<'_>, _inst: Inst, inst_data: &InstData) -> Expr {
    match inst_data {
        InstData::Jump {
            opcode: Opcode::Br,
            bbs,
        } => GenericExpr::Call(
            DUMMY_SPAN.clone(),
            Symbol::new(LLHD_TERM_FIELD_BR),
            vec![block_expr(bbs[0])],
        ),
        InstData::Branch {
            opcode: Opcode::BrCond,
            args,
            bbs,
        } => {
            let cond_ty = ty_expr(&unit.value_type(args[0]));
            let cond_expr = dfg_value_expr(unit, args[0]);
            GenericExpr::Call(
                DUMMY_SPAN.clone(),
                Symbol::new(LLHD_TERM_FIELD_BRCOND),
                vec![cond_ty, cond_expr, block_expr(bbs[0]), block_expr(bbs[1])],
            )
        }
        InstData::Wait { opcode, bbs, args } => {
            let values_expr = vec_value_expr(unit, args.iter().copied());
            let term_symbol = if *opcode == Opcode::WaitTime {
                LLHD_TERM_FIELD_WAITTIME
            } else {
                LLHD_TERM_FIELD_WAIT
            };
            GenericExpr::Call(
                DUMMY_SPAN.clone(),
                Symbol::new(term_symbol),
                vec![block_expr(bbs[0]), values_expr],
            )
        }
        InstData::Nullary {
            opcode: Opcode::Halt,
        } => GenericExpr::Call(
            DUMMY_SPAN.clone(),
            Symbol::new(LLHD_TERM_FIELD_HALT),
            vec![],
        ),
        InstData::Nullary {
            opcode: Opcode::Ret,
        } => GenericExpr::Call(DUMMY_SPAN.clone(), Symbol::new(LLHD_TERM_FIELD_RET), vec![]),
        InstData::Unary {
            opcode: Opcode::RetValue,
            args,
        } => {
            let value_ty = ty_expr(&unit.value_type(args[0]));
            let value_expr = dfg_value_expr(unit, args[0]);
            GenericExpr::Call(
                DUMMY_SPAN.clone(),
                Symbol::new(LLHD_TERM_FIELD_RETVALUE),
                vec![value_ty, value_expr],
            )
        }
        _ => panic!("Unsupported terminator for CFG skeleton."),
    }
}

fn process_expr(expr: &Expr, expr_fifo: &mut ExprWithIDFIFO) {
    match expr {
        GenericExpr::Lit(_span, literal) => {
            expr_fifo.push_front((None, Expr::Lit(DUMMY_SPAN.clone(), literal.to_owned())));
        }
        GenericExpr::Var(_span, symbol) => {
            expr_fifo.push_front((None, Expr::Var(DUMMY_SPAN.clone(), symbol.to_owned())));
        }
        GenericExpr::Call(_, symbol, children) => {
            if opcode::get_symbol_opcode(symbol).is_some() {
                if let GenericExpr::Lit(_span, llhd_literal) = &children[0] {
                    let inst_id: Inst = literal_llhd_inst_id(llhd_literal);
                    expr_fifo.push_front((
                        Some(inst_id),
                        Expr::Call(DUMMY_SPAN.clone(), symbol.to_owned(), vec![]),
                    ));
                    for dep in children.iter().skip(2) {
                        process_expr(dep, expr_fifo);
                    }
                } else {
                    panic!("Unhandled Expr type in LLHD DFG S-expression: {:?}", expr);
                }
            } else if Symbol::new(LLHD_TYPE_INT_FIELD) == *symbol {
            } else {
                for dep in children.iter() {
                    process_expr(dep, expr_fifo);
                }
            }
        }
    }
}

fn process_expr_fifo_opcode(
    opcode: Opcode,
    value_stack: &mut ValueStack,
    time_value_stack: &mut TimeValueStack,
    unit_builder: &mut UnitBuilder,
) {
    match opcode {
        Opcode::Or => {
            let arg1_value = value_stack
                .pop_back()
                .expect("Or arg2 Stack empty despite still trying to process operation.");
            let arg2_value = value_stack
                .pop_back()
                .expect("Or arg1 Stack empty despite still trying to process operation.");
            value_stack.push_back(unit_builder.ins().or(arg1_value, arg2_value));
        }
        Opcode::And => {
            let arg1_value = value_stack
                .pop_back()
                .expect("And arg2 Stack empty despite still trying to process operation.");
            let arg2_value = value_stack
                .pop_back()
                .expect("And arg1 Stack empty despite still trying to process operation.");
            value_stack.push_back(unit_builder.ins().and(arg1_value, arg2_value));
        }
        Opcode::ConstTime => {
            let arg1_value = time_value_stack
                .pop_back()
                .expect("ConstTime arg1 Stack empty despite still trying to process operation.");
            value_stack.push_back(unit_builder.ins().const_time(arg1_value));
        }
        Opcode::Drv => {
            let arg1_value = value_stack
                .pop_back()
                .expect("Drv arg3 Stack empty despite still trying to process operation.");
            let arg2_value = value_stack
                .pop_back()
                .expect("Drv arg2 Stack empty despite still trying to process operation.");
            let arg3_value = value_stack
                .pop_back()
                .expect("Drv arg1 Stack empty despite still trying to process operation.");
            let _ = unit_builder.ins().drv(arg1_value, arg2_value, arg3_value);
        }
        _ => {
            panic!("Unhandled opcode => {:?}", opcode);
        }
    }
}

fn backfill_expr_fifo_opcode(
    inst_id: Inst,
    opcode: Opcode,
    value_stack: &mut ValueStack,
    time_value_stack: &mut TimeValueStack,
    unit_builder: &UnitBuilder,
) {
    if let Some(inst_ret_value) = unit_builder.get_inst_result(inst_id) {
        match opcode {
            Opcode::ConstTime => {
                if let Some(_time_value) = unit_builder.get_const_time(inst_ret_value) {
                    // time_value_stack.push_back(time_value.clone());
                    time_value_stack.pop_back();
                }
            }
            _ => {
                value_stack.pop_back();
                value_stack.pop_back();
            }
        }
        value_stack.push_back(inst_ret_value);
    }
}

fn process_expr_fifo(
    expr_fifo: ExprWithIDFIFO,
    value_stack: &mut ValueStack,
    _int_value_stack: &mut IntValueStack,
    time_value_stack: &mut TimeValueStack,
    unit_builder: &mut UnitBuilder,
) {
    let mut visited_llhd_inst_ids: HashSet<Inst> = Default::default();
    for (inst_id_option, expr) in expr_fifo {
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
            GenericExpr::Call(_, symbol, _children) => {
                if let Some(opcode) = opcode::get_symbol_opcode(&symbol) {
                    if let Some(inst_id) = inst_id_option {
                        if !visited_llhd_inst_ids.contains(&inst_id) {
                            process_expr_fifo_opcode(
                                opcode,
                                value_stack,
                                time_value_stack,
                                unit_builder,
                            );
                            visited_llhd_inst_ids.insert(inst_id);
                        } else {
                            backfill_expr_fifo_opcode(
                                inst_id,
                                opcode,
                                value_stack,
                                time_value_stack,
                                unit_builder,
                            );
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
    if let GenericExpr::Call(_, type_symbol, type_args) = expr {
        if *type_symbol == Symbol::new(LLHD_TYPE_VOID_FIELD) {
            type_expr_fifo.push_back(llhd::void_ty());
        } else if *type_symbol == Symbol::new(LLHD_TYPE_INT_FIELD) {
            if let GenericExpr::Lit(_, Literal::Int(iid)) = &type_args[0] {
                type_expr_fifo.push_back(llhd::int_ty(
                    usize::try_from(*iid).expect("Failure to convert egglog Int to usize."),
                ));
            };
        } else if *type_symbol == Symbol::new(LLHD_TYPE_SIGNAL_FIELD) {
            process_arg_expr(&type_args[0], type_expr_fifo);
            let signal_info_expr = type_expr_fifo
                .pop_back()
                .expect("Stack empty despite still trying to process operation.");
            type_expr_fifo.push_back(llhd::signal_ty(signal_info_expr));
        }
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
            if symbol == Symbol::new(LLHD_UNIT_FIELD)
                || symbol == Symbol::new(LLHD_UNIT_WITH_CFG_FIELD)
                || symbol == Symbol::new(LLHD_UNIT_DECL_FIELD)
            {
                if info_exprs.len() >= 5 {
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
    let mut expr_fifo: ExprWithIDFIFO = Default::default();

    if let GenericExpr::Call(_, symbol, ref unit_info_and_data_exprs) = unit_expr {
        if symbol == Symbol::new(LLHD_UNIT_FIELD) && unit_info_and_data_exprs.len() == 6 {
            process_expr(&unit_info_and_data_exprs[5], &mut expr_fifo);
        } else if symbol == Symbol::new(LLHD_UNIT_WITH_CFG_FIELD)
            && unit_info_and_data_exprs.len() == 7
        {
            process_expr(&unit_info_and_data_exprs[5], &mut expr_fifo);
        }
    }

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

fn terminator() -> Command {
    let ty_datatype = Symbol::new(LLHD_TYPE_DATATYPE);
    let block_datatype = Symbol::new(LLHD_BLOCK_DATATYPE);
    let llhd_dfg_datatype = Symbol::new(LLHD_DFG_DATATYPE);
    Command::Datatype {
        span: DUMMY_SPAN.clone(),
        name: Symbol::new(LLHD_TERM_DATATYPE),
        variants: vec![
            Variant {
                span: DUMMY_SPAN.clone(),
                name: Symbol::new(LLHD_TERM_FIELD_BR),
                types: vec![block_datatype.clone()],
                cost: None,
            },
            Variant {
                span: DUMMY_SPAN.clone(),
                name: Symbol::new(LLHD_TERM_FIELD_BRCOND),
                types: vec![
                    ty_datatype.clone(),
                    llhd_dfg_datatype.clone(),
                    block_datatype.clone(),
                    block_datatype.clone(),
                ],
                cost: None,
            },
            Variant {
                span: DUMMY_SPAN.clone(),
                name: Symbol::new(LLHD_TERM_FIELD_WAIT),
                types: vec![block_datatype.clone(), Symbol::new(LLHD_VEC_VALUE_DATATYPE)],
                cost: None,
            },
            Variant {
                span: DUMMY_SPAN.clone(),
                name: Symbol::new(LLHD_TERM_FIELD_WAITTIME),
                types: vec![block_datatype.clone(), Symbol::new(LLHD_VEC_VALUE_DATATYPE)],
                cost: None,
            },
            Variant {
                span: DUMMY_SPAN.clone(),
                name: Symbol::new(LLHD_TERM_FIELD_HALT),
                types: vec![],
                cost: None,
            },
            Variant {
                span: DUMMY_SPAN.clone(),
                name: Symbol::new(LLHD_TERM_FIELD_RET),
                types: vec![],
                cost: None,
            },
            Variant {
                span: DUMMY_SPAN.clone(),
                name: Symbol::new(LLHD_TERM_FIELD_RETVALUE),
                types: vec![ty_datatype, llhd_dfg_datatype],
                cost: None,
            },
        ],
    }
}

fn effect() -> Command {
    let i64_sort = I64Sort;
    let ty_datatype = Symbol::new(LLHD_TYPE_DATATYPE);
    let llhd_dfg_datatype = Symbol::new(LLHD_DFG_DATATYPE);
    let ext_unit_datatype = Symbol::new(LLHD_EXT_UNIT_DATATYPE);
    let vec_value_datatype = Symbol::new(LLHD_VEC_VALUE_DATATYPE);
    Command::Datatype {
        span: DUMMY_SPAN.clone(),
        name: Symbol::new(LLHD_EFFECT_DATATYPE),
        variants: vec![
            Variant {
                span: DUMMY_SPAN.clone(),
                name: Symbol::new(LLHD_EFFECT_FIELD_SIG),
                types: vec![ty_datatype.clone(), llhd_dfg_datatype.clone()],
                cost: None,
            },
            Variant {
                span: DUMMY_SPAN.clone(),
                name: Symbol::new(LLHD_EFFECT_FIELD_DRV),
                types: vec![
                    ty_datatype.clone(),
                    llhd_dfg_datatype.clone(),
                    llhd_dfg_datatype.clone(),
                    llhd_dfg_datatype.clone(),
                ],
                cost: None,
            },
            Variant {
                span: DUMMY_SPAN.clone(),
                name: Symbol::new(LLHD_EFFECT_FIELD_DRVCOND),
                types: vec![
                    ty_datatype.clone(),
                    llhd_dfg_datatype.clone(),
                    llhd_dfg_datatype.clone(),
                    llhd_dfg_datatype.clone(),
                    llhd_dfg_datatype.clone(),
                ],
                cost: None,
            },
            Variant {
                span: DUMMY_SPAN.clone(),
                name: Symbol::new(LLHD_EFFECT_FIELD_VAR),
                types: vec![ty_datatype.clone(), llhd_dfg_datatype.clone()],
                cost: None,
            },
            Variant {
                span: DUMMY_SPAN.clone(),
                name: Symbol::new(LLHD_EFFECT_FIELD_LD),
                types: vec![ty_datatype.clone(), llhd_dfg_datatype.clone()],
                cost: None,
            },
            Variant {
                span: DUMMY_SPAN.clone(),
                name: Symbol::new(LLHD_EFFECT_FIELD_ST),
                types: vec![
                    ty_datatype.clone(),
                    llhd_dfg_datatype.clone(),
                    llhd_dfg_datatype.clone(),
                ],
                cost: None,
            },
            Variant {
                span: DUMMY_SPAN.clone(),
                name: Symbol::new(LLHD_EFFECT_FIELD_CALL),
                types: vec![
                    ty_datatype.clone(),
                    ext_unit_datatype.clone(),
                    i64_sort.name(),
                    vec_value_datatype.clone(),
                ],
                cost: None,
            },
            Variant {
                span: DUMMY_SPAN.clone(),
                name: Symbol::new(LLHD_EFFECT_FIELD_INST),
                types: vec![
                    ty_datatype,
                    ext_unit_datatype,
                    i64_sort.name(),
                    vec_value_datatype,
                ],
                cost: None,
            },
        ],
    }
}

fn vec_effect() -> Command {
    let vec_sort_symbol = Symbol::new(LLHD_VEC_EFFECT_DATATYPE);
    let symbol_vec = Symbol::new(EGGLOG_VEC_SORT);
    let effect_datatype = Symbol::new(LLHD_EFFECT_DATATYPE);
    let effect_expr = Expr::Var(DUMMY_SPAN.clone(), effect_datatype);
    Command::Sort(
        DUMMY_SPAN.clone(),
        vec_sort_symbol,
        Some((symbol_vec, vec![effect_expr])),
    )
}

fn block_skeleton() -> Command {
    let block_datatype = Symbol::new(LLHD_BLOCK_DATATYPE);
    let vec_value_datatype = Symbol::new(LLHD_VEC_VALUE_DATATYPE);
    let vec_effect_datatype = Symbol::new(LLHD_VEC_EFFECT_DATATYPE);
    let terminator_datatype = Symbol::new(LLHD_TERM_DATATYPE);
    let block_skeleton_variant = Variant {
        span: DUMMY_SPAN.clone(),
        name: Symbol::new(LLHD_BLOCK_SKELETON_FIELD),
        types: vec![
            block_datatype,
            vec_value_datatype,
            vec_effect_datatype,
            terminator_datatype,
        ],
        cost: None,
    };
    let symbol = Symbol::new(LLHD_BLOCK_SKELETON_DATATYPE);
    Command::Datatype {
        span: DUMMY_SPAN.clone(),
        name: symbol,
        variants: vec![block_skeleton_variant],
    }
}

fn vec_block_skeleton() -> Command {
    let vec_sort_symbol = Symbol::new(LLHD_VEC_BLOCK_SKELETON_DATATYPE);
    let symbol_vec = Symbol::new(EGGLOG_VEC_SORT);
    let block_skeleton_datatype = Symbol::new(LLHD_BLOCK_SKELETON_DATATYPE);
    let block_skeleton_expr = Expr::Var(DUMMY_SPAN.clone(), block_skeleton_datatype);
    Command::Sort(
        DUMMY_SPAN.clone(),
        vec_sort_symbol,
        Some((symbol_vec, vec![block_skeleton_expr])),
    )
}

fn cfg_skeleton() -> Command {
    let block_skeleton_vec_datatype = Symbol::new(LLHD_VEC_BLOCK_SKELETON_DATATYPE);
    let cfg_variant = Variant {
        span: DUMMY_SPAN.clone(),
        name: Symbol::new(LLHD_CFG_SKELETON_FIELD),
        types: vec![block_skeleton_vec_datatype],
        cost: None,
    };
    let symbol = Symbol::new(LLHD_CFG_SKELETON_DATATYPE);
    Command::Datatype {
        span: DUMMY_SPAN.clone(),
        name: symbol,
        variants: vec![cfg_variant],
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
    let unit_with_cfg_variant = Variant {
        span: DUMMY_SPAN.clone(),
        name: Symbol::new(LLHD_UNIT_WITH_CFG_FIELD),
        types: vec![
            i64_sort.name(),
            LLHD_UNIT_KIND_DATATYPE.into(),
            string_sort.name(),
            LLHD_VEC_VALUE_DATATYPE.into(),
            LLHD_VEC_VALUE_DATATYPE.into(),
            LLHD_DFG_CTX_DATATYPE.into(),
            LLHD_CFG_SKELETON_DATATYPE.into(),
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
        variants: vec![unit_variant, unit_with_cfg_variant, unit_decl_variant],
    }
}

fn unit_basic_types() -> EgglogCommandList {
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

pub(in crate::llhd_egraph) fn unit_cfg_types() -> EgglogCommandList {
    vec![
        terminator(),
        effect(),
        vec_effect(),
        block_skeleton(),
        vec_block_skeleton(),
        cfg_skeleton(),
    ]
}

pub(in crate::llhd_egraph) fn unit_types() -> EgglogCommandList {
    unit_basic_types()
}

pub(in crate::llhd_egraph) fn dfg() -> EgglogCommandList {
    vec![unit_ctx(), unit_def()]
}

#[cfg(test)]
mod tests;
