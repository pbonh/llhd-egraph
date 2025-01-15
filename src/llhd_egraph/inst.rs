use egglog::ast::{Command, Expr, GenericExpr, Literal, Symbol, Variant};
use lazy_static::lazy_static;
use llhd::ir::prelude::*;
use llhd::ir::{InstData, ValueData};
use llhd::table::TableKey;
use llhd::{IntValue, TimeValue, Type};

use crate::llhd_egraph::datatype::{value_ref_variant, variant};
use crate::llhd_egraph::egglog_names::*;
use egglog_program::egraph::egglog_names::{EGGLOG_I64_SORT, EGGLOG_STRING_SORT};
use egglog_program::*;

pub(in crate::llhd_egraph) mod opcode;
use opcode::opcode_symbol;

lazy_static! {
    static ref LLHD_DFG_VARIANTS: Vec<Variant> = vec![
        value_ref_variant(),
        variant(
            Opcode::ConstInt,
            vec![EGGLOG_I64_SORT, LLHD_TYPE_DATATYPE, EGGLOG_STRING_SORT]
        ),
        variant(
            Opcode::ConstTime,
            vec![EGGLOG_I64_SORT, LLHD_TYPE_DATATYPE, EGGLOG_STRING_SORT]
        ),
        variant(
            Opcode::Alias,
            vec![EGGLOG_I64_SORT, LLHD_TYPE_DATATYPE, LLHD_DFG_DATATYPE]
        ),
        variant(
            Opcode::ArrayUniform,
            vec![
                EGGLOG_I64_SORT,
                LLHD_TYPE_DATATYPE,
                EGGLOG_I64_SORT,
                LLHD_DFG_DATATYPE
            ]
        ),
        variant(
            Opcode::Array,
            vec![EGGLOG_I64_SORT, LLHD_VEC_VALUE_DATATYPE]
        ),
        variant(
            Opcode::Struct,
            vec![EGGLOG_I64_SORT, LLHD_VEC_VALUE_DATATYPE]
        ),
        variant(
            Opcode::Not,
            vec![EGGLOG_I64_SORT, LLHD_TYPE_DATATYPE, LLHD_DFG_DATATYPE]
        ),
        variant(
            Opcode::Neg,
            vec![EGGLOG_I64_SORT, LLHD_TYPE_DATATYPE, LLHD_DFG_DATATYPE]
        ),
        variant(
            Opcode::Add,
            vec![
                EGGLOG_I64_SORT,
                LLHD_TYPE_DATATYPE,
                LLHD_DFG_DATATYPE,
                LLHD_DFG_DATATYPE
            ]
        ),
        variant(
            Opcode::Sub,
            vec![
                EGGLOG_I64_SORT,
                LLHD_TYPE_DATATYPE,
                LLHD_DFG_DATATYPE,
                LLHD_DFG_DATATYPE
            ]
        ),
        variant(
            Opcode::And,
            vec![
                EGGLOG_I64_SORT,
                LLHD_TYPE_DATATYPE,
                LLHD_DFG_DATATYPE,
                LLHD_DFG_DATATYPE
            ]
        ),
        variant(
            Opcode::Or,
            vec![
                EGGLOG_I64_SORT,
                LLHD_TYPE_DATATYPE,
                LLHD_DFG_DATATYPE,
                LLHD_DFG_DATATYPE
            ]
        ),
        variant(
            Opcode::Xor,
            vec![
                EGGLOG_I64_SORT,
                LLHD_TYPE_DATATYPE,
                LLHD_DFG_DATATYPE,
                LLHD_DFG_DATATYPE
            ]
        ),
        variant(
            Opcode::Smul,
            vec![
                EGGLOG_I64_SORT,
                LLHD_TYPE_DATATYPE,
                LLHD_DFG_DATATYPE,
                LLHD_DFG_DATATYPE
            ]
        ),
        variant(
            Opcode::Sdiv,
            vec![
                EGGLOG_I64_SORT,
                LLHD_TYPE_DATATYPE,
                LLHD_DFG_DATATYPE,
                LLHD_DFG_DATATYPE
            ]
        ),
        variant(
            Opcode::Smod,
            vec![
                EGGLOG_I64_SORT,
                LLHD_TYPE_DATATYPE,
                LLHD_DFG_DATATYPE,
                LLHD_DFG_DATATYPE
            ]
        ),
        variant(
            Opcode::Srem,
            vec![
                EGGLOG_I64_SORT,
                LLHD_TYPE_DATATYPE,
                LLHD_DFG_DATATYPE,
                LLHD_DFG_DATATYPE
            ]
        ),
        variant(
            Opcode::Umul,
            vec![
                EGGLOG_I64_SORT,
                LLHD_TYPE_DATATYPE,
                LLHD_DFG_DATATYPE,
                LLHD_DFG_DATATYPE
            ]
        ),
        variant(
            Opcode::Udiv,
            vec![
                EGGLOG_I64_SORT,
                LLHD_TYPE_DATATYPE,
                LLHD_DFG_DATATYPE,
                LLHD_DFG_DATATYPE
            ]
        ),
        variant(
            Opcode::Umod,
            vec![
                EGGLOG_I64_SORT,
                LLHD_TYPE_DATATYPE,
                LLHD_DFG_DATATYPE,
                LLHD_DFG_DATATYPE
            ]
        ),
        variant(
            Opcode::Urem,
            vec![
                EGGLOG_I64_SORT,
                LLHD_TYPE_DATATYPE,
                LLHD_DFG_DATATYPE,
                LLHD_DFG_DATATYPE
            ]
        ),
        variant(
            Opcode::Eq,
            vec![
                EGGLOG_I64_SORT,
                LLHD_TYPE_DATATYPE,
                LLHD_DFG_DATATYPE,
                LLHD_DFG_DATATYPE
            ]
        ),
        variant(
            Opcode::Neq,
            vec![
                EGGLOG_I64_SORT,
                LLHD_TYPE_DATATYPE,
                LLHD_DFG_DATATYPE,
                LLHD_DFG_DATATYPE
            ]
        ),
        variant(
            Opcode::Slt,
            vec![
                EGGLOG_I64_SORT,
                LLHD_TYPE_DATATYPE,
                LLHD_DFG_DATATYPE,
                LLHD_DFG_DATATYPE
            ]
        ),
        variant(
            Opcode::Sgt,
            vec![
                EGGLOG_I64_SORT,
                LLHD_TYPE_DATATYPE,
                LLHD_DFG_DATATYPE,
                LLHD_DFG_DATATYPE
            ]
        ),
        variant(
            Opcode::Sle,
            vec![
                EGGLOG_I64_SORT,
                LLHD_TYPE_DATATYPE,
                LLHD_DFG_DATATYPE,
                LLHD_DFG_DATATYPE
            ]
        ),
        variant(
            Opcode::Sge,
            vec![
                EGGLOG_I64_SORT,
                LLHD_TYPE_DATATYPE,
                LLHD_DFG_DATATYPE,
                LLHD_DFG_DATATYPE
            ]
        ),
        variant(
            Opcode::Ult,
            vec![
                EGGLOG_I64_SORT,
                LLHD_TYPE_DATATYPE,
                LLHD_DFG_DATATYPE,
                LLHD_DFG_DATATYPE
            ]
        ),
        variant(
            Opcode::Ugt,
            vec![
                EGGLOG_I64_SORT,
                LLHD_TYPE_DATATYPE,
                LLHD_DFG_DATATYPE,
                LLHD_DFG_DATATYPE
            ]
        ),
        variant(
            Opcode::Ule,
            vec![
                EGGLOG_I64_SORT,
                LLHD_TYPE_DATATYPE,
                LLHD_DFG_DATATYPE,
                LLHD_DFG_DATATYPE
            ]
        ),
        variant(
            Opcode::Uge,
            vec![
                EGGLOG_I64_SORT,
                LLHD_TYPE_DATATYPE,
                LLHD_DFG_DATATYPE,
                LLHD_DFG_DATATYPE
            ]
        ),
        variant(
            Opcode::Shl,
            vec![
                EGGLOG_I64_SORT,
                LLHD_TYPE_DATATYPE,
                LLHD_DFG_DATATYPE,
                LLHD_DFG_DATATYPE,
                LLHD_DFG_DATATYPE
            ]
        ),
        variant(
            Opcode::Shr,
            vec![
                EGGLOG_I64_SORT,
                LLHD_TYPE_DATATYPE,
                LLHD_DFG_DATATYPE,
                LLHD_DFG_DATATYPE,
                LLHD_DFG_DATATYPE
            ]
        ),
        variant(
            Opcode::Mux,
            vec![
                EGGLOG_I64_SORT,
                LLHD_TYPE_DATATYPE,
                LLHD_DFG_DATATYPE,
                LLHD_DFG_DATATYPE
            ]
        ),
        variant(
            Opcode::Reg,
            vec![
                EGGLOG_I64_SORT,
                LLHD_TYPE_DATATYPE,
                LLHD_VEC_VALUE_DATATYPE,
                LLHD_VEC_REGMODE_DATATYPE
            ]
        ),
        variant(
            Opcode::InsField,
            vec![
                EGGLOG_I64_SORT,
                LLHD_TYPE_DATATYPE,
                LLHD_DFG_DATATYPE,
                LLHD_DFG_DATATYPE,
                EGGLOG_I64_SORT,
                EGGLOG_I64_SORT
            ]
        ),
        variant(
            Opcode::InsSlice,
            vec![
                EGGLOG_I64_SORT,
                LLHD_TYPE_DATATYPE,
                LLHD_DFG_DATATYPE,
                LLHD_DFG_DATATYPE,
                EGGLOG_I64_SORT,
                EGGLOG_I64_SORT
            ]
        ),
        variant(
            Opcode::ExtField,
            vec![
                EGGLOG_I64_SORT,
                LLHD_TYPE_DATATYPE,
                LLHD_DFG_DATATYPE,
                LLHD_DFG_DATATYPE,
                EGGLOG_I64_SORT,
                EGGLOG_I64_SORT
            ]
        ),
        variant(
            Opcode::ExtSlice,
            vec![
                EGGLOG_I64_SORT,
                LLHD_TYPE_DATATYPE,
                LLHD_DFG_DATATYPE,
                LLHD_DFG_DATATYPE,
                EGGLOG_I64_SORT,
                EGGLOG_I64_SORT
            ]
        ),
        variant(
            Opcode::Con,
            vec![
                EGGLOG_I64_SORT,
                LLHD_TYPE_DATATYPE,
                LLHD_DFG_DATATYPE,
                LLHD_DFG_DATATYPE
            ]
        ),
        variant(
            Opcode::Del,
            vec![
                EGGLOG_I64_SORT,
                LLHD_TYPE_DATATYPE,
                LLHD_DFG_DATATYPE,
                LLHD_DFG_DATATYPE,
                LLHD_DFG_DATATYPE
            ]
        ),
        variant(
            Opcode::Call,
            vec![
                EGGLOG_I64_SORT,
                LLHD_TYPE_DATATYPE,
                LLHD_EXT_UNIT_DATATYPE,
                EGGLOG_I64_SORT,
                LLHD_VEC_VALUE_DATATYPE
            ]
        ),
        variant(
            Opcode::Inst,
            vec![
                EGGLOG_I64_SORT,
                LLHD_TYPE_DATATYPE,
                LLHD_EXT_UNIT_DATATYPE,
                EGGLOG_I64_SORT,
                LLHD_VEC_VALUE_DATATYPE
            ]
        ),
        variant(
            Opcode::Sig,
            vec![EGGLOG_I64_SORT, LLHD_TYPE_DATATYPE, LLHD_DFG_DATATYPE]
        ),
        variant(
            Opcode::Prb,
            vec![EGGLOG_I64_SORT, LLHD_TYPE_DATATYPE, LLHD_DFG_DATATYPE]
        ),
        variant(
            Opcode::Drv,
            vec![
                EGGLOG_I64_SORT,
                LLHD_TYPE_DATATYPE,
                LLHD_DFG_DATATYPE,
                LLHD_DFG_DATATYPE,
                LLHD_DFG_DATATYPE
            ],
        ),
        variant(
            Opcode::DrvCond,
            vec![
                EGGLOG_I64_SORT,
                LLHD_TYPE_DATATYPE,
                LLHD_DFG_DATATYPE,
                LLHD_DFG_DATATYPE,
                LLHD_DFG_DATATYPE,
                LLHD_DFG_DATATYPE
            ],
        ),
        variant(
            Opcode::Var,
            vec![EGGLOG_I64_SORT, LLHD_TYPE_DATATYPE, LLHD_DFG_DATATYPE],
        ),
        variant(
            Opcode::Ld,
            vec![EGGLOG_I64_SORT, LLHD_TYPE_DATATYPE, LLHD_DFG_DATATYPE],
        ),
        variant(
            Opcode::St,
            vec![
                EGGLOG_I64_SORT,
                LLHD_TYPE_DATATYPE,
                LLHD_DFG_DATATYPE,
                LLHD_DFG_DATATYPE
            ],
        ),
        variant(Opcode::Halt, vec![EGGLOG_I64_SORT,],),
        variant(Opcode::Ret, vec![EGGLOG_I64_SORT,],),
        variant(
            Opcode::RetValue,
            vec![EGGLOG_I64_SORT, LLHD_TYPE_DATATYPE, LLHD_DFG_DATATYPE],
        ),
        variant(
            Opcode::Phi,
            vec![
                EGGLOG_I64_SORT,
                LLHD_VEC_VALUE_DATATYPE,
                LLHD_VEC_BLOCK_DATATYPE
            ]
        ),
        variant(Opcode::Br, vec![EGGLOG_I64_SORT, LLHD_BLOCK_DATATYPE]),
        variant(
            Opcode::BrCond,
            vec![
                EGGLOG_I64_SORT,
                LLHD_TYPE_DATATYPE,
                LLHD_DFG_DATATYPE,
                LLHD_BLOCK_DATATYPE,
                LLHD_BLOCK_DATATYPE
            ]
        ),
        variant(
            Opcode::Wait,
            vec![
                EGGLOG_I64_SORT,
                LLHD_BLOCK_DATATYPE,
                LLHD_VEC_VALUE_DATATYPE
            ]
        ),
        variant(
            Opcode::WaitTime,
            vec![
                EGGLOG_I64_SORT,
                LLHD_BLOCK_DATATYPE,
                LLHD_VEC_VALUE_DATATYPE
            ]
        ),
    ];
    static ref LLHD_DFG_VARIANTS_COUNT: usize = LLHD_DFG_VARIANTS.len();
}

pub(in crate::llhd_egraph) fn dfg_insts() -> Command {
    let dfg_symbol = Symbol::new(LLHD_DFG_DATATYPE);
    Command::Datatype {
        span: DUMMY_SPAN.clone(),
        name: dfg_symbol,
        variants: LLHD_DFG_VARIANTS.to_vec(),
    }
}

pub(in crate::llhd_egraph) fn dfg() -> EgglogCommandList {
    vec![dfg_insts()]
}

pub(in crate::llhd_egraph) fn cfg() -> EgglogCommandList {
    let _symbol = Symbol::new(LLHD_CFG_DATATYPE);
    todo!()
}

pub(in crate::llhd_egraph) fn ty_expr(llhd_ty: &Type) -> Expr {
    if llhd_ty.is_void() {
        GenericExpr::Call(
            DUMMY_SPAN.clone(),
            Symbol::new(LLHD_TYPE_VOID_FIELD),
            vec![],
        )
    } else if llhd_ty.is_time() {
        GenericExpr::Call(
            DUMMY_SPAN.clone(),
            Symbol::new(LLHD_TYPE_TIME_FIELD),
            vec![],
        )
    } else if llhd_ty.is_int() {
        let int_value = Literal::Int(
            i64::try_from(llhd_ty.unwrap_int())
                .expect("Out-of-bound value for usize -> i64 conversion."),
        );
        let literal_value = GenericExpr::Lit(DUMMY_SPAN.clone(), int_value);
        GenericExpr::Call(
            DUMMY_SPAN.clone(),
            Symbol::new(LLHD_TYPE_INT_FIELD),
            vec![literal_value],
        )
    } else if llhd_ty.is_enum() {
        panic!("Cant handle EnumType yet.");
    } else if llhd_ty.is_pointer() {
        let inner_expr = ty_expr(llhd_ty.unwrap_pointer());
        GenericExpr::Call(
            DUMMY_SPAN.clone(),
            Symbol::new(LLHD_TYPE_POINTER_FIELD),
            vec![inner_expr],
        )
    } else if llhd_ty.is_signal() {
        let inner_expr = ty_expr(llhd_ty.unwrap_signal());
        GenericExpr::Call(
            DUMMY_SPAN.clone(),
            Symbol::new(LLHD_TYPE_SIGNAL_FIELD),
            vec![inner_expr],
        )
    } else if llhd_ty.is_array() {
        panic!("Cant handle ArrayType yet.");
    } else if llhd_ty.is_struct() {
        panic!("Cant handle StructType yet.");
    } else if llhd_ty.is_func() {
        panic!("Cant handle FuncType yet.");
    } else if llhd_ty.is_enum() {
        panic!("Cant handle EnumType yet.");
    } else {
        panic!("Unknown type.");
    }
}

pub(crate) fn value_def_expr(value_ty: Type, table_key: impl TableKey) -> Expr {
    let value_ty_expr = ty_expr(&value_ty);

    let converted_i64_num =
        i64::try_from(table_key.index()).expect("Out-of-bound value for usize -> i64 conversion.");
    let converted_literal = Literal::Int(converted_i64_num);
    let literal_value = GenericExpr::Lit(DUMMY_SPAN.clone(), converted_literal);

    let llhd_value_datatype_symbol = Symbol::new(LLHD_VALUE_FIELD);
    GenericExpr::Call(
        DUMMY_SPAN.clone(),
        llhd_value_datatype_symbol,
        vec![value_ty_expr, literal_value],
    )
}

pub(crate) fn value_ref_expr(value_ty: Type, table_key: impl TableKey) -> Expr {
    let value_ty_expr = ty_expr(&value_ty);

    let converted_i64_num =
        i64::try_from(table_key.index()).expect("Out-of-bound value for usize -> i64 conversion.");
    let converted_literal = Literal::Int(converted_i64_num);
    let literal_value = GenericExpr::Lit(DUMMY_SPAN.clone(), converted_literal);

    let llhd_value_datatype_symbol = Symbol::new(LLHD_VALUE_FIELD);
    let value_stmt = GenericExpr::Call(
        DUMMY_SPAN.clone(),
        llhd_value_datatype_symbol,
        vec![value_ty_expr, literal_value],
    );

    let llhd_value_ref_datatype_symbol = Symbol::new(LLHD_VALUE_REF_FIELD);
    GenericExpr::Call(
        DUMMY_SPAN.clone(),
        llhd_value_ref_datatype_symbol,
        vec![value_stmt],
    )
}

fn value_data_expr(unit: &Unit<'_>, value_data: &ValueData) -> Expr {
    match value_data {
        ValueData::Inst { ty, inst } => inst_expr(unit, *inst, ty.clone(), &unit[*inst]),
        ValueData::Arg { ty, arg } => value_ref_expr(ty.clone(), arg.to_owned()),
        _ => panic!("Value type not supported."),
    }
}

pub(in crate::llhd_egraph) fn literal_llhd_value(literal: &Literal) -> Value {
    match literal {
        Literal::Int(value) => {
            Value::new(usize::try_from(*value).expect("Failure to convert from i64 to usize."))
        }
        _ => panic!("Non-Int Literal"),
    }
}

pub(in crate::llhd_egraph) fn literal_llhd_inst_id(literal: &Literal) -> Inst {
    match literal {
        Literal::Int(value) => {
            Inst::new(usize::try_from(*value).expect("Failure to convert from i64 to usize."))
        }
        _ => panic!("Non-Int Literal"),
    }
}

fn int_value_expr(int_value: IntValue) -> Expr {
    let converted_literal = Literal::String(int_value.to_string().into());
    GenericExpr::Lit(DUMMY_SPAN.clone(), converted_literal)
}

pub(super) fn expr_int_value(literal: &Literal) -> IntValue {
    match literal {
        Literal::Int(value) => IntValue::from_isize(
            64,
            isize::try_from(*value).expect("Failure to convert from i64 to isize."),
        ),
        _ => panic!("Non-Int Literal"),
    }
}

fn time_value_expr(time_value: TimeValue) -> Expr {
    let converted_literal = Literal::String(time_value.to_string().into());
    GenericExpr::Lit(DUMMY_SPAN.clone(), converted_literal)
}

pub(in crate::llhd_egraph) fn expr_time_value(_literal: &Literal) -> TimeValue {
    TimeValue::zero()
}

pub(in crate::llhd_egraph) fn inst_expr(
    unit: &Unit<'_>,
    inst_id: Inst,
    inst_ty: Type,
    inst_data: &InstData,
) -> Expr {
    let inst_symbol = opcode_symbol(inst_data.opcode());
    let inst_id_literal = GenericExpr::Lit(
        DUMMY_SPAN.clone(),
        Literal::Int(
            i64::try_from(inst_id.index())
                .expect("Out-of-bound value for usize -> i64 conversion."),
        ),
    );
    let inst_ty_expr = ty_expr(&inst_ty);
    let mut children: Vec<Expr> = vec![inst_id_literal, inst_ty_expr];
    match inst_data {
        InstData::Binary { args, .. } => {
            let expr_left = value_data_expr(unit, &unit[args[0]]);
            let expr_right = value_data_expr(unit, &unit[args[1]]);
            children.append(&mut vec![expr_left, expr_right]);
        }
        InstData::Unary { args, .. } => {
            let value_data = &unit[args[0]];
            let expr_left = value_data_expr(unit, value_data);
            children.append(&mut vec![expr_left]);
        }
        InstData::ConstInt { imm, .. } => {
            children.append(&mut vec![int_value_expr(imm.clone())]);
        }
        InstData::ConstTime { imm, .. } => {
            children.append(&mut vec![time_value_expr(imm.clone())]);
        }
        InstData::Ternary { args, .. } => {
            let expr_x = value_data_expr(unit, &unit[args[0]]);
            let expr_y = value_data_expr(unit, &unit[args[1]]);
            let expr_z = value_data_expr(unit, &unit[args[2]]);
            children.append(&mut vec![expr_x, expr_y, expr_z]);
        }
        InstData::Nullary { .. } => {}
        _ => {
            panic!("No implementation for this InstData type.")
        }
    }
    GenericExpr::Call(DUMMY_SPAN.clone(), inst_symbol, children)
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;
    use llhd::table::TableKey;

    use super::*;
    use crate::llhd::LLHDUtils;

    extern crate utilities;

    #[test]
    fn llhd_egglog_dfg_datatypes() {
        let dfg_datatype = dfg();
        let expected_str = utilities::trim_expr_whitespace(indoc::indoc! {"
            (datatype LLHDDFG
                (ValueRef LLHDValue)
                (ConstInt i64 LLHDTy String)
                (ConstTime i64 LLHDTy String)
                (Alias i64 LLHDTy LLHDDFG)
                (ArrayUniform i64 LLHDTy i64 LLHDDFG)
                (Array i64 LLHDVecValue)
                (Struct i64 LLHDVecValue)
                (Not i64 LLHDTy LLHDDFG)
                (Neg i64 LLHDTy LLHDDFG)
                (Add i64 LLHDTy LLHDDFG LLHDDFG)
                (Sub i64 LLHDTy LLHDDFG LLHDDFG)
                (And i64 LLHDTy LLHDDFG LLHDDFG)
                (Or i64 LLHDTy LLHDDFG LLHDDFG)
                (Xor i64 LLHDTy LLHDDFG LLHDDFG)
                (Smul i64 LLHDTy LLHDDFG LLHDDFG)
                (Sdiv i64 LLHDTy LLHDDFG LLHDDFG)
                (Smod i64 LLHDTy LLHDDFG LLHDDFG)
                (Srem i64 LLHDTy LLHDDFG LLHDDFG)
                (Umul i64 LLHDTy LLHDDFG LLHDDFG)
                (Udiv i64 LLHDTy LLHDDFG LLHDDFG)
                (Umod i64 LLHDTy LLHDDFG LLHDDFG)
                (Urem i64 LLHDTy LLHDDFG LLHDDFG)
                (Eq i64 LLHDTy LLHDDFG LLHDDFG)
                (Neq i64 LLHDTy LLHDDFG LLHDDFG)
                (Slt i64 LLHDTy LLHDDFG LLHDDFG)
                (Sgt i64 LLHDTy LLHDDFG LLHDDFG)
                (Sle i64 LLHDTy LLHDDFG LLHDDFG)
                (Sge i64 LLHDTy LLHDDFG LLHDDFG)
                (Ult i64 LLHDTy LLHDDFG LLHDDFG)
                (Ugt i64 LLHDTy LLHDDFG LLHDDFG)
                (Ule i64 LLHDTy LLHDDFG LLHDDFG)
                (Uge i64 LLHDTy LLHDDFG LLHDDFG)
                (Shl i64 LLHDTy LLHDDFG LLHDDFG LLHDDFG)
                (Shr i64 LLHDTy LLHDDFG LLHDDFG LLHDDFG)
                (Mux i64 LLHDTy LLHDDFG LLHDDFG)
                (Reg i64 LLHDTy LLHDVecValue LLHDVecRegMode)
                (InsField i64 LLHDTy LLHDDFG LLHDDFG i64 i64)
                (InsSlice i64 LLHDTy LLHDDFG LLHDDFG i64 i64)
                (ExtField i64 LLHDTy LLHDDFG LLHDDFG i64 i64)
                (ExtSlice i64 LLHDTy LLHDDFG LLHDDFG i64 i64)
                (Con i64 LLHDTy LLHDDFG LLHDDFG)
                (Del i64 LLHDTy LLHDDFG LLHDDFG LLHDDFG)
                (Call i64 LLHDTy LLHDExtUnit i64 LLHDVecValue)
                (Inst i64 LLHDTy LLHDExtUnit i64 LLHDVecValue)
                (Sig i64 LLHDTy LLHDDFG)
                (Prb i64 LLHDTy LLHDDFG)
                (Drv i64 LLHDTy LLHDDFG LLHDDFG LLHDDFG)
                (DrvCond i64 LLHDTy LLHDDFG LLHDDFG LLHDDFG LLHDDFG)
                (Var i64 LLHDTy LLHDDFG)
                (Ld i64 LLHDTy LLHDDFG)
                (St i64 LLHDTy LLHDDFG LLHDDFG)
                (Halt i64)
                (Ret i64)
                (RetValue i64 LLHDTy LLHDDFG)
                (Phi i64 LLHDVecValue LLHDVecBlock)
                (Br i64 LLHDBlock)
                (BrCond i64 LLHDTy LLHDDFG LLHDBlock LLHDBlock)
                (Wait i64 LLHDBlock LLHDVecValue)
                (WaitTime i64 LLHDBlock LLHDVecValue)
            )
        "});
        assert_eq!(
            expected_str,
            dfg_datatype.into_iter().join(""),
            "LLHD DFG Egglog datatype does not match expected string."
        );
    }

    #[test]
    fn llhd_value_ref_egglog_expr() {
        let unit_data = utilities::build_entity_alpha(UnitName::anonymous(0));
        let unit = Unit::new(UnitId::new(0), &unit_data);
        let value1 = Value::new(1);
        let unit_sig = unit.sig();
        let value1_ty = unit_sig.arg_type(Arg::new(1));
        let value1_expr = value_ref_expr(value1_ty, value1);
        let expected_str = "(ValueRef (Value (Signal (IntTy 1)) 1))";
        assert_eq!(
            expected_str,
            value1_expr.to_string(),
            "Expr should match LLHDValue Constructor, (Value _)."
        );
    }

    #[test]
    fn llhd_value_def_egglog_expr() {
        let unit_data = utilities::build_entity_alpha(UnitName::anonymous(0));
        let unit = Unit::new(UnitId::new(0), &unit_data);
        let value1 = Value::new(1);
        let unit_sig = unit.sig();
        let value1_ty = unit_sig.arg_type(Arg::new(1));
        let value1_expr = value_def_expr(value1_ty, value1);
        let expected_str = "(Value (Signal (IntTy 1)) 1)";
        assert_eq!(
            expected_str,
            value1_expr.to_string(),
            "Expr should match LLHDValue Constructor, (Value _)."
        );
    }

    #[test]
    fn llhd_inst_egglog_expr() {
        let unit_data = utilities::build_entity_alpha(UnitName::anonymous(0));
        let unit = Unit::new(UnitId::new(0), &unit_data);
        let insts = LLHDUtils::iterate_unit_insts(&unit).collect_vec();
        let add2_inst = insts[4];
        let add2_inst_data = &unit[add2_inst.1];
        assert_eq!(Opcode::Add, add2_inst_data.opcode(), "Inst should be Add.");
        if let Some(add2_value) = unit.get_inst_result(add2_inst.1) {
            let add2_value_data = &unit[add2_value];
            if let ValueData::Inst { ty, .. } = add2_value_data {
                let add2_expr = inst_expr(&unit, add2_inst.1, ty.clone(), add2_inst_data);
                let expected_str = utilities::trim_expr_whitespace(indoc::indoc! {"
                    (Add 5 (IntTy 1)
                        (Add 3 (IntTy 1)
                            (ConstInt 1 (IntTy 1) \"i1 0\")
                            (ConstInt 2 (IntTy 1) \"i1 1\"))
                        (Prb 4 (IntTy 1) (ValueRef (Value (Signal (IntTy 1)) 2))))
                "});
                assert_eq!(
                    expected_str,
                    add2_expr.to_string(),
                    "Expr should match nested Add expr."
                );
            } else {
                panic!("add2 inst value data should be of type ValueData::Inst.");
            }
        } else {
            panic!("add2 inst value is not available.");
        }
    }
}
