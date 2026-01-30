use egglog::ast::{
    GenericAction, GenericCommand, GenericExpr, GenericRunConfig, GenericSchedule, Symbol,
};
use egglog::{EGraph, TermDag};
use itertools::Itertools;
use llhd::ir::InstData;
use llhd::table::TableKey;

use super::*;
use crate::llhd::module::LLHDModule;
use crate::llhd::LLHDModuleTester;
use crate::llhd_egraph::datatype::LLHDEgglogSorts;
use crate::llhd_egraph::egglog_names::{LLHD_CFG_SKELETON_FIELD, LLHD_UNIT_WITH_CFG_FIELD};
use egglog_program::egraph::egglog_names::EGGLOG_VEC_OF_OP;

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
    if let GenericCommand::Action(GenericAction::Let(_dummy, let_stmt_symbol, _let_stmt)) =
        &egglog_facts.0[0]
    {
        assert_eq!(
            "unit_0",
            let_stmt_symbol.to_string(),
            "Let Stmt should match UnitName"
        );
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
    let egglog_expr_str = egglog_expr.to_string();
    assert!(
        egglog_expr_str.contains("LLHDUnitWithCFG"),
        "Expected LLHDUnitWithCFG wrapper."
    );
    assert!(
        egglog_expr_str.contains("CFGSkeleton"),
        "Expected CFGSkeleton in expression."
    );
    assert!(
        egglog_expr_str.contains("(Add 5 (IntTy 1)"),
        "Expected Add expression in DFG context."
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
    let egglog_expr_str = egglog_expr.to_string();
    assert!(
        egglog_expr_str.contains("LLHDUnitWithCFG"),
        "Expected LLHDUnitWithCFG wrapper."
    );
    assert!(
        egglog_expr_str.contains("CFGSkeleton"),
        "Expected CFGSkeleton in expression."
    );
    assert!(
        egglog_expr_str.contains("(Drv 5 (Void )"),
        "Expected Drv expression in DFG context."
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
    let egglog_expr_str = egglog_expr.to_string();
    assert!(
        egglog_expr_str.contains("LLHDUnitWithCFG"),
        "Expected LLHDUnitWithCFG wrapper."
    );
    assert!(
        egglog_expr_str.contains("CFGSkeleton"),
        "Expected CFGSkeleton in expression."
    );
    assert!(
        egglog_expr_str.contains("(Drv 11 (Void )"),
        "Expected first Drv expression in DFG context."
    );
    assert!(
        egglog_expr_str.contains("(Drv 6 (Void )"),
        "Expected second Drv expression in DFG context."
    );
}

#[test]
fn dfg_expression_tree_dual_output() {
    let module = utilities::load_llhd_module("dual_outputs_single_dfg.llhd");
    let units = LLHDUtils::iterate_unit_ids(&module).collect_vec();
    let unit = module.unit(*units.first().unwrap());
    let egglog_expr = from_unit(&unit);
    let egglog_expr_str = egglog_expr.to_string();
    assert!(
        egglog_expr_str.contains("LLHDUnitWithCFG"),
        "Expected LLHDUnitWithCFG wrapper."
    );
    assert!(
        egglog_expr_str.contains("CFGSkeleton"),
        "Expected CFGSkeleton in expression."
    );
    assert!(
        egglog_expr_str.contains("(Drv 4 (Void )"),
        "Expected first Drv expression in DFG context."
    );
    assert!(
        egglog_expr_str.contains("(Drv 3 (Void )"),
        "Expected second Drv expression in DFG context."
    );
}

#[test]
fn dfg_expression_tree_dual_output_no_redundant_insts_roundtrip() {
    let module = LLHDModule::from(utilities::load_llhd_module("dual_outputs_single_dfg.llhd"));
    let egglog_program: EgglogProgram = module.clone().into();
    let module_from_egglog: LLHDModule = egglog_program.into();

    let original_module = LLHDModuleTester::from(module);
    let round_trip_module = LLHDModuleTester::from(module_from_egglog);
    assert_eq!(
        original_module, round_trip_module,
        "Round-trip Module does not match original."
    );
}

#[test]
fn dfg_expression_tree_triple_output_no_redundant_insts_roundtrip() {
    let module = LLHDModule::from(utilities::load_llhd_module(
        "triple_outputs_single_dfg.llhd",
    ));
    let egglog_program: EgglogProgram = module.clone().into();
    let module_from_egglog: LLHDModule = egglog_program.into();

    let original_module = LLHDModuleTester::from(module);
    let round_trip_module = LLHDModuleTester::from(module_from_egglog);
    assert_eq!(
        original_module, round_trip_module,
        "Round-trip Module does not match original."
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

fn rewrite_module(module: &Module) -> UnitData {
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
        29,
        egraph.num_tuples(),
        "There should be 29 facts remaining in the egraph."
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
        31,
        egraph.num_tuples(),
        "There should be 31 facts remaining in the egraph(new 'And', new 'Or' nodes)."
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
    match egraph.extract(test_unit_symbol_value, &mut extracted_termdag, &unit_sort) {
        Ok((_unit_cost, unit_term)) => {
            let extracted_expr = extracted_termdag.term_to_expr(&unit_term, DUMMY_SPAN.clone());
            assert!(
                matches!(extracted_expr, GenericExpr::Call { .. }),
                "Top level expression should be a call."
            );
            let extracted_str = extracted_expr.to_string();
            assert!(
                extracted_str.contains("LLHDUnitWithCFG"),
                "Expected LLHDUnitWithCFG wrapper."
            );
            assert!(
                extracted_str.contains("CFGSkeleton"),
                "Expected CFGSkeleton in expression."
            );
            assert!(
                extracted_str.contains("(Drv 5 (Void )"),
                "Expected Drv expression in extracted unit."
            );
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
        }
        Err(msg) => panic!("Failure to extract Term DAG from EGraph: {:?}", msg),
    }
}

#[test]
fn llhd_rewrite_egglog_program() {
    let mut test_module = utilities::load_llhd_module("2and_1or_common.llhd");
    let test_unit_id = LLHDUtils::iterate_unit_ids(&test_module).collect_vec()[0];
    test_module[test_unit_id] = rewrite_module(&test_module);
    let new_unit_data = test_module.unit(test_unit_id);
    let new_unit_insts = new_unit_data.all_insts().collect_vec();
    assert_eq!(
        5,
        new_unit_insts.len(),
        "There should be 5 Insts in rewritten Unit."
    );
    let inst_opcodes = new_unit_insts
        .iter()
        .map(|inst| new_unit_data[*inst].opcode())
        .collect_vec();
    assert!(
        inst_opcodes
            .iter()
            .any(|op| matches!(op, Opcode::ConstTime)),
        "Expected a const time inst."
    );
    assert!(
        inst_opcodes.iter().any(|op| matches!(op, Opcode::Or)),
        "Expected an or inst."
    );
    assert!(
        inst_opcodes.iter().any(|op| matches!(op, Opcode::And)),
        "Expected an and inst."
    );
    assert!(
        inst_opcodes.iter().any(|op| matches!(op, Opcode::Drv)),
        "Expected a drv inst."
    );
    assert!(
        new_unit_insts
            .iter()
            .any(|inst| matches!(new_unit_data[*inst], InstData::Nullary { .. })),
        "Expected a nullary instruction."
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

#[test]
fn llhd_egglog_terminator_datatype() {
    let term_datatype = terminator();
    let expected_str = utilities::trim_expr_whitespace(indoc::indoc! {"
            (datatype LLHDTerminator
                (TermBr LLHDBlock)
                (TermBrCond LLHDTy LLHDDFG LLHDBlock LLHDBlock)
                (TermWait LLHDBlock LLHDVecValue)
                (TermWaitTime LLHDBlock LLHDVecValue)
                (TermHalt)
                (TermRet)
                (TermRetValue LLHDTy LLHDDFG))
        "});
    assert_eq!(expected_str, term_datatype.to_string());
}

#[test]
fn llhd_egglog_effect_datatype() {
    let effect_datatype = effect();
    let expected_str = utilities::trim_expr_whitespace(indoc::indoc! {"
            (datatype LLHDEffect
                (EffectSig LLHDTy LLHDDFG)
                (EffectDrv LLHDTy LLHDDFG LLHDDFG LLHDDFG)
                (EffectDrvCond LLHDTy LLHDDFG LLHDDFG LLHDDFG LLHDDFG)
                (EffectVar LLHDTy LLHDDFG)
                (EffectLd LLHDTy LLHDDFG)
                (EffectSt LLHDTy LLHDDFG LLHDDFG)
                (EffectCall LLHDTy LLHDExtUnit i64 LLHDUnitDFGContext)
                (EffectInst LLHDTy LLHDExtUnit i64 LLHDUnitDFGContext))
        "});
    assert_eq!(expected_str, effect_datatype.to_string());
}

#[test]
fn llhd_egglog_effect_vec_sort() {
    let vec_effect_sort = vec_effect();
    let expected_str = "(sort LLHDVecEffect (Vec LLHDEffect))".to_owned();
    assert_eq!(expected_str, vec_effect_sort.to_string());
}

#[test]
fn llhd_egglog_block_cfg_skeleton_datatypes() {
    let block_skeleton_datatype = block_skeleton();
    let expected_block_skeleton = utilities::trim_expr_whitespace(indoc::indoc! {"
            (datatype LLHDBlockSkeleton
                (BlockSkeleton LLHDBlock LLHDVecValue LLHDVecEffect LLHDTerminator))
        "});
    assert_eq!(expected_block_skeleton, block_skeleton_datatype.to_string());

    let vec_block_skeleton_sort = vec_block_skeleton();
    let expected_vec_block_skeleton =
        "(sort LLHDVecBlockSkeleton (Vec LLHDBlockSkeleton))".to_owned();
    assert_eq!(
        expected_vec_block_skeleton,
        vec_block_skeleton_sort.to_string()
    );

    let cfg_skeleton_datatype = cfg_skeleton();
    let expected_cfg_skeleton = utilities::trim_expr_whitespace(indoc::indoc! {"
            (datatype LLHDCFGSkeleton (CFGSkeleton LLHDVecBlockSkeleton))
        "});
    assert_eq!(expected_cfg_skeleton, cfg_skeleton_datatype.to_string());
}

#[test]
fn llhd_egglog_unit_with_cfg_datatype() {
    let unit_datatype = unit_def();
    let expected_str = utilities::trim_expr_whitespace(indoc::indoc! {"
            (datatype LLHDUnitDFG
                (LLHDUnit i64 LLHDUnitKind String LLHDVecValue LLHDVecValue LLHDUnitDFGContext)
                (LLHDUnitWithCFG i64 LLHDUnitKind String LLHDVecValue LLHDVecValue LLHDUnitDFGContext LLHDCFGSkeleton)
                (LLHDUnitDecl i64 LLHDUnitKind String LLHDVecValue LLHDVecValue))
        "});
    assert_eq!(expected_str, unit_datatype.to_string());
}

fn build_cfg_test_module() -> Module {
    utilities::build_llhd_module(indoc::indoc! {"
        entity @acc_tb () -> () {
          %zero = const i1 0
          %sig = sig i1 %zero
        }
        proc @acc_tb_initial (i1$ %sig) -> () {
          entry:
            %zero = const i1 0
            %delay = const time 1ns
            drv i1$ %sig, %zero, %delay
            call void @acc_tb_check (i1 %zero)
            wait %exit for %delay
          exit:
            halt
        }
        func @acc_tb_check (i1 %x) void {
          entry:
            ret
        }
    "})
}

#[test]
fn llhd_cfg_skeleton_facts_for_non_entity_units() {
    let module = build_cfg_test_module();
    let egglog_facts = LLHDEgglogFacts::from_module(&module);
    let mut saw_entity = false;
    let mut saw_proc = false;
    let mut saw_func = false;
    let mut saw_wait = false;
    let mut saw_drv = false;
    let mut saw_call = false;

    for cmd in egglog_facts.0.iter() {
        if let GenericCommand::Action(GenericAction::Let(_, unit_symbol, unit_expr)) = cmd {
            let unit_symbol_str = unit_symbol.to_string();
            if unit_symbol_str == "unit_acc_tb" {
                if let GenericExpr::Call(_, symbol, _) = unit_expr {
                    assert_eq!(LLHD_UNIT_WITH_CFG_FIELD, symbol.to_string());
                } else {
                    panic!("Entity unit should be an LLHDUnitWithCFG call expression.");
                }
                saw_entity = true;
                continue;
            }

            if unit_symbol_str == "unit_acc_tb_initial" || unit_symbol_str == "unit_acc_tb_check" {
                if let GenericExpr::Call(_, symbol, args) = unit_expr {
                    assert_eq!(LLHD_UNIT_WITH_CFG_FIELD, symbol.to_string());
                    let cfg_expr = args
                        .last()
                        .expect("LLHDUnitWithCFG should include a CFG skeleton.");
                    if let GenericExpr::Call(_, cfg_symbol, cfg_args) = cfg_expr {
                        assert_eq!(LLHD_CFG_SKELETON_FIELD, cfg_symbol.to_string());
                        if let Some(GenericExpr::Call(_, vec_symbol, block_exprs)) =
                            cfg_args.first()
                        {
                            assert_eq!(EGGLOG_VEC_OF_OP, vec_symbol.to_string());
                            assert!(
                                !block_exprs.is_empty(),
                                "CFG skeleton should include blocks."
                            );
                        } else {
                            panic!("CFG skeleton should include a vec-of block skeletons.");
                        }
                    } else {
                        panic!("CFG skeleton should be a CFGSkeleton call.");
                    }

                    let unit_expr_str = unit_expr.to_string();
                    if unit_expr_str.contains("TermWait") || unit_expr_str.contains("TermWaitTime")
                    {
                        saw_wait = true;
                    }
                    if unit_expr_str.contains("EffectDrv") {
                        saw_drv = true;
                    }
                    if unit_expr_str.contains("EffectCall") {
                        saw_call = true;
                    }
                } else {
                    panic!("Unit should be represented as a call expression.");
                }

                if unit_symbol_str == "unit_acc_tb_initial" {
                    saw_proc = true;
                } else {
                    saw_func = true;
                }
            }
        }
    }

    assert!(saw_entity, "Expected entity unit facts for acc_tb.");
    assert!(saw_proc, "Expected process unit facts for acc_tb_initial.");
    assert!(saw_func, "Expected function unit facts for acc_tb_check.");
    assert!(saw_wait, "Expected wait terminator in CFG skeleton.");
    assert!(saw_drv, "Expected drv effect in CFG skeleton.");
    assert!(saw_call, "Expected call effect in CFG skeleton.");
}

#[test]
fn llhd_egglog_program_accepts_cfg_units() {
    let module = LLHDModule::from(build_cfg_test_module());
    let egglog_program: EgglogProgram = module.into();
    let mut egraph = EGraph::default();
    let egraph_msgs = egraph.run_program(egglog_program.into());
    assert!(
        egraph_msgs.is_ok(),
        "EGraph failed to load CFG skeleton facts. Error: {:?}",
        egraph_msgs.err()
    );
}

#[test]
fn llhd_cfg_skeleton_facts_testbench_paper() {
    let module = utilities::load_llhd_module("testbench_paper.llhd");
    let egglog_facts = LLHDEgglogFacts::from_module(&module);
    let mut saw_br = false;
    let mut saw_brcond = false;
    let mut saw_wait = false;
    let mut saw_halt = false;
    let mut saw_var = false;
    let mut saw_ld = false;
    let mut saw_st = false;
    let mut saw_drv = false;
    let mut saw_call = false;
    let mut saw_ret = false;

    for cmd in egglog_facts.0.iter() {
        if let GenericCommand::Action(GenericAction::Let(_, unit_symbol, unit_expr)) = cmd {
            let unit_symbol_str = unit_symbol.to_string();
            if unit_symbol_str != "unit_acc_tb_initial" && unit_symbol_str != "unit_acc_tb_check" {
                continue;
            }
            let unit_expr_str = unit_expr.to_string();
            if unit_expr_str.contains("TermBr ") {
                saw_br = true;
            }
            if unit_expr_str.contains("TermBrCond") {
                saw_brcond = true;
            }
            if unit_expr_str.contains("TermWaitTime") || unit_expr_str.contains("TermWait") {
                saw_wait = true;
            }
            if unit_expr_str.contains("TermHalt") {
                saw_halt = true;
            }
            if unit_expr_str.contains("EffectVar") {
                saw_var = true;
            }
            if unit_expr_str.contains("EffectLd") {
                saw_ld = true;
            }
            if unit_expr_str.contains("EffectSt") {
                saw_st = true;
            }
            if unit_expr_str.contains("EffectDrv") {
                saw_drv = true;
            }
            if unit_expr_str.contains("EffectCall") {
                saw_call = true;
            }
            if unit_expr_str.contains("TermRet") {
                saw_ret = true;
            }
        }
    }

    assert!(saw_br, "Expected br terminator in CFG skeleton.");
    assert!(saw_brcond, "Expected brcond terminator in CFG skeleton.");
    assert!(saw_wait, "Expected wait terminator in CFG skeleton.");
    assert!(saw_halt, "Expected halt terminator in CFG skeleton.");
    assert!(saw_var, "Expected var effect in CFG skeleton.");
    assert!(saw_ld, "Expected ld effect in CFG skeleton.");
    assert!(saw_st, "Expected st effect in CFG skeleton.");
    assert!(saw_drv, "Expected drv effect in CFG skeleton.");
    assert!(saw_call, "Expected call effect in CFG skeleton.");
    assert!(saw_ret, "Expected ret terminator in CFG skeleton.");
}

#[test]
fn llhd_cfg_skeleton_roundtrip_testbench_paper() {
    let module = LLHDModule::from(utilities::load_llhd_module("testbench_paper.llhd"));
    let egglog_program: EgglogProgram = module.clone().into();
    let module_from_egglog: LLHDModule = egglog_program.into();

    let original_module = LLHDModuleTester::from(module);
    let round_trip_module = LLHDModuleTester::from(module_from_egglog);
    assert_eq!(
        original_module, round_trip_module,
        "Round-trip Module does not match original."
    );
}
