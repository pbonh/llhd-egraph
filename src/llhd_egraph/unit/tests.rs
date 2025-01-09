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
