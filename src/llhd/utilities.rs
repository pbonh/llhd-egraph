use itertools::Itertools;
use llhd::ir::prelude::*;
use std::collections::HashSet;

pub type LLHDArgs = Vec<Value>;
pub type LLHDInstArgs = Vec<Opcode>;
pub type LLHDInstInfo = Vec<(Opcode, LLHDInstArgs)>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LLHDModuleTester {
    unit_args: HashSet<LLHDArgs>,
    unit_insts: HashSet<LLHDInstInfo>,
}

impl From<Module> for LLHDModuleTester {
    fn from(module: Module) -> Self {
        Self {
            unit_args: module
                .units()
                .map(|unit| unit.args().collect_vec())
                .collect(),
            unit_insts: module
                .units()
                .map(|unit| {
                    unit.all_insts()
                        .map(|inst| {
                            (
                                unit[inst].opcode(),
                                unit[inst]
                                    .args()
                                    .iter()
                                    .filter(|inst_arg| unit.get_value_inst(**inst_arg).is_some())
                                    .map(|inst_arg| unit[unit.value_inst(*inst_arg)].opcode())
                                    .collect_vec(),
                            )
                        })
                        .collect_vec()
                })
                .collect(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn from_example_module() {
        let module = utilities::load_llhd_module("2and_1or_common.llhd");
        let module_test_data = LLHDModuleTester::from(module);
        assert_eq!(
            module_test_data.unit_args.len(),
            1,
            "There is 1 Unit present in 2and_1or_common."
        );
        assert_eq!(
            module_test_data.unit_insts.len(),
            1,
            "There is 1 Unit present in 2and_1or_common."
        );
        let unit1_arg_data = module_test_data.unit_args.iter().next().unwrap();
        assert_eq!(unit1_arg_data.len(), 4, "There should be 4 Args in Unit.");
        let unit1_test_data = module_test_data.unit_insts.iter().next().unwrap();
        assert_eq!(unit1_test_data.len(), 6, "There should be 6 Insts in Unit.");

        let const_time_inst = &unit1_test_data[0];
        assert!(
            matches!(const_time_inst.0, Opcode::ConstTime),
            "Opcode for 1st Inst should be ConstTime."
        );

        let and1_inst = &unit1_test_data[1];
        let and1_inst_args = and1_inst.1.clone();
        assert!(
            matches!(and1_inst.0, Opcode::And),
            "Opcode for 2nd Inst should be And."
        );
        assert!(
            and1_inst_args.is_empty(),
            "And Inst Args are Unit Args, which have no type."
        );

        let and2_inst = &unit1_test_data[2];
        let and2_inst_args = and2_inst.1.clone();
        assert!(
            matches!(and2_inst.0, Opcode::And),
            "Opcode for 3rd Inst should be And."
        );
        assert!(
            and2_inst_args.is_empty(),
            "And Inst Args are Unit Args, which have no type."
        );

        let or1_inst = &unit1_test_data[3];
        let or1_inst_args = or1_inst.1.clone();
        assert!(
            matches!(or1_inst.0, Opcode::Or),
            "Opcode for 4th Inst should be Or."
        );
        assert!(
            matches!(or1_inst_args[0], Opcode::And),
            "Opcode for 1st Or Inst Arg should be And."
        );
        assert!(
            matches!(or1_inst_args[1], Opcode::And),
            "Opcode for 2nd Or Inst Arg should be And."
        );

        let drv_inst = &unit1_test_data[4];
        assert!(
            matches!(drv_inst.0, Opcode::Drv),
            "Opcode for 1st Inst should be Drv."
        );

        assert!(
            matches!(unit1_test_data[5].0, Opcode::Halt),
            "Opcode for 1st Inst should be Halt."
        );
    }
}
