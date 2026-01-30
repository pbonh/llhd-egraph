use crate::llhd::module::LLHDModule;
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
                    let mut insts = unit
                        .all_insts()
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
                        .collect_vec();
                    insts.sort_by_key(|(opcode, args)| {
                        let mut key = format!("{:?}", opcode);
                        for arg in args {
                            key.push('|');
                            key.push_str(&format!("{:?}", arg));
                        }
                        key
                    });
                    insts
                })
                .collect(),
        }
    }
}

impl From<LLHDModule> for LLHDModuleTester {
    fn from(llhd_module: LLHDModule) -> Self {
        let module = Module::from(llhd_module);
        Self::from(module)
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

        let opcodes = unit1_test_data.iter().map(|inst| inst.0).collect_vec();
        assert!(
            opcodes.iter().any(|op| matches!(op, Opcode::ConstTime)),
            "Expected a ConstTime inst."
        );
        assert_eq!(
            2,
            opcodes
                .iter()
                .filter(|op| matches!(op, Opcode::And))
                .count(),
            "Expected two And insts."
        );
        assert!(
            opcodes.iter().any(|op| matches!(op, Opcode::Or)),
            "Expected an Or inst."
        );
        assert!(
            opcodes.iter().any(|op| matches!(op, Opcode::Drv)),
            "Expected a Drv inst."
        );
        let or_inst = unit1_test_data
            .iter()
            .find(|inst| matches!(inst.0, Opcode::Or))
            .expect("Expected Or inst.");
        let or_inst_args = &or_inst.1;
        assert_eq!(2, or_inst_args.len(), "Expected Or inst to have two args.");
        assert!(
            or_inst_args.iter().all(|op| matches!(op, Opcode::And)),
            "Expected Or inst args to be And opcodes."
        );

        assert!(
            unit1_test_data
                .iter()
                .any(|inst| matches!(inst.0, Opcode::Halt)),
            "Expected a Halt inst."
        );
    }
}
