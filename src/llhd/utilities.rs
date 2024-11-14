use itertools::Itertools;
use llhd::ir::prelude::*;
use std::collections::HashSet;

pub type LLHDInstArgs = Vec<Value>;
pub type LLHDInstInfo = Vec<(Opcode, LLHDInstArgs)>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LLHDUnitTester {
    unit_insts: HashSet<LLHDInstInfo>,
}

impl From<Module> for LLHDUnitTester {
    fn from(module: Module) -> Self {
        Self {
            unit_insts: module
                .units()
                .map(|unit| {
                    unit.all_insts()
                        .map(|inst| (unit[inst].opcode(), unit[inst].args().to_vec()))
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
        let module_test_data = LLHDUnitTester::from(module);
        assert_eq!(
            module_test_data.unit_insts.len(),
            1,
            "There is 1 Unit present in 2and_1or_common."
        );
        let unit1_test_data = module_test_data.unit_insts.iter().next().unwrap();
        assert_eq!(unit1_test_data.len(), 6, "There should be 6 Insts in Unit.");
        assert!(
            matches!(unit1_test_data[0].0, Opcode::ConstTime),
            "Opcode for 1st Inst should be ConstTime."
        );
        assert!(
            matches!(unit1_test_data[1].0, Opcode::And),
            "Opcode for 1st Inst should be And."
        );
        assert!(
            matches!(unit1_test_data[2].0, Opcode::And),
            "Opcode for 1st Inst should be And."
        );
        assert!(
            matches!(unit1_test_data[3].0, Opcode::Or),
            "Opcode for 1st Inst should be Or."
        );
        assert!(
            matches!(unit1_test_data[4].0, Opcode::Drv),
            "Opcode for 1st Inst should be Drv."
        );
        assert!(
            matches!(unit1_test_data[5].0, Opcode::Halt),
            "Opcode for 1st Inst should be Halt."
        );
    }
}
