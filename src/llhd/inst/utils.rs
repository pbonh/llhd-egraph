use indexmap::IndexSet;
use llhd::ir::prelude::*;
use llhd::ir::InstData;

use crate::llhd::{LLHDInst, LLHDUtils, LLHDValueRef};

impl LLHDUtils {
    pub fn iterate_unit_insts<'unit>(unit: &'unit Unit) -> impl Iterator<Item = LLHDInst> + 'unit {
        unit.all_insts().filter_map(|inst| {
            let unit_id = unit.id();
            let inst_data = &unit[inst];
            if !matches!(inst_data, InstData::Nullary { .. }) {
                Some((unit_id, inst))
            } else {
                None
            }
        })
    }

    pub fn root_unit_inst(unit: &Unit) -> IndexSet<LLHDInst> {
        let unit_id = unit.id();
        let mut last_unit_insts: IndexSet<LLHDInst> = Default::default();
        let unit_outputs: IndexSet<Value> = unit.output_args().collect();
        Self::iterate_unit_insts(unit)
            .flat_map(|(_unit_id, inst_id)| {
                let inst_data = &unit[inst_id];
                inst_data
                    .args()
                    .iter()
                    .map(move |inst_arg| (inst_id, inst_arg))
            })
            .filter(|(_inst_id, inst_arg)| unit_outputs.contains(*inst_arg))
            .for_each(|(inst_id, _inst_arg)| {
                last_unit_insts.insert((unit_id, inst_id));
            });
        last_unit_insts
    }

    pub fn iterate_unit_value_defs<'unit>(
        unit: &'unit Unit,
    ) -> impl Iterator<Item = LLHDValueRef> + 'unit {
        unit.all_insts()
            .filter(|inst| unit.get_inst_result(*inst).is_some())
            .map(|inst| {
                let value_id = unit.inst_result(inst);
                (unit.id(), inst, value_id)
            })
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;
    use llhd::table::TableKey;

    use super::*;

    extern crate utilities;

    #[test]
    fn create_insts_and_value_defs() {
        let unit_data = utilities::build_entity_alpha(UnitName::anonymous(0));
        let unit = Unit::new(UnitId::new(0), &unit_data);
        let insts = LLHDUtils::iterate_unit_insts(&unit).collect_vec();
        let value_defs = LLHDUtils::iterate_unit_value_defs(&unit).collect_vec();
        assert_eq!(5, insts.len(), "There should be 5 Insts defined in Unit.");
        assert_eq!(
            5,
            value_defs.len(),
            "There should be 5 Values defined in Unit."
        );
        assert_eq!(
            Value::new(4),
            value_defs[0].2,
            "First Id should be Arg with Id: 4(4 args first)"
        );
        assert_eq!(
            Value::new(5),
            value_defs[1].2,
            "Second Id should be Arg with Id: 5(4 args first)"
        );
    }

    #[test]
    #[ignore]
    fn get_last_llhd_unit_inst_no_output() {
        let unit_data = utilities::build_entity_alpha(UnitName::anonymous(0));
        let unit = Unit::new(UnitId::new(0), &unit_data);
        let add2_inst = LLHDUtils::root_unit_inst(&unit).into_iter().next().unwrap();
        let add2_inst_data = &unit[add2_inst.1];
        assert_eq!(Opcode::Add, add2_inst_data.opcode(), "Inst should be Add.");
    }

    #[test]
    fn get_last_llhd_unit_inst() {
        let unit_data = utilities::build_entity_2and_1or_common(UnitName::anonymous(0));
        let unit = Unit::new(UnitId::new(0), &unit_data);
        let drv_inst = LLHDUtils::root_unit_inst(&unit).into_iter().next().unwrap();
        let drv_inst_data = &unit[drv_inst.1];
        assert_eq!(Opcode::Drv, drv_inst_data.opcode(), "Inst should be Drv.");
    }
}
