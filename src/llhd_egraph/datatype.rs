use egglog::ast::{Symbol, Variant, DUMMY_SPAN};
use itertools::Itertools;
use llhd::ir::Opcode;

use super::egglog_names::{LLHD_UNIT_FIELD, LLHD_VALUE_DATATYPE, LLHD_VALUE_REF_FIELD};
use super::inst::opcode::opcode_symbol;
use super::{inst, unit};
use egglog_program::*;

#[derive(Debug, Clone)]
pub struct LLHDEgglogSorts(EgglogCommandList);

impl LLHDEgglogSorts {
    pub fn llhd_dfg() -> Self {
        let mut unit_type_sorts = unit::unit_types();
        let mut inst_sorts = inst::dfg();
        let mut unit_sorts = unit::dfg();
        unit_type_sorts.append(&mut inst_sorts);
        unit_type_sorts.append(&mut unit_sorts);
        Self(unit_type_sorts)
    }
}

impl Default for LLHDEgglogSorts {
    fn default() -> Self {
        Self::llhd_dfg()
    }
}

impl From<LLHDEgglogSorts> for EgglogCommandList {
    fn from(llhd_sorts: LLHDEgglogSorts) -> Self {
        llhd_sorts.0
    }
}

impl From<LLHDEgglogSorts> for EgglogSorts {
    fn from(llhd_sorts: LLHDEgglogSorts) -> Self {
        Self::default().add_sorts(<LLHDEgglogSorts as Into<EgglogCommandList>>::into(
            llhd_sorts,
        ))
    }
}

pub(in crate::llhd_egraph) fn variant(opcode: Opcode, symbol_strs: Vec<&str>) -> Variant {
    Variant {
        span: DUMMY_SPAN.clone(),
        name: opcode_symbol(opcode),
        types: symbol_strs.into_iter().map(Symbol::new).collect_vec(),
        cost: None,
    }
}

pub(in crate::llhd_egraph) fn value_ref_variant() -> Variant {
    let value_sort = Symbol::new(LLHD_VALUE_DATATYPE);
    Variant {
        span: DUMMY_SPAN.clone(),
        name: Symbol::new(LLHD_VALUE_REF_FIELD),
        types: vec![value_sort],
        cost: None,
    }
}

pub(in crate::llhd_egraph) fn unit_root_variant_symbol() -> Symbol {
    Symbol::new(LLHD_UNIT_FIELD)
}

#[cfg(test)]
mod tests {
    use egglog::EGraph;

    use super::*;

    #[test]
    fn default_llhd_egglog_datatypes() {
        let llhd_dfg_sort = LLHDEgglogSorts::default();
        let mut egraph = EGraph::default();
        let egraph_msgs = egraph.run_program(llhd_dfg_sort.into());
        assert!(
            egraph_msgs.is_ok(),
            "Error loading LLHD DFG Datatype. Error: {:?}",
            egraph_msgs.err().unwrap()
        );
    }

    #[test]
    fn valid_dfg_llhd_egglog_datatypes() {
        let llhd_dfg_sort = LLHDEgglogSorts::llhd_dfg();
        let mut egraph = EGraph::default();
        let egraph_msgs = egraph.run_program(llhd_dfg_sort.into());
        assert!(
            egraph_msgs.is_ok(),
            "Error loading LLHD DFG Datatype. Error: {:?}",
            egraph_msgs.err().unwrap()
        );
    }
}
