use egglog::ast::{Symbol, Variant};
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
        let mut cfg_sorts = unit::unit_cfg_types();
        let mut unit_sorts = unit::dfg();
        unit_type_sorts.append(&mut inst_sorts);
        unit_type_sorts.append(&mut cfg_sorts);
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
    use egglog::ast::Command;
    use egglog::EGraph;

    use super::*;
    use crate::llhd_egraph::egglog_names::{
        LLHD_BLOCK_SKELETON_DATATYPE, LLHD_CFG_SKELETON_DATATYPE, LLHD_EFFECT_DATATYPE,
        LLHD_TERM_DATATYPE, LLHD_UNIT_DFG_DATATYPE, LLHD_UNIT_WITH_CFG_FIELD,
        LLHD_VEC_BLOCK_SKELETON_DATATYPE, LLHD_VEC_EFFECT_DATATYPE,
    };

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

    #[test]
    fn llhd_dfg_includes_cfg_skeleton_datatypes() {
        let llhd_dfg_sort = LLHDEgglogSorts::llhd_dfg();
        let cmd_list: EgglogCommandList = llhd_dfg_sort.into();

        let mut has_term = false;
        let mut has_effect = false;
        let mut has_vec_effect = false;
        let mut has_block_skel = false;
        let mut has_vec_block_skel = false;
        let mut has_cfg_skel = false;
        let mut has_unit_with_cfg_variant = false;

        for cmd in cmd_list.iter() {
            match cmd {
                Command::Datatype { name, variants, .. } => {
                    let name_str = name.to_string();
                    if name_str == LLHD_TERM_DATATYPE {
                        has_term = true;
                    } else if name_str == LLHD_EFFECT_DATATYPE {
                        has_effect = true;
                    } else if name_str == LLHD_BLOCK_SKELETON_DATATYPE {
                        has_block_skel = true;
                    } else if name_str == LLHD_CFG_SKELETON_DATATYPE {
                        has_cfg_skel = true;
                    } else if name_str == LLHD_UNIT_DFG_DATATYPE {
                        has_unit_with_cfg_variant = variants
                            .iter()
                            .any(|variant| variant.name.to_string() == LLHD_UNIT_WITH_CFG_FIELD);
                    }
                }
                Command::Sort(_span, name, _) => {
                    let name_str = name.to_string();
                    if name_str == LLHD_VEC_EFFECT_DATATYPE {
                        has_vec_effect = true;
                    } else if name_str == LLHD_VEC_BLOCK_SKELETON_DATATYPE {
                        has_vec_block_skel = true;
                    }
                }
                _ => {}
            }
        }

        assert!(has_term, "LLHDTerminator datatype should be present");
        assert!(has_effect, "LLHDEffect datatype should be present");
        assert!(has_vec_effect, "LLHDVecEffect sort should be present");
        assert!(
            has_block_skel,
            "LLHDBlockSkeleton datatype should be present"
        );
        assert!(
            has_vec_block_skel,
            "LLHDVecBlockSkeleton sort should be present"
        );
        assert!(has_cfg_skel, "LLHDCFGSkeleton datatype should be present");
        assert!(
            has_unit_with_cfg_variant,
            "LLHDUnitDFG should include LLHDUnitWithCFG variant"
        );
    }
}
