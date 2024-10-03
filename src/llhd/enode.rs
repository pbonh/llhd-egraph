use std::fmt;
use std::sync::Arc;

use llhd::ir::{InstData, Unit, Value, ValueData};
use llhd::table::TableKey;
use llhd::ty::{Type, TypeKind};

/// `Net` `ENode` Data within LLHD `Unit`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LLHDENode {
    pub(crate) id: Value,
    pub(crate) ty: Type,
    pub(crate) data: InstData,
}

impl fmt::Display for LLHDENode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "v{} {}", self.id, self.ty)
    }
}

impl Default for LLHDENode {
    fn default() -> Self {
        Self {
            id: Value::new(usize::MAX),
            ty: Arc::new(TypeKind::VoidType),
            data: InstData::default(),
        }
    }
}

impl LLHDENode {
    /// Create LLHD `ENode` Op from `Value`
    pub(crate) fn new(unit: &Unit, value: Value) -> Self {
        let (ty, data) = match &unit[value] {
            ValueData::Arg { ty, .. } => (ty.clone(), InstData::default()),
            ValueData::Inst { ty, inst } => (ty.clone(), unit[*inst].clone()),
            _ => (TypeKind::VoidType.into(), InstData::default()),
        };
        Self {
            id: value,
            ty,
            data,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn llhd_enode_construction_default() {
        let _: LLHDENode = Default::default();
    }

    #[test]
    fn llhd_enode_construction_new() {
        let input = indoc::indoc! {"
            entity @test_entity (i1 %in1, i1 %in2) -> () {
                %or1 = or i1 %in1, %in2
            }
        "};
        let module = llhd::assembly::parse_module(input).unwrap();
        let unit = module.units().next().unwrap();
        let args: Vec<LLHDENode> = unit.args().map(|arg| LLHDENode::new(&unit, arg)).collect();
        assert!(!args.is_empty(), "Args should not be empty");
        assert_eq!(args.len(), 2, "There should be 2 Args present");
        let insts: Vec<LLHDENode> = unit
            .all_insts()
            .filter(|inst| !matches!(unit[*inst], InstData::Nullary { .. }))
            .map(|inst| LLHDENode::new(&unit, unit.inst_result(inst)))
            .collect();
        assert_eq!(insts.len(), 1, "There should be 1 Inst present");
    }
}
