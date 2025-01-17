use serde::{Deserialize, Serialize};

#[derive(Debug)]
pub struct LLHDUtils;

/// LLHD Inst `ENode` Type
pub mod enode;

/// Helper Functions for LLHD Types
pub mod common;

/// Data for LLHD Nets/Instructions
pub mod inst;

/// LLHD Module Type Wrapper
pub mod module;
pub use module::*;

/// LLHD Unit Data
pub mod unit;

/// LLHD Utilities
pub mod utilities;
pub use utilities::*;

use llhd::ir::{Inst, UnitId, Value};

/// `Net/Value` Identifier within LLHD `Unit`
pub type LLHDDef = (UnitId, Value);

/// `Inst` Identifier within LLHD `Unit`
pub type LLHDInst = (UnitId, Inst);

/// `Value` Identifier within LLHD `Unit`
pub type LLHDValueRef = (UnitId, Inst, Value);

/// Generic LLHD Value Identifier
pub type LLHDIndex = (UnitId, Value, Option<Inst>, Option<Value>);

/// LLHD Scope
#[derive(
    Debug, Clone, Copy, Default, Hash, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize,
)]
pub enum LLHDScope {
    #[default]
    Module,
    Unit(UnitId),
    ValueDef(UnitId, Value),
    Inst(UnitId, Value, Inst),
    ValueRef(UnitId, Value, Inst, Value),
}

impl From<()> for LLHDScope {
    fn from(_no_scope: ()) -> Self {
        Self::Module
    }
}

impl From<UnitId> for LLHDScope {
    fn from(unit_id: UnitId) -> Self {
        Self::Unit(unit_id)
    }
}

impl From<(UnitId, Value)> for LLHDScope {
    fn from(value_def: (UnitId, Value)) -> Self {
        Self::ValueDef(value_def.0, value_def.1)
    }
}

impl From<(UnitId, Value, Inst)> for LLHDScope {
    fn from(inst_def: (UnitId, Value, Inst)) -> Self {
        Self::Inst(inst_def.0, inst_def.1, inst_def.2)
    }
}

impl From<(UnitId, Value, Inst, Value)> for LLHDScope {
    fn from(value_ref: (UnitId, Value, Inst, Value)) -> Self {
        Self::ValueRef(value_ref.0, value_ref.1, value_ref.2, value_ref.3)
    }
}
