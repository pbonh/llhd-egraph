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
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum LLHDScope {
    Module,
    Unit(UnitId),
    ValueDef(UnitId, Value),
    Inst(UnitId, Value, Inst),
    ValueRef(UnitId, Value, Inst, Value),
}
