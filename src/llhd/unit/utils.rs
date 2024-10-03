use llhd::ir::prelude::*;

use crate::llhd::{LLHDDef, LLHDUtils};

impl LLHDUtils {
    pub(crate) fn iterate_unit_ids(module: &Module) -> impl Iterator<Item = UnitId> + '_ {
        module.units().map(|unit| unit.id())
    }

    pub(crate) fn iterate_unit_input_arg_defs<'unit>(
        unit: &'unit Unit,
    ) -> impl Iterator<Item = LLHDDef> + 'unit {
        unit.input_args().map(|arg| (unit.id(), arg))
    }

    pub(crate) fn iterate_unit_arg_defs<'unit>(
        unit: &'unit Unit,
    ) -> impl Iterator<Item = LLHDDef> + 'unit {
        Self::iterate_unit_input_arg_defs(unit)
            .map(|(_unit_id, arg)| arg)
            .chain(unit.output_args())
            .map(|arg| (unit.id(), arg))
    }
}

// TODO: Test fixture
