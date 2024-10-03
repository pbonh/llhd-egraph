use llhd::ir::{Inst, InstData, Module, Signature, Unit, UnitBuilder, UnitData, UnitName, Value};

use super::enode::LLHDENode;

pub fn filter_nullary(unit: &Unit, inst_id: Inst) -> bool {
    let inst_data = &unit[inst_id];
    !matches!(inst_data, InstData::Nullary { .. })
}

pub fn filter_instantiations(unit: &Unit, inst_id: Inst) -> bool {
    let inst_data = &unit[inst_id];
    matches!(inst_data, InstData::Call { .. })
}

pub fn get_unit_name(scope_unit: &Unit) -> String {
    // scope_unit.name().to_string()
    scope_unit
        .name()
        .get_name()
        .expect("UnitName does not resolve to a String.")
        .to_owned()
}

pub fn get_inst_name(module: &Module, scope_unit: &Unit, inst_id: Inst) -> String {
    let scope_unit_id = scope_unit.id();
    if let InstData::Call { unit, .. } = scope_unit[inst_id] {
        match module
            .lookup_ext_unit(unit, scope_unit_id)
            .expect("ExtUnit does not exist in Module.")
        {
            llhd::ir::LinkedUnit::Def(ext_unit_id) => {
                let mut unit_name = get_unit_name(&module.unit(scope_unit_id));
                unit_name.push('.');
                unit_name.push_str(&get_unit_name(&module.unit(ext_unit_id)));
                unit_name.push('.');
                unit_name.push_str(&inst_id.to_string());
                unit_name
            }
            llhd::ir::LinkedUnit::Decl(decl_id) => {
                module[decl_id].name.to_string() + &inst_id.to_string()
            }
        }
    } else {
        let mut inst_name = get_unit_name(&module.unit(scope_unit_id));
        inst_name.push('.');
        inst_name.push_str(&scope_unit[inst_id].opcode().to_string());
        inst_name.push('.');
        inst_name.push_str(&inst_id.to_string());
        inst_name
    }
}

pub fn get_value_name(module: &Module, scope_unit: &Unit, value: Value) -> String {
    let scope_unit_id = scope_unit.id();
    let mut value_name = get_unit_name(&module.unit(scope_unit_id));
    value_name.push('.');
    value_name.push_str(&value.to_string());
    value_name
}

pub fn build_unit(nets: &[LLHDENode], name: &UnitName, sig: &Signature) -> UnitData {
    let mut unit = UnitData::new(llhd::ir::UnitKind::Entity, name.clone(), sig.clone());
    {
        let mut builder = UnitBuilder::new_anonymous(&mut unit);
        nets.iter()
            .filter(|enode| InstData::default() != enode.data)
            .for_each(|inst_enode| {
                builder.build_inst(inst_enode.data.clone(), inst_enode.ty.clone());
            });
        Unit::new_anonymous(&unit).verify();
    }
    unit
}

pub fn build_enodes<'u>(unit: &'u Unit) -> impl Iterator<Item = LLHDENode> + 'u {
    unit.input_args()
        .map(|arg| LLHDENode {
            id: arg,
            ty: unit.value_type(arg),
            data: InstData::default(),
        })
        .chain(
            unit.all_insts()
                .filter(|inst| filter_nullary(unit, *inst))
                .map(|inst| LLHDENode {
                    id: unit.inst_result(inst),
                    ty: unit.inst_type(inst),
                    data: unit[inst].clone(),
                }),
        )
}

pub fn build_unit_name(name: &str) -> UnitName {
    UnitName::Global(name.to_owned())
}

#[cfg(test)]
mod tests {
    use llhd::ir::{ExtUnit, Opcode, UnitId, Value};
    use llhd::table::TableKey;

    use super::*;

    #[test]
    fn filter_nullary_from_unit() {
        let input = indoc::indoc! {"
            entity @ent2 (i1 %in1, i1 %in2, i1 %in3) -> () {
                %and1 = and i1 %in1, %in2
                %or1 = or i1 %and1, %in3
            }
        "};
        let module = llhd::assembly::parse_module(input).unwrap();
        let unit = module.units().next().unwrap();
        let unit_insts = unit.all_insts().filter(|inst| filter_nullary(&unit, *inst));
        assert_eq!(
            2,
            unit_insts.count(),
            "There should be 2 Instructions present in Unit."
        );
    }

    #[test]
    fn build_unit_name_from_string() {
        let unit_name = build_unit_name("top");
        assert!(unit_name.is_global(), "Unit should have global type.");
        assert_eq!(
            "top",
            unit_name.get_name().unwrap(),
            "Unit name should match."
        );
    }

    #[test]
    fn build_unit_from_slice() {
        let input = indoc::indoc! {"
            entity @ent2 (i1 %in1, i1 %in2, i1 %in3) -> () {
            }
        "};
        let mut module = llhd::assembly::parse_module(input).unwrap();
        let unit_id = module.units().next().unwrap().id();
        let unit_name = module.units().next().unwrap().name().clone();
        let unit_sig = module.units().next().unwrap().sig().clone();
        let args: Vec<Value> = module.unit(unit_id).input_args().collect();
        let mut unit = module.unit_mut(unit_id);

        let arg1 = LLHDENode {
            id: args[0],
            ty: unit.value_type(args[0]),
            data: InstData::default(),
        };
        let arg2 = LLHDENode {
            id: args[1],
            ty: unit.value_type(args[1]),
            data: InstData::default(),
        };
        let arg3 = LLHDENode {
            id: args[2],
            ty: unit.value_type(args[2]),
            data: InstData::default(),
        };

        // Make up the Id's/Types
        let and1_id = Value::new(3);
        let or1_id = Value::new(4);
        let and1 = LLHDENode {
            id: and1_id,
            ty: unit.value_type(args[0]),
            data: InstData::Binary {
                opcode: Opcode::And,
                args: [args[0], args[1]],
            },
        };
        let or1 = LLHDENode {
            id: or1_id,
            ty: unit.value_type(args[0]),
            data: InstData::Binary {
                opcode: Opcode::Or,
                args: [and1_id, args[2]],
            },
        };

        let enodes = vec![arg1, arg2, arg3, and1, or1];
        let updated_unit_data = build_unit(&enodes, &unit_name, &unit_sig);
        *unit.data() = updated_unit_data;

        let final_args: Vec<Value> = unit.input_args().collect();
        assert_eq!(
            3,
            final_args.len(),
            "3 Args should be present in this Unit."
        );
        let final_insts: Vec<Inst> = unit
            .all_insts()
            .filter(|inst| filter_nullary(&unit, *inst))
            .collect();
        assert_eq!(
            2,
            final_insts.len(),
            "2 Inst's should be present in this Unit."
        );
        assert_eq!(
            Opcode::And,
            unit[final_insts[0]].opcode(),
            "Inst Opcode should be And."
        );
        assert_eq!(
            Opcode::Or,
            unit[final_insts[1]].opcode(),
            "Inst Opcode should be Or."
        );
    }

    #[test]
    fn construct_enode_from_unit() {
        let input = indoc::indoc! {"
            entity @ent2 (i1 %in1, i1 %in2, i1 %in3) -> () {
                %and1 = and i1 %in1, %in2
                %or1 = or i1 %and1, %in3
            }
        "};
        let module = llhd::assembly::parse_module(input).unwrap();
        let unit = module.units().next().unwrap();
        let enodes: Vec<_> = build_enodes(&unit).collect();
        assert_eq!(5, enodes.len(), "There should be 5 ENodes built from Unit.");
    }

    #[test]
    fn llhd_get_name_for_inst() {
        let input = indoc::indoc! {"
            entity @ent2 (i1 %in1, i1 %in2, i1 %in3) -> () {
                %and1 = and i1 %in1, %in2
                %or1 = or i1 %and1, %in3
            }
        "};
        let module = llhd::assembly::parse_module(input).unwrap();
        let unit = module.units().next().unwrap();
        let and_inst = unit.all_insts().next().unwrap();
        let and_inst_name = get_inst_name(&module, &unit, and_inst);
        assert_eq!(
            "ent2.and.i1", and_inst_name,
            "And instruction name does not match."
        );
    }

    #[test]
    fn llhd_get_name_for_inst_call() {
        let input = indoc::indoc! {"
            proc %top.and (i1$ %in1, i1$ %in2) -> (i1$ %out1) {
            %init:
                %epsilon = const time 0s 1e
                %in1_prb = prb i1$ %in1
                %in2_prb = prb i1$ %in2
                %and1 = and i1 %in1_prb, %in2_prb
                drv i1$ %out1, %and1, %epsilon
                wait %init for %epsilon
            }

            entity @top () -> () {
                %top_input1 = const i1 0
                %in1 = sig i1 %top_input1
                %top_input2 = const i1 1
                %in2 = sig i1 %top_input2
                %top_out1 = const i1 0
                %out1 = sig i1 %top_out1
                inst %top.and (i1$ %in1, i1$ %in2) -> (i1$ %out1)
            }
        "};
        let module = llhd::assembly::parse_module(input).unwrap();
        let inst_info = module
            .units()
            .map(|module_unit| {
                let unit_id = module_unit.id();
                let unit_name = module_unit.name().to_string();
                (module_unit, unit_id, unit_name)
            })
            .filter(|(_, _, unit_name)| unit_name == "@top")
            .map(|(module_unit, unit_id, _)| (module_unit, unit_id))
            .flat_map(|(module_unit, unit_id)| {
                module_unit
                    .all_insts()
                    .filter(move |inst| match &module_unit[*inst] {
                        InstData::Call { .. } => true,
                        _ => false,
                    })
                    .map(move |inst| (unit_id, inst, module_unit[inst].to_owned()))
                    .map(|(unit_id, inst, inst_data)| {
                        let mut ext_unit_id = ExtUnit::new(0);
                        if let InstData::Call { unit, .. } = inst_data {
                            ext_unit_id = unit;
                        }
                        (unit_id, inst, ext_unit_id)
                    })
            })
            .collect::<Vec<(UnitId, Inst, ExtUnit)>>();
        let top_unit = module.unit(inst_info[0].0);
        let and_inst = inst_info[0].1;
        let and_inst_name = get_inst_name(&module, &top_unit, and_inst);
        assert_eq!(
            "top.top.and.i7", and_inst_name,
            "And instantiation name does not match."
        );
    }

    #[test]
    fn llhd_get_name_for_value() {
        let input = indoc::indoc! {"
            proc %top.and (i1$ %in1, i1$ %in2) -> (i1$ %out1) {
            %init:
                %epsilon = const time 0s 1e
                %in1_prb = prb i1$ %in1
                %in2_prb = prb i1$ %in2
                %and1 = and i1 %in1_prb, %in2_prb
                drv i1$ %out1, %and1, %epsilon
                wait %init for %epsilon
            }

            entity @top () -> () {
                %top_input1 = const i1 0
                %in1 = sig i1 %top_input1
                %top_input2 = const i1 1
                %in2 = sig i1 %top_input2
                %top_out1 = const i1 0
                %out1 = sig i1 %top_out1
                inst %top.and (i1$ %in1, i1$ %in2) -> (i1$ %out1)
            }
        "};
        let module = llhd::assembly::parse_module(input).unwrap();
        let inst_info = module
            .units()
            .map(|module_unit| {
                let unit_id = module_unit.id();
                let unit_name = module_unit.name().to_string();
                (module_unit, unit_id, unit_name)
            })
            .filter(|(_, _, unit_name)| unit_name == "@top")
            .map(|(module_unit, unit_id, _)| (module_unit, unit_id))
            .flat_map(|(module_unit, unit_id)| {
                module_unit
                    .all_insts()
                    .filter(move |inst| match &module_unit[*inst] {
                        InstData::Call { .. } => true,
                        _ => false,
                    })
                    .map(move |inst| {
                        let value = module_unit[inst].args().last().unwrap().to_owned();
                        (unit_id, value, module_unit[inst].to_owned())
                    })
                    .map(|(unit_id, value, inst_data)| {
                        let mut ext_unit_id = ExtUnit::new(0);
                        if let InstData::Call { unit, .. } = inst_data {
                            ext_unit_id = unit;
                        }
                        (unit_id, value, ext_unit_id)
                    })
            })
            .collect::<Vec<(UnitId, Value, ExtUnit)>>();
        let top_unit = module.unit(inst_info[0].0);
        let and_value = inst_info[0].1;
        let and_inst_name = get_value_name(&module, &top_unit, and_value);
        assert_eq!(
            "top.v5", and_inst_name,
            "And instantiation name does not match."
        );
    }

    #[test]
    fn llhd_get_instantiation_insts() {
        let input = indoc::indoc! {"
            proc %top.and (i1$ %in1, i1$ %in2) -> (i1$ %out1) {
            %init:
                %epsilon = const time 0s 1e
                %in1_prb = prb i1$ %in1
                %in2_prb = prb i1$ %in2
                %and1 = and i1 %in1_prb, %in2_prb
                drv i1$ %out1, %and1, %epsilon
                wait %init for %epsilon
            }

            entity @top () -> () {
                %top_input1 = const i1 0
                %in1 = sig i1 %top_input1
                %top_input2 = const i1 1
                %in2 = sig i1 %top_input2
                %top_out1 = const i1 0
                %out1 = sig i1 %top_out1
                inst %top.and (i1$ %in1, i1$ %in2) -> (i1$ %out1)
            }
        "};
        let module = llhd::assembly::parse_module(input).unwrap();
        let top_unit = module
            .units()
            .map(|module_unit| {
                let unit_id = module_unit.id();
                let unit_name = module_unit.name().to_string();
                (module_unit, unit_id, unit_name)
            })
            .filter(|(_, _, unit_name)| unit_name == "@top")
            .map(|(module_unit, _, _)| module_unit)
            .collect::<Vec<Unit>>()[0];
        assert_eq!("top", get_unit_name(&top_unit));
        let unit_insts: Vec<_> = top_unit
            .all_insts()
            .filter(|inst| filter_instantiations(&top_unit, *inst))
            .collect();
        assert_eq!(
            1,
            unit_insts.len(),
            "There should be 1 Call/Instantiation Instruction present in Unit."
        );
        let instantiation_inst = unit_insts[0];
        let instantiation_inst_name = get_inst_name(&module, &top_unit, instantiation_inst);
        assert_eq!(
            "top.top.and.i7", instantiation_inst_name,
            "Instantiation Inst name should match."
        );
    }
}
