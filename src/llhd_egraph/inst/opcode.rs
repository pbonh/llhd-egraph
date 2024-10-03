use std::collections::HashMap;

use egglog::ast::Symbol;
use lazy_static::lazy_static;
use llhd::ir::Opcode;

type LLHDOpcodeSymbolLookup = HashMap<Symbol, Opcode>;

lazy_static! {
    pub(in crate::llhd_egraph) static ref OPCODESYMBOLMAP: LLHDOpcodeSymbolLookup = {
        let mut opcode_symbol_map = HashMap::new();
        opcode_symbol_map.insert(opcode_symbol(Opcode::ConstInt), Opcode::ConstInt);
        opcode_symbol_map.insert(opcode_symbol(Opcode::ConstTime), Opcode::ConstTime);
        opcode_symbol_map.insert(opcode_symbol(Opcode::Alias), Opcode::Alias);
        opcode_symbol_map.insert(opcode_symbol(Opcode::ArrayUniform), Opcode::ArrayUniform);
        opcode_symbol_map.insert(opcode_symbol(Opcode::Array), Opcode::Array);
        opcode_symbol_map.insert(opcode_symbol(Opcode::Struct), Opcode::Struct);
        opcode_symbol_map.insert(opcode_symbol(Opcode::Not), Opcode::Not);
        opcode_symbol_map.insert(opcode_symbol(Opcode::Neg), Opcode::Neg);
        opcode_symbol_map.insert(opcode_symbol(Opcode::Add), Opcode::Add);
        opcode_symbol_map.insert(opcode_symbol(Opcode::Sub), Opcode::Sub);
        opcode_symbol_map.insert(opcode_symbol(Opcode::And), Opcode::And);
        opcode_symbol_map.insert(opcode_symbol(Opcode::Or), Opcode::Or);
        opcode_symbol_map.insert(opcode_symbol(Opcode::Xor), Opcode::Xor);
        opcode_symbol_map.insert(opcode_symbol(Opcode::Smul), Opcode::Smul);
        opcode_symbol_map.insert(opcode_symbol(Opcode::Sdiv), Opcode::Sdiv);
        opcode_symbol_map.insert(opcode_symbol(Opcode::Smod), Opcode::Smod);
        opcode_symbol_map.insert(opcode_symbol(Opcode::Srem), Opcode::Srem);
        opcode_symbol_map.insert(opcode_symbol(Opcode::Umul), Opcode::Umul);
        opcode_symbol_map.insert(opcode_symbol(Opcode::Udiv), Opcode::Udiv);
        opcode_symbol_map.insert(opcode_symbol(Opcode::Umod), Opcode::Umod);
        opcode_symbol_map.insert(opcode_symbol(Opcode::Urem), Opcode::Urem);
        opcode_symbol_map.insert(opcode_symbol(Opcode::Eq), Opcode::Eq);
        opcode_symbol_map.insert(opcode_symbol(Opcode::Neq), Opcode::Neq);
        opcode_symbol_map.insert(opcode_symbol(Opcode::Slt), Opcode::Slt);
        opcode_symbol_map.insert(opcode_symbol(Opcode::Sgt), Opcode::Sgt);
        opcode_symbol_map.insert(opcode_symbol(Opcode::Sle), Opcode::Sle);
        opcode_symbol_map.insert(opcode_symbol(Opcode::Sge), Opcode::Sge);
        opcode_symbol_map.insert(opcode_symbol(Opcode::Ult), Opcode::Ult);
        opcode_symbol_map.insert(opcode_symbol(Opcode::Ugt), Opcode::Ugt);
        opcode_symbol_map.insert(opcode_symbol(Opcode::Ule), Opcode::Ule);
        opcode_symbol_map.insert(opcode_symbol(Opcode::Uge), Opcode::Uge);
        opcode_symbol_map.insert(opcode_symbol(Opcode::Shl), Opcode::Shl);
        opcode_symbol_map.insert(opcode_symbol(Opcode::Shr), Opcode::Shr);
        opcode_symbol_map.insert(opcode_symbol(Opcode::Mux), Opcode::Mux);
        opcode_symbol_map.insert(opcode_symbol(Opcode::Reg), Opcode::Reg);
        opcode_symbol_map.insert(opcode_symbol(Opcode::InsField), Opcode::InsField);
        opcode_symbol_map.insert(opcode_symbol(Opcode::InsSlice), Opcode::InsSlice);
        opcode_symbol_map.insert(opcode_symbol(Opcode::ExtField), Opcode::ExtField);
        opcode_symbol_map.insert(opcode_symbol(Opcode::ExtSlice), Opcode::ExtSlice);
        opcode_symbol_map.insert(opcode_symbol(Opcode::Con), Opcode::Con);
        opcode_symbol_map.insert(opcode_symbol(Opcode::Del), Opcode::Del);
        opcode_symbol_map.insert(opcode_symbol(Opcode::Call), Opcode::Call);
        opcode_symbol_map.insert(opcode_symbol(Opcode::Inst), Opcode::Inst);
        opcode_symbol_map.insert(opcode_symbol(Opcode::Sig), Opcode::Sig);
        opcode_symbol_map.insert(opcode_symbol(Opcode::Prb), Opcode::Prb);
        opcode_symbol_map.insert(opcode_symbol(Opcode::Drv), Opcode::Drv);
        opcode_symbol_map.insert(opcode_symbol(Opcode::DrvCond), Opcode::DrvCond);
        opcode_symbol_map.insert(opcode_symbol(Opcode::Var), Opcode::Var);
        opcode_symbol_map.insert(opcode_symbol(Opcode::Ld), Opcode::Ld);
        opcode_symbol_map.insert(opcode_symbol(Opcode::St), Opcode::St);
        opcode_symbol_map.insert(opcode_symbol(Opcode::Halt), Opcode::Halt);
        opcode_symbol_map.insert(opcode_symbol(Opcode::Ret), Opcode::Ret);
        opcode_symbol_map.insert(opcode_symbol(Opcode::RetValue), Opcode::RetValue);
        opcode_symbol_map.insert(opcode_symbol(Opcode::Phi), Opcode::Phi);
        opcode_symbol_map.insert(opcode_symbol(Opcode::Br), Opcode::Br);
        opcode_symbol_map.insert(opcode_symbol(Opcode::BrCond), Opcode::BrCond);
        opcode_symbol_map.insert(opcode_symbol(Opcode::Wait), Opcode::Wait);
        opcode_symbol_map.insert(opcode_symbol(Opcode::WaitTime), Opcode::WaitTime);
        opcode_symbol_map
    };
    pub(in crate::llhd_egraph) static ref OPCODESYMBOLMAP_COUNT: usize = OPCODESYMBOLMAP.len();
}

fn uppercase_first_letter(s: &mut str) {
    if let Some(r) = s.get_mut(0..1) {
        r.make_ascii_uppercase();
    }
}

pub(in crate::llhd_egraph) fn opcode_symbol(opcode: Opcode) -> Symbol {
    let mut opcode_str = opcode.to_string();
    match opcode {
        Opcode::ConstTime => opcode_str.push_str("Time"),
        Opcode::ConstInt => opcode_str.push_str("Int"),
        Opcode::DrvCond => opcode_str.push_str("Cond"),
        Opcode::ArrayUniform => opcode_str.push_str("Uniform"),
        Opcode::InsField => {
            opcode_str = opcode_str.replace("insf", "insField");
        }
        Opcode::InsSlice => {
            opcode_str = opcode_str.replace("inss", "insSlice");
        }
        Opcode::ExtField => {
            opcode_str = opcode_str.replace("extf", "extField");
        }
        Opcode::ExtSlice => {
            opcode_str = opcode_str.replace("exts", "extSlice");
        }
        Opcode::RetValue => opcode_str.push_str("Value"),
        Opcode::BrCond => opcode_str.push_str("Cond"),
        Opcode::WaitTime => opcode_str.push_str("Time"),
        _ => (),
    }
    uppercase_first_letter(&mut opcode_str);
    Symbol::new(opcode_str)
}

pub(in crate::llhd_egraph) fn get_symbol_opcode(symbol: &Symbol) -> Option<Opcode> {
    OPCODESYMBOLMAP.get(symbol).copied()
}

pub(in crate::llhd_egraph) fn symbol_opcode(symbol: Symbol) -> Opcode {
    OPCODESYMBOLMAP[&symbol]
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::llhd_egraph::inst::*;

    #[test]
    fn all_opcodes_available_in_egglog() {
        assert_eq!(
            LLHD_DFG_VARIANTS_COUNT.to_owned(),
            OPCODESYMBOLMAP_COUNT.to_owned() + 1,
            "Not all LLHD Inst Opcodes are available in Egglog."
        );
    }

    #[test]
    fn egglog_symbol_from_llhd_opcode() {
        let opcode = Opcode::Eq;
        let egglog_symbol = opcode_symbol(opcode);
        let expected_str = "Eq".to_owned();
        assert_eq!(
            expected_str,
            egglog_symbol.to_string(),
            "Opcode::Eq should be represented as 'Eq'."
        );
        let drv_opcode = Opcode::Drv;
        let drv_egglog_symbol = opcode_symbol(drv_opcode);
        let drv_expected_str = "Drv".to_owned();
        assert_eq!(
            drv_expected_str,
            drv_egglog_symbol.to_string(),
            "Opcode::Drv should be represented as 'Drv'."
        );
        let drv_cond_opcode = Opcode::DrvCond;
        let drv_cond_egglog_symbol = opcode_symbol(drv_cond_opcode);
        let drv_cond_expected_str = "DrvCond".to_owned();
        assert_eq!(
            drv_cond_expected_str,
            drv_cond_egglog_symbol.to_string(),
            "Opcode::DrvCond should be represented as 'DrvCond'."
        );
        let array_opcode = Opcode::ArrayUniform;
        let array_egglog_symbol = opcode_symbol(array_opcode);
        let array_expected_str = "ArrayUniform".to_owned();
        assert_eq!(
            array_expected_str,
            array_egglog_symbol.to_string(),
            "Opcode::Array should be represented as 'ArrayUniform'."
        );
        let ins_field_opcode = Opcode::InsField;
        let ins_field_egglog_symbol = opcode_symbol(ins_field_opcode);
        let ins_field_expected_str = "InsField".to_owned();
        assert_eq!(
            ins_field_expected_str,
            ins_field_egglog_symbol.to_string(),
            "Opcode::InsField should be represented as 'InsField'."
        );
        let ins_slice_opcode = Opcode::InsSlice;
        let ins_slice_egglog_symbol = opcode_symbol(ins_slice_opcode);
        let ins_slice_expected_str = "InsSlice".to_owned();
        assert_eq!(
            ins_slice_expected_str,
            ins_slice_egglog_symbol.to_string(),
            "Opcode::InsSlice should be represented as 'InsSlice'."
        );
        let ext_field_opcode = Opcode::ExtField;
        let ext_field_egglog_symbol = opcode_symbol(ext_field_opcode);
        let ext_field_expected_str = "ExtField".to_owned();
        assert_eq!(
            ext_field_expected_str,
            ext_field_egglog_symbol.to_string(),
            "Opcode::ExtField should be represented as 'ExtField'."
        );
        let ext_slice_opcode = Opcode::ExtSlice;
        let ext_slice_egglog_symbol = opcode_symbol(ext_slice_opcode);
        let ext_slice_expected_str = "ExtSlice".to_owned();
        assert_eq!(
            ext_slice_expected_str,
            ext_slice_egglog_symbol.to_string(),
            "Opcode::ExtSlice should be represented as 'ExtSlice'."
        );
        let ret_opcode = Opcode::Ret;
        let ret_egglog_symbol = opcode_symbol(ret_opcode);
        let ret_expected_str = "Ret".to_owned();
        assert_eq!(
            ret_expected_str,
            ret_egglog_symbol.to_string(),
            "Opcode::Ret should be represented as 'Ret'."
        );
        let ret_value_opcode = Opcode::RetValue;
        let ret_value_egglog_symbol = opcode_symbol(ret_value_opcode);
        let ret_value_expected_str = "RetValue".to_owned();
        assert_eq!(
            ret_value_expected_str,
            ret_value_egglog_symbol.to_string(),
            "Opcode::RetValue should be represented as 'RetValue'."
        );
        let br_opcode = Opcode::Br;
        let br_egglog_symbol = opcode_symbol(br_opcode);
        let br_expected_str = "Br".to_owned();
        assert_eq!(
            br_expected_str,
            br_egglog_symbol.to_string(),
            "Opcode::Br should be represented as 'Br'."
        );
        let br_cond_opcode = Opcode::BrCond;
        let br_cond_egglog_symbol = opcode_symbol(br_cond_opcode);
        let br_cond_expected_str = "BrCond".to_owned();
        assert_eq!(
            br_cond_expected_str,
            br_cond_egglog_symbol.to_string(),
            "Opcode::BrCond should be represented as 'BrCond'."
        );
        let wait_opcode = Opcode::Wait;
        let wait_egglog_symbol = opcode_symbol(wait_opcode);
        let wait_expected_str = "Wait".to_owned();
        assert_eq!(
            wait_expected_str,
            wait_egglog_symbol.to_string(),
            "Opcode::Wait should be represented as 'Wait'."
        );
        let wait_time_opcode = Opcode::WaitTime;
        let wait_time_egglog_symbol = opcode_symbol(wait_time_opcode);
        let wait_time_expected_str = "WaitTime".to_owned();
        assert_eq!(
            wait_time_expected_str,
            wait_time_egglog_symbol.to_string(),
            "Opcode::WaitTime should be represented as 'WaitTime'."
        );
    }

    #[test]
    fn llhd_opcode_from_egglog_symbol() {
        let symbol = Symbol::new("Eq");
        let opcode = symbol_opcode(symbol);
        let expected_opcode = Opcode::Eq;
        assert_eq!(
            expected_opcode, opcode,
            "Symbol('Eq') should be map to Opcode::Eq."
        );
        let drv_symbol = Symbol::new("Drv");
        let drv_opcode = symbol_opcode(drv_symbol);
        let drv_expected_opcode = Opcode::Drv;
        assert_eq!(
            drv_expected_opcode, drv_opcode,
            "Symbol('Drv') should be map to Opcode::Drv."
        );
    }
}
