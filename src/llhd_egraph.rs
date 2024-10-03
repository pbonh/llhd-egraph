pub mod datatype;
mod egglog_names;
mod inst;
pub(crate) mod unit;
pub use unit::LLHDEgglogFacts;
pub mod llhd;
pub mod rules;
pub mod schedules;

#[cfg(test)]
mod tests {
    use itertools::Itertools;

    use crate::egraph::EgglogCommandList;
    use crate::llhd_egraph::datatype::LLHDEgglogSorts;

    #[test]
    fn llhd_unit_dft_sort_valid_egglog_program() {
        let _llhd_sort_egraph = utilities::load_egraph("llhd_dfg_sort.egg");
    }

    #[test]
    fn llhd_egglog_dfg_datatypes() {
        let expected_str =
            utilities::trim_expr_whitespace(&utilities::get_egglog_commands("llhd_dfg_sort.egg"));
        let llhd_dfg_sort: EgglogCommandList = LLHDEgglogSorts::llhd_dfg().into();
        assert_eq!(
            expected_str,
            llhd_dfg_sort.into_iter().join(""),
            "LLHD DFG Egglog sorts don't match expected string."
        );
    }
}
