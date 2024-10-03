pub(crate) mod utils;

#[cfg(test)]
mod tests {
    use itertools::Itertools;

    use crate::llhd::LLHDUtils;

    extern crate utilities;

    #[test]
    fn build_unit_component() {
        let module = utilities::load_llhd_module("testbench_example1.llhd");
        let units = LLHDUtils::iterate_unit_ids(&module).collect_vec();
        assert_eq!(2, units.len(), "There should be 2 Units present in Module.");
        let first_unit = module.units().collect_vec()[0];
        let second_unit = module.units().collect_vec()[1];
        let first_unit_args = LLHDUtils::iterate_unit_arg_defs(&first_unit).collect_vec();
        assert_eq!(
            4,
            first_unit_args.len(),
            "There should be 4 args present in first unit."
        );
        let second_unit_args = LLHDUtils::iterate_unit_arg_defs(&second_unit).collect_vec();
        assert_eq!(
            0,
            second_unit_args.len(),
            "There should be 3 args present in second unit."
        );
    }
}
