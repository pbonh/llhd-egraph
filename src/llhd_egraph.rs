pub mod datatype;
mod egglog_names;
mod inst;
pub mod unit;
pub use unit::LLHDEgglogFacts;
pub mod llhd;
pub mod rules;
pub mod schedules;

#[cfg(test)]
mod tests {
    use egglog::ast::Symbol;
    use itertools::Itertools;
    use std::str::FromStr;

    use crate::llhd::module::LLHDModule;
    use crate::llhd::LLHDModuleTester;
    use crate::llhd_egraph::datatype::LLHDEgglogSorts;
    use crate::llhd_egraph::llhd::LLHDEgglogRuleset;
    use crate::llhd_egraph::rules::LLHDEgglogRules;
    use crate::llhd_egraph::schedules::LLHDEgglogSchedules;
    use crate::llhd_egraph::unit::LLHDEgglogFacts;
    use egglog_program::egraph::schedule::EgglogSchedules;
    use egglog_program::egraph::EgglogCommandList;
    use egglog_program::egraph::*;

    use super::llhd::LLHDEgglogProgram;

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

    #[test]
    fn llhd_rewrite_egglog_program_concise1() {
        let test_module = utilities::load_llhd_module("2and_1or_common.llhd");
        let test_module_facts = LLHDEgglogFacts::from_module(&test_module);
        let llhd_bindings: EgglogSymbols = test_module
            .units()
            .map(|unit| {
                let unit_stmt_prefix: &str = "unit_";
                let mut unit_name = unit.name().to_string().replace(&['@', '%', ','][..], "");
                unit_name.insert_str(0, unit_stmt_prefix);
                Symbol::new(unit_name)
            })
            .collect();
        let llhd_egglog_program = LLHDEgglogProgram::builder()
            .facts(test_module_facts)
            .rules(
                LLHDEgglogRules::from_str(&utilities::get_egglog_commands("llhd_div_extract.egg"))
                    .unwrap(),
            )
            .build();
        let llhd_egglog_schedule = EgglogSchedules::default().add_schedule_str(
            &utilities::get_egglog_commands("llhd_div_extract_schedule.egg"),
        );
        let egglog_program = EgglogProgramBuilder::<InitState>::new()
            .sorts(llhd_egglog_program.sorts().clone().into())
            .facts(llhd_egglog_program.facts().clone().into())
            .rules(llhd_egglog_program.rules().clone().into())
            .schedules(llhd_egglog_schedule)
            .bindings(llhd_bindings)
            .program();

        let extracted_module = LLHDModule::from(egglog_program);
        let expected_module = utilities::load_llhd_module("2and_1or_common_extracted.llhd");
        let extracted_module_data = LLHDModuleTester::from(extracted_module);
        let expected_module_data = LLHDModuleTester::from(expected_module);
        assert_eq!(
            expected_module_data, extracted_module_data,
            "Extracted Module doesn't match expected Module after divisor extraction."
        );
    }

    #[test]
    fn llhd_rewrite_egglog_program_concise2() {
        let test_module: LLHDModule = utilities::load_llhd_module("2and_1or_common.llhd").into();
        let llhd_egglog_rule =
            LLHDEgglogRules::from_str(&utilities::get_egglog_commands("llhd_div_extract.egg"))
                .unwrap();
        let llhd_egglog_schedule = LLHDEgglogSchedules::from_str(&utilities::get_egglog_commands(
            "llhd_div_extract_schedule.egg",
        ))
        .unwrap();
        let test_module_ruleset = LLHDEgglogRuleset {
            module: test_module,
            rules: llhd_egglog_rule,
            schedules: llhd_egglog_schedule,
        };
        let egglog_program = EgglogProgram::from(test_module_ruleset);

        let extracted_module = LLHDModule::from(egglog_program);
        let expected_module = utilities::load_llhd_module("2and_1or_common_extracted.llhd");
        let extracted_module_data = LLHDModuleTester::from(extracted_module);
        let expected_module_data = LLHDModuleTester::from(expected_module);
        assert_eq!(
            expected_module_data, extracted_module_data,
            "Extracted Module doesn't match expected Module after divisor extraction."
        );
    }
}
