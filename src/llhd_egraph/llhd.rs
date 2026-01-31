use std::ops::{Deref, DerefMut};

use egglog::ast::{GenericCommand, GenericExpr};
use egglog::{EGraph, Error, TermDag};
use typed_builder::TypedBuilder;

use super::datatype::LLHDEgglogSorts;
use super::rules::LLHDEgglogRules;
use super::schedules::LLHDEgglogSchedules;
use super::unit::LLHDEgglogFacts;
use crate::llhd::module::LLHDModule;
use crate::llhd_egraph::unit::{expr_to_unit_data, expr_to_unit_info, unit_symbol};
use egglog_program::*;

#[derive(Debug, Clone, Default, TypedBuilder)]
pub struct LLHDEgglogProgram {
    #[builder(default=LLHDEgglogSorts::llhd_full())]
    sorts: LLHDEgglogSorts,

    #[builder(default)]
    facts: LLHDEgglogFacts,

    #[builder(default)]
    rules: LLHDEgglogRules,

    #[builder(default)]
    schedules: LLHDEgglogSchedules,
}

impl LLHDEgglogProgram {
    pub const fn sorts(&self) -> &LLHDEgglogSorts {
        &self.sorts
    }

    pub const fn facts(&self) -> &LLHDEgglogFacts {
        &self.facts
    }

    pub const fn rules(&self) -> &LLHDEgglogRules {
        &self.rules
    }

    pub const fn schedules(&self) -> &LLHDEgglogSchedules {
        &self.schedules
    }
}

// impl Add for LLHDEgglogProgram {
//     type Output = Self;
//
//     fn add(mut self, mut rhs: Self) -> Self::Output {
//         self.sorts.0 = rhs.sorts.0;
//         self.facts.0.append(&mut rhs.facts.0);
//         self.rules.0.append(&mut rhs.rules.0);
//         self.schedules.0.append(&mut rhs.schedules.0);
//         self
//     }
// }

impl From<LLHDEgglogFacts> for LLHDEgglogProgram {
    fn from(facts: LLHDEgglogFacts) -> Self {
        Self::builder().facts(facts).build()
    }
}

#[derive(Clone)]
pub struct LLHDEGraph(EGraph);

impl TryFrom<LLHDEgglogProgram> for LLHDEGraph {
    type Error = Error;

    fn try_from(program: LLHDEgglogProgram) -> Result<Self, Self::Error> {
        let mut egraph = EGraph::default();
        match egraph.run_program(program.sorts().to_owned().into()) {
            Ok(_sorts_msgs) => match egraph.run_program(program.rules().to_owned().into()) {
                Ok(_rules_msgs) => match egraph.run_program(program.facts().to_owned().into()) {
                    Ok(_facts_msgs) => Ok(Self(egraph)),
                    Err(egraph_error) => Err(egraph_error),
                },
                Err(egraph_error) => Err(egraph_error),
            },
            Err(egraph_error) => Err(egraph_error),
        }
    }
}

impl Default for LLHDEGraph {
    fn default() -> Self {
        let mut egraph = EGraph::default();
        let llhd_inst_msgs = egraph.run_program(LLHDEgglogSorts::default().into());
        if let Err(egraph_msg) = llhd_inst_msgs {
            panic!("Failure to load LLHD Prelude. Err: {:?}", egraph_msg);
        }
        Self(egraph)
    }
}

impl Deref for LLHDEGraph {
    type Target = EGraph;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for LLHDEGraph {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<EGraph> AsRef<EGraph> for LLHDEGraph
where
    EGraph: ?Sized,
    <Self as Deref>::Target: AsRef<EGraph>,
{
    fn as_ref(&self) -> &EGraph {
        self.deref().as_ref()
    }
}

impl<EGraph> AsMut<EGraph> for LLHDEGraph
where
    <Self as Deref>::Target: AsMut<EGraph>,
{
    fn as_mut(&mut self) -> &mut EGraph {
        self.deref_mut().as_mut()
    }
}

impl From<LLHDModule> for EgglogProgram {
    fn from(module: LLHDModule) -> Self {
        let llhd_dfg_sort = LLHDEgglogSorts::llhd_full();
        let module_facts = LLHDEgglogFacts::from_module(&module);
        let unit_symbols: EgglogSymbols = module.units().map(unit_symbol).collect();
        EgglogProgramBuilder::initialize()
            .sorts(llhd_dfg_sort.into())
            .facts(module_facts.into())
            .bindings(unit_symbols)
            .variables()
    }
}

pub struct LLHDEgglogRuleset {
    pub module: LLHDModule,
    pub rules: LLHDEgglogRules,
    pub schedules: LLHDEgglogSchedules,
}

impl From<LLHDEgglogRuleset> for EgglogProgram {
    fn from(data: LLHDEgglogRuleset) -> Self {
        let (module, llhd_rules, llhd_schedules) = (data.module, data.rules, data.schedules);
        let llhd_dfg_sort = LLHDEgglogSorts::llhd_full();
        let module_facts = LLHDEgglogFacts::from_module(&module);
        let rules = EgglogRules::from(llhd_rules);
        let schedules = EgglogSchedules::from(llhd_schedules);
        let unit_symbols: EgglogSymbols = module.units().map(unit_symbol).collect();
        EgglogProgramBuilder::initialize()
            .sorts(llhd_dfg_sort.into())
            .facts(module_facts.into())
            .rules(rules)
            .schedules(schedules)
            .bindings(unit_symbols)
            .program()
    }
}

impl From<&LLHDModule> for EgglogProgram {
    fn from(module: &LLHDModule) -> Self {
        let llhd_dfg_sort = LLHDEgglogSorts::llhd_full();
        let module_facts = LLHDEgglogFacts::from_module(module);
        let unit_symbols: EgglogSymbols = module.units().map(unit_symbol).collect();
        EgglogProgramBuilder::initialize()
            .sorts(llhd_dfg_sort.into())
            .facts(module_facts.into())
            .bindings(unit_symbols)
            .variables()
    }
}

impl From<EgglogProgram> for LLHDModule {
    fn from(program: EgglogProgram) -> Self {
        let unit_symbols = program.bindings().to_owned();
        let mut module = Self::default();
        let mut egraph = EGraph::default();
        if let Err(err_msg) = egraph.run_program(program.clone().into()) {
            println!("{:?}", program.facts());
            panic!("Failure to run EgglogProgram. Err: {:?}", err_msg);
        }
        for unit_symbol in unit_symbols.into_iter() {
            let extract_cmd = GenericCommand::QueryExtract {
                span: DUMMY_SPAN.clone(),
                variants: 0,
                expr: GenericExpr::Var(DUMMY_SPAN.clone(), unit_symbol),
            };
            if let Err(egraph_extract_err) = egraph.run_program(vec![extract_cmd]) {
                println!("Cannot extract expression: {:?}", egraph_extract_err);
            }
            let mut extracted_termdag = TermDag::default();
            let (unit_sort, unit_symbol_value) = egraph
                .eval_expr(&GenericExpr::Var(DUMMY_SPAN.clone(), unit_symbol))
                .unwrap();
            // let (_unit_cost, unit_term) =
            match egraph.extract(unit_symbol_value, &mut extracted_termdag, &unit_sort) {
                Ok((_unit_cost, unit_term)) => {
                    let extracted_expr =
                        extracted_termdag.term_to_expr(&unit_term, DUMMY_SPAN.clone());
                    let (unit_kind_extract, unit_name_extract, unit_sig_extract) =
                        expr_to_unit_info(extracted_expr.clone());
                    let unit_data = expr_to_unit_data(
                        extracted_expr,
                        unit_kind_extract,
                        unit_name_extract,
                        unit_sig_extract,
                    );
                    let _unit_id = module.add_unit(unit_data);
                }
                Err(msg) => panic!("Failure to extract Term DAG from EGraph: {:?}", msg),
            }
        }
        module.verify();
        module
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;
    use llhd::ir::{UnitKind, UnitName};
    use std::str::FromStr;

    use super::*;

    extern crate utilities;

    #[test]
    fn build_llhd_egglog_program() {
        let llhd_egglog_program = LLHDEgglogProgram::builder()
            .rules(
                LLHDEgglogRules::from_str(&utilities::get_egglog_commands("llhd_div_extract.egg"))
                    .unwrap(),
            )
            .build();
        let egraph_msgs = LLHDEGraph::try_from(llhd_egglog_program);
        assert!(
            egraph_msgs.is_ok(),
            "Error loading LLHD DFG Datatype. Error: {:?}",
            egraph_msgs.err().unwrap()
        );
    }

    // #[test]
    // fn add_llhd_egglog_programs() {
    //     let llhd_egglog_program_div_extract = LLHDEgglogProgram::builder()
    //         .rules(
    //             LLHDEgglogRules::from_str(&utilities::get_egglog_commands("llhd_div_extract.egg"))
    //                 .unwrap(),
    //         )
    //         .build();
    //     assert_eq!(
    //         2,
    //         llhd_egglog_program_div_extract.rules().0.len(),
    //         "There should be 2 rules in div_extract program."
    //     );
    //     let llhd_egglog_program_demorgans_theorem = LLHDEgglogProgram::builder()
    //         .rules(
    //             LLHDEgglogRules::from_str(&utilities::get_egglog_commands(
    //                 "llhd_demorgans_theorem.egg",
    //             ))
    //             .unwrap(),
    //         )
    //         .build();
    //     assert_eq!(
    //         2,
    //         llhd_egglog_program_demorgans_theorem.rules().0.len(),
    //         "There should be 2 rules in demorgans_theorem program."
    //     );
    //     let combined_program =
    //         llhd_egglog_program_div_extract + llhd_egglog_program_demorgans_theorem;
    //     assert_eq!(
    //         4,
    //         combined_program.rules().0.len(),
    //         "There should be 4 rules in combined program."
    //     );
    //     let egraph = LLHDEGraph::try_from(combined_program);
    //     if let Err(err_msg) = egraph {
    //         panic!("Error building EGraph. Err: {:?}", err_msg);
    //     }
    // }

    #[test]
    fn default_llhd_egraph() {
        let _egraph = LLHDEGraph::default();
    }

    #[test]
    fn build_llhd_egraph() {
        let program: LLHDEgglogProgram = Default::default();
        let egraph_msgs = LLHDEGraph::try_from(program);
        assert!(
            egraph_msgs.is_ok(),
            "Error loading LLHD DFG Datatype. Error: {:?}",
            egraph_msgs.err().unwrap()
        );
    }

    #[test]
    fn egglog_program_from_facts() {
        let test_module = utilities::load_llhd_module("2and_1or_common.llhd");
        let test_module_facts = LLHDEgglogFacts::from_module(&test_module);
        let llhd_egglog_program = LLHDEgglogProgram::from(test_module_facts);
        let _egraph = LLHDEGraph::try_from(llhd_egglog_program).unwrap();
    }

    #[test]
    fn egglog_program_from_llhd_unit() {
        let test_module = utilities::load_llhd_module("2and_1or_common.llhd");
        let llhd_egglog_program = LLHDEgglogProgram::builder()
            .rules(
                LLHDEgglogRules::from_str(&utilities::get_egglog_commands("llhd_div_extract.egg"))
                    .unwrap(),
            )
            .facts(LLHDEgglogFacts::from_module(&test_module))
            .build();
        let _egraph = LLHDEGraph::try_from(llhd_egglog_program).unwrap();
    }

    #[test]
    fn egglog_program_from_llhd_unit_with_unsigned_id_sort() {
        let _egraph = utilities::load_egraph("llhd_dfg_example2.egg");
    }

    // #[test]
    // const fn llhd_unit_sort_valid_egglog_program() {
    //     static LLHD_UNIT_SORT_EGGLOG_RESOURCES_STR: &str = include_str!(concat!(
    //         env!("CARGO_MANIFEST_DIR"),
    //         "/resources/egglog/llhd_dfg_sort.egg"
    //     ));
    //
    //     use egglog_syntax::egglog_expr_str;
    //     extern crate proc_macro;
    //
    //     let llhd_unit_sort_egglog_resources_stream: proc_macro::TokenStream =
    //         LLHD_UNIT_SORT_EGGLOG_RESOURCES_STR.parse().unwrap();
    //     let _llhd_dfg_egglog_expr = egglog_expr_str!(llhd_unit_sort_egglog_resources_stream);
    // }

    use test_log::test;
    #[test_log::test]
    fn egglog_program_from_llhd_module() {
        let test_module: LLHDModule = utilities::load_llhd_module("2and_1or_common.llhd").into();

        let egglog_program = EgglogProgram::from(test_module);
        assert_eq!(24, egglog_program.sorts().1.len());
        assert_eq!(1, egglog_program.facts().1.len());
        assert_eq!(0, egglog_program.rules().len());
        assert_eq!(0, egglog_program.schedules().len());
        assert_eq!(1, egglog_program.bindings().len());
        println!("{:?}", egglog_program.sorts());
        let round_trip_test_module = LLHDModule::from(egglog_program);
        let unit_ids = round_trip_test_module
            .units()
            .map(|unit| unit.id())
            .collect_vec();
        assert_eq!(1, unit_ids.len());
        let round_trip_test_entity = round_trip_test_module.unit(unit_ids[0]);
        assert!(matches!(round_trip_test_entity.kind(), UnitKind::Entity));
        if let UnitName::Global(unit_name) = round_trip_test_entity.name() {
            let unit_name_str = unit_name.to_string();
            assert_eq!("\"test_entity\"", unit_name_str);
        } else {
            panic!("UnitName is not Global type.");
        }
    }

    // ({"unit_test_entity"}, EgglogFacts([Action(Let(In 1:1-1: , "unit_test_entity",
    // Call(In 1:1-1: , "LLHDUnit", [
    // Lit(In 1:1-1: , Int(0)),
    // Call(In 1:1-1: , "Entity", []),
    // Lit(In 1:1-1: , String("@test_entity")),
    // Call(In 1:1-1: , "vec-of", [Call(In 1:1-1: , "Value", [Call(In 1:1-1: , "Id", [Lit(In 1:1-1: , Int(1))]), Lit(In 1:1-1: , Int(0))]),
    // Call(In 1:1-1: , "Value", [Call(In 1:1-1: , "Id", [Lit(In 1:1-1: , Int(1))]), Lit(In 1:1-1: , Int(1))]),
    // Call(In 1:1-1: , "Value", [Call(In 1:1-1: , "Id", [Lit(In 1:1-1: , Int(1))]), Lit(In 1:1-1: , Int(2))])]),
    // Call(In 1:1-1: , "vec-of", [Call(In 1:1-1: , "Value", [Call(In 1:1-1: , "Signal", [Call(In 1:1-1: , "Id", [Lit(In 1:1-1: , Int(1))])]), Lit(In 1:1-1: , Int(3))])]),
    // Call(In 1:1-1: , "Drv", [Lit(In 1:1-1: , Int(5)), Call(In 1:1-1: , "Void", []), Call(In 1:1-1: , "ValueRef", [Call(In 1:1-1: , "Value", [Call(In 1:1-1: , "Signal", [Call(In 1:1-1: , "Id", [Lit(In 1:1-1: , Int(1))])]), Lit(In 1:1-1: , Int(3))])]), Call(In 1:1-1: , "Or", [Lit(In 1:1-1: , Int(4)), Call(In 1:1-1: , "Id", [Lit(In 1:1-1: , Int(1))]), Call(In 1:1-1:, "And", [Lit(In 1:1-1: , Int(2)), Call(In 1:1-1: , "Id", [Lit(In 1:1-1: , Int(1))]), Call(In 1:1-1: , "ValueRef", [Call(In 1:1-1: , "Value", [Call(In 1:1-1: , "Id", [Lit(In 1:1-1: , Int(1))]), Lit(In 1:1-1: , Int(0))])]), Call(In 1:1-1: , "ValueRef", [Call(In 1:1-1: , "Value", [Call(In 1:1-1: , "Id", [Lit(In 1:1-1: , Int(1))]), Lit(In 1:1-1: , Int(1))])])]), Call(In 1:1-1: , "And", [Lit(In 1:1-1: , Int(3)), Call(In 1:1-1: , "Id", [Lit(In 1:1-1: , Int(1))]), Call(In 1:1-1: , "ValueRef", [Call(In 1:1-1: , "Value", [Call(In 1:1-1: , "Id", [Lit(In 1:1-1: , Int(1))]), Lit(In 1:1-1: , Int(2))])]), Call(In 1:1-1: , "ValueRef", [Call(In 1:1-1: , "Value", [Call(In 1:1-1: , "Id", [Lit(In 1:1-1: , Int(1))]), Lit(In 1:1-1: , Int(1))])])])]), Call(In 1:1-1: , "ConstTime", [Lit(In 1:1-1: , Int(1)), Call(In 1:1-1: , "Time", []), Lit(In 1:1-1: , String("0s 1e"))])])
    // ])))]))
}
