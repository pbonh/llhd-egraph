use std::str::FromStr;

use egglog_program::*;

#[derive(Debug, Clone, Default)]
pub struct LLHDEgglogRules(pub(in crate::llhd_egraph) EgglogCommandList);

impl FromStr for LLHDEgglogRules {
    type Err = egglog::ast::parse::ParseError;

    fn from_str(rule_str: &str) -> Result<Self, Self::Err> {
        match egglog::ast::parse_program(None, rule_str) {
            Ok(rule_cmds) => Ok(Self(rule_cmds)),
            Err(err_msgs) => Err(err_msgs),
        }
    }
}

impl From<LLHDEgglogRules> for EgglogCommandList {
    fn from(rules: LLHDEgglogRules) -> Self {
        rules.0
    }
}

impl From<LLHDEgglogRules> for EgglogRules {
    fn from(llhd_rules: LLHDEgglogRules) -> Self {
        Self::default().add_rules(<LLHDEgglogRules as Into<EgglogCommandList>>::into(
            llhd_rules,
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::llhd_egraph::llhd::LLHDEGraph;

    #[test]
    fn create_llhd_rules_from_str() {
        let mut llhd_egraph = LLHDEGraph::default();
        let rule_cmds_result =
            LLHDEgglogRules::from_str(&utilities::get_egglog_commands("llhd_div_extract.egg"));
        if let Err(err_msg) = rule_cmds_result {
            panic!("Failure to parse LLHD Egglog rules. Err: {:?}", err_msg);
        }
        let rule_cmds = rule_cmds_result.unwrap();
        let egraph_with_rules_msgs = llhd_egraph.run_program(rule_cmds.clone().into());
        assert!(egraph_with_rules_msgs.is_ok());
        assert_eq!(
            2,
            <LLHDEgglogRules as Into<EgglogCommandList>>::into(rule_cmds).len(),
            "There should be 2 rules present in rewrite(1 ruleset, and 1 rule)."
        );
    }
}
