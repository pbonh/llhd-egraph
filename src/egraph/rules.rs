use std::ops::Deref;

use egglog::ast::{Command, GenericRule, Symbol};
use egglog::EGraph;
use itertools::Itertools;

use crate::egraph::EgglogCommandList;

#[derive(Debug, Clone, Default)]
pub struct EgglogRules(EgglogCommandList);

type EgglogRule<Call, Var> = (Symbol, Symbol, GenericRule<Call, Var>);

impl EgglogRules {
    pub fn add_rules<SymbolList>(self, ruleset_names: SymbolList) -> Self
    where
        SymbolList: IntoIterator<Item = Command>,
    {
        let mut rulesets = ruleset_names
            .into_iter()
            .filter(|command| {
                matches!(*command, Command::AddRuleset(..))
                    || matches!(*command, Command::Rule { .. })
                    || matches!(*command, Command::Rewrite { .. })
                    || matches!(*command, Command::BiRewrite { .. })
            })
            .collect_vec();
        let mut updated_rulesets = Self(self.0);
        updated_rulesets.0.append(&mut rulesets);
        updated_rulesets
    }

    pub fn add_rule_str(self, rule_str: &str) -> Self {
        match EGraph::default().parse_program(None, rule_str) {
            Ok(rule_commands) => Self::add_rules(self, rule_commands),
            Err(error) => panic!("Failure to build rules from string: {:?}", error),
        }
    }
}

impl Deref for EgglogRules {
    type Target = EgglogCommandList;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<EgglogCommandList> AsRef<EgglogCommandList> for EgglogRules
where
    EgglogCommandList: ?Sized,
    <Self as Deref>::Target: AsRef<EgglogCommandList>,
{
    fn as_ref(&self) -> &EgglogCommandList {
        self.deref().as_ref()
    }
}

impl Into<EgglogCommandList> for EgglogRules {
    fn into(self) -> EgglogCommandList {
        self.0
    }
}

impl IntoIterator for EgglogRules {
    type Item = Command;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

#[cfg(test)]
mod tests {
    use egglog::ast::GenericCommand;

    use super::*;

    #[test]
    fn create_egglog_rules_from_ruleset() {
        let ruleset_symbol = Symbol::new("rule1");
        let rule1 = GenericCommand::AddRuleset(ruleset_symbol);
        let egglog_rules = EgglogRules::default().add_rules(vec![rule1]);
        assert_eq!(
            1,
            egglog_rules.len(),
            "There should be 1 command in ruleset."
        );
        assert!(
            matches!(egglog_rules[0], GenericCommand::AddRuleset(..)),
            "First command should be a ruleset."
        );
        if let GenericCommand::AddRuleset(rule_symbol) = egglog_rules[0] {
            assert_eq!("rule1", rule_symbol.as_str(), "Rule name does not match.");
        }
    }

    #[test]
    fn create_egglog_rules_from_str() {
        let egglog_rule_str = utilities::get_egglog_commands("llhd_div_extract.egg");
        let egglog_rules = EgglogRules::default().add_rule_str(&egglog_rule_str);
        assert_eq!(
            2,
            egglog_rules.len(),
            "There should be 2 command in rules, 1 ruleset and 1 rule."
        );
        assert!(
            matches!(egglog_rules[0], GenericCommand::AddRuleset(..)),
            "First command should be a ruleset."
        );
        assert!(
            matches!(egglog_rules[1], GenericCommand::Rewrite { .. }),
            "Second command should be a rule."
        );
        if let GenericCommand::AddRuleset(rule_symbol) = egglog_rules[0] {
            assert_eq!("div-ext", rule_symbol.as_str(), "Rule name does not match.");
        }
    }
}
