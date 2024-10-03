use std::ops::Deref;

use egglog::ast::Command;
use egglog::EGraph;
use itertools::Itertools;

use crate::egraph::EgglogCommandList;

#[derive(Debug, Clone, Default)]
pub struct EgglogFacts(EgglogCommandList);

impl EgglogFacts {
    pub fn add_facts<CommandList>(self, fact_list: CommandList) -> Self
    where
        CommandList: IntoIterator<Item = Command>,
    {
        let mut facts = fact_list
            .into_iter()
            .filter(|command| matches!(*command, Command::Action(..)))
            .collect_vec();
        let mut updated_facts = Self(self.0);
        updated_facts.0.append(&mut facts);
        updated_facts
    }

    pub fn add_facts_str(self, fact_str: &str) -> Self {
        match EGraph::default().parse_program(None, fact_str) {
            Ok(fact_commands) => Self::add_facts(self, fact_commands),
            Err(error) => panic!("Failure to build facts from string: {:?}", error),
        }
    }
}

impl Deref for EgglogFacts {
    type Target = EgglogCommandList;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<EgglogCommandList> AsRef<EgglogCommandList> for EgglogFacts
where
    EgglogCommandList: ?Sized,
    <Self as Deref>::Target: AsRef<EgglogCommandList>,
{
    fn as_ref(&self) -> &EgglogCommandList {
        self.deref().as_ref()
    }
}

impl Into<EgglogCommandList> for EgglogFacts {
    fn into(self) -> EgglogCommandList {
        self.0
    }
}

impl IntoIterator for EgglogFacts {
    type Item = Command;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

#[cfg(test)]
mod tests {

    use egglog::ast::{Action, Expr, GenericCommand, Literal, Symbol, DUMMY_SPAN};

    use super::*;

    #[test]
    fn create_egglog_facts_from_cmd() {
        let let_stmt1 = GenericCommand::Action(Action::Let(
            DUMMY_SPAN.clone(),
            Symbol::new("var1"),
            Expr::Lit(DUMMY_SPAN.clone(), Literal::UInt(0)),
        ));
        let egglog_sorts = EgglogFacts::default().add_facts(vec![let_stmt1]);
        assert_eq!(
            1,
            egglog_sorts.len(),
            "There should be 1 commands present, one for each action stmt."
        );
    }
}
