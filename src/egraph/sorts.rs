use std::ops::Deref;

use egglog::ast::Command;
use egglog::EGraph;
use itertools::Itertools;

use crate::egraph::EgglogCommandList;

#[derive(Debug, Clone, Default)]
pub struct EgglogSorts(EgglogCommandList);

impl EgglogSorts {
    pub fn add_sorts<CommandList>(self, sort_list: CommandList) -> Self
    where
        CommandList: IntoIterator<Item = Command>,
    {
        let mut sorts = sort_list
            .into_iter()
            .filter(|command| {
                matches!(*command, Command::Sort(..))
                    || matches!(*command, Command::Datatype { .. })
                    || matches!(*command, Command::Relation { .. })
                    || matches!(*command, Command::Function(..))
            })
            .collect_vec();
        let mut updated_sorts = Self(self.0);
        updated_sorts.0.append(&mut sorts);
        updated_sorts
    }

    pub fn add_sort_str(self, sort_str: &str) -> Self {
        match EGraph::default().parse_program(None, sort_str) {
            Ok(sort_commands) => Self::add_sorts(self, sort_commands),
            Err(error) => panic!("Failure to build sorts from string: {:?}", error),
        }
    }
}

impl Deref for EgglogSorts {
    type Target = EgglogCommandList;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<EgglogCommandList> AsRef<EgglogCommandList> for EgglogSorts
where
    EgglogCommandList: ?Sized,
    <Self as Deref>::Target: AsRef<EgglogCommandList>,
{
    fn as_ref(&self) -> &EgglogCommandList {
        self.deref().as_ref()
    }
}

impl From<EgglogSorts> for EgglogCommandList {
    fn from(sorts: EgglogSorts) -> Self {
        sorts.0
    }
}

impl IntoIterator for EgglogSorts {
    type Item = Command;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

#[cfg(test)]
mod tests {

    use egglog::ast::{GenericActions, GenericCommand, Schema, Symbol, DUMMY_SPAN};

    use super::*;

    #[test]
    fn create_egglog_sorts_from_cmd() {
        let sort1 = GenericCommand::Sort(DUMMY_SPAN.clone(), Symbol::new("sort1"), None);
        let datatype1 = GenericCommand::Datatype {
            span: DUMMY_SPAN.clone(),
            name: Symbol::new("datatype1"),
            variants: vec![],
        };
        let relation1 = GenericCommand::Relation {
            span: DUMMY_SPAN.clone(),
            constructor: Symbol::new("relation1"),
            inputs: vec![],
        };
        let function1 = GenericCommand::Function(egglog::ast::GenericFunctionDecl {
            name: Symbol::new("func1"),
            schema: Schema {
                input: vec![],
                output: Symbol::new("func1_out"),
            },
            default: None,
            merge: None,
            merge_action: GenericActions::default(),
            cost: None,
            unextractable: false,
            ignore_viz: false,
            span: DUMMY_SPAN.clone(),
        });
        let egglog_sorts =
            EgglogSorts::default().add_sorts(vec![sort1, datatype1, relation1, function1]);
        assert_eq!(
            4,
            egglog_sorts.len(),
            "There should be 4 commands present, one for each declaration."
        );
    }

    #[test]
    fn create_egglog_sorts_from_str() {
        let sort_str = utilities::get_egglog_commands("llhd_dfg_example1.egg");
        let sort = EgglogSorts::default().add_sort_str(&sort_str);
        assert_eq!(
            4,
            sort.len(),
            "There should be 4 sorts/datatypes/declarations present in program."
        );
    }
}
