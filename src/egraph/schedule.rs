use std::ops::Deref;

use egglog::ast::Command;
use egglog::EGraph;
use itertools::Itertools;

use crate::egraph::EgglogCommandList;

#[derive(Debug, Clone, Default)]
pub struct EgglogSchedules(EgglogCommandList);

impl EgglogSchedules {
    pub fn add_schedule<CommandList>(self, schedule_list: CommandList) -> Self
    where
        CommandList: IntoIterator<Item = Command>,
    {
        let mut schedules = schedule_list
            .into_iter()
            .filter(|command| matches!(*command, Command::RunSchedule(..)))
            .collect_vec();
        let mut updated_schedules = Self(self.0);
        updated_schedules.0.append(&mut schedules);
        updated_schedules
    }

    pub fn add_schedule_str(self, schedule_str: &str) -> Self {
        match EGraph::default().parse_program(None, schedule_str) {
            Ok(schedule_commands) => Self::add_schedule(self, schedule_commands),
            Err(error) => panic!("Failure to build schedule from string: {:?}", error),
        }
    }
}

impl Deref for EgglogSchedules {
    type Target = EgglogCommandList;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<EgglogCommandList> AsRef<EgglogCommandList> for EgglogSchedules
where
    EgglogCommandList: ?Sized,
    <Self as Deref>::Target: AsRef<EgglogCommandList>,
{
    fn as_ref(&self) -> &EgglogCommandList {
        self.deref().as_ref()
    }
}

impl Into<EgglogCommandList> for EgglogSchedules {
    fn into(self) -> EgglogCommandList {
        self.0
    }
}

impl IntoIterator for EgglogSchedules {
    type Item = Command;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

#[cfg(test)]
mod tests {
    use egglog::ast::{GenericCommand, GenericRunConfig, Schedule, Symbol, DUMMY_SPAN};

    use super::*;

    #[test]
    fn create_egglog_schedule_from_cmd() {
        let schedule1 = GenericCommand::RunSchedule(Schedule::Run(
            DUMMY_SPAN.clone(),
            GenericRunConfig {
                ruleset: Symbol::new("schedule1"),
                until: None,
            },
        ));
        let egglog_sorts = EgglogSchedules::default().add_schedule(vec![schedule1]);
        assert_eq!(
            1,
            egglog_sorts.len(),
            "There should be 1 commands present, one for each schedule."
        );
    }
}
