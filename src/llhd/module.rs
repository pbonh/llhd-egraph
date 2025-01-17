use std::ops::{Deref, DerefMut};

use llhd::ir::Module;

#[derive(Debug, Clone, Default)]
pub struct LLHDModule(Module);

impl Deref for LLHDModule {
    type Target = Module;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for LLHDModule {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<Module> AsRef<Module> for LLHDModule
where
    Module: ?Sized,
    <Self as Deref>::Target: AsRef<Module>,
{
    fn as_ref(&self) -> &Module {
        self.deref().as_ref()
    }
}

impl<Module> AsMut<Module> for LLHDModule
where
    <Self as Deref>::Target: AsMut<Module>,
{
    fn as_mut(&mut self) -> &mut Module {
        self.deref_mut().as_mut()
    }
}

impl From<Module> for LLHDModule {
    fn from(module: Module) -> Self {
        Self(module)
    }
}

impl From<LLHDModule> for Module {
    fn from(llhd_module: LLHDModule) -> Self {
        llhd_module.0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_module_creation_via_default() {
        let _ = LLHDModule::default();
    }
}
