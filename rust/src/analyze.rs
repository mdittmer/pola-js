use std::fmt::Debug;

use crate::parse::Module;
use crate::scope::ScopeAnalysis;
use crate::scope::ScopeAnalysisState;
use color_eyre::Result;
use swc_ecma_visit::VisitWith as _;

#[derive(Debug)]
pub struct ModuleAnalysis {
    pub scope_analysis: ScopeAnalysis,
}

#[tracing::instrument(skip(module))]
pub fn analyze_module(module: &Module) -> Result<ModuleAnalysis> {
    let mut scope_state = ScopeAnalysisState::new(module.syntax_metadata.clone());
    module.swc_module.visit_with(&mut scope_state);

    let scope_analysis_result: Result<ScopeAnalysis> = scope_state.into();
    Ok(ModuleAnalysis {
        scope_analysis: scope_analysis_result?,
    })
}
