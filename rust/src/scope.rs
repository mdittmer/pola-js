use std::collections::hash_map::Entry;
use std::fmt::Debug;

use crate::parse::Module;
use crate::parse::SyntaxMetadata;
use color_eyre::Result;
use std::hash::Hash;
use swc_common::collections::AHashMap;
use swc_common::collections::AHashSet;
use swc_common::source_map as sm;
use swc_common::Mark;
use swc_common::SyntaxContext;
use swc_ecma_ast as ast;
use swc_ecma_visit::Visit;
use swc_ecma_visit::VisitWith;

pub trait Span: Clone + Copy + Debug + Hash {}

impl<S: Clone + Copy + Debug + Hash> Span for S {}

#[derive(Debug)]
pub struct ScopeAnalysis {
    pub syntax_metadata: SyntaxMetadata,
    pub scopes_and_variables: ScopesAndVariables<sm::Span>,
}

#[tracing::instrument(skip(module))]
pub fn analyze_scopes(module: &Module) -> Result<ScopeAnalysis> {
    let mut state = ScopeAnalysisState::new(module.syntax_metadata.clone());
    module.swc_module.visit_with(&mut state);

    state.into()
}

fn ast_expr_span(expr: &ast::Expr) -> sm::Span {
    match expr {
        ast::Expr::Array(e) => e.span,
        ast::Expr::Arrow(e) => e.span,
        ast::Expr::Assign(e) => e.span,
        ast::Expr::Await(e) => e.span,
        ast::Expr::Bin(e) => e.span,
        ast::Expr::Call(e) => e.span,
        ast::Expr::Class(e) => e.class.span,
        ast::Expr::Cond(e) => e.span,
        ast::Expr::Fn(e) => e.function.span,
        ast::Expr::Ident(e) => e.span,
        ast::Expr::Invalid(e) => e.span,
        ast::Expr::JSXElement(e) => e.span,
        ast::Expr::JSXEmpty(e) => e.span,
        ast::Expr::JSXFragment(e) => e.span,
        ast::Expr::JSXMember(_e) => {
            todo!("JSXMember syntax does not have a representative Span")
        }
        ast::Expr::JSXNamespacedName(_e) => {
            todo!("JSXNamespacedName syntax does not have a representative Span")
        }
        ast::Expr::Lit(ast::Lit::BigInt(e)) => e.span,
        ast::Expr::Lit(ast::Lit::Bool(e)) => e.span,
        ast::Expr::Lit(ast::Lit::JSXText(e)) => e.span,
        ast::Expr::Lit(ast::Lit::Null(e)) => e.span,
        ast::Expr::Lit(ast::Lit::Num(e)) => e.span,
        ast::Expr::Lit(ast::Lit::Regex(e)) => e.span,
        ast::Expr::Lit(ast::Lit::Str(e)) => e.span,
        ast::Expr::Member(e) => e.span,
        ast::Expr::MetaProp(e) => e.span,
        ast::Expr::New(e) => e.span,
        ast::Expr::Object(e) => e.span,
        ast::Expr::OptChain(e) => e.span,
        ast::Expr::Paren(e) => e.span,
        ast::Expr::PrivateName(e) => e.span,
        ast::Expr::Seq(e) => e.span,
        ast::Expr::SuperProp(e) => e.span,
        ast::Expr::TaggedTpl(e) => e.span,
        ast::Expr::This(e) => e.span,
        ast::Expr::Tpl(e) => e.span,
        ast::Expr::TsAs(e) => e.span,
        ast::Expr::TsConstAssertion(e) => e.span,
        ast::Expr::TsInstantiation(e) => e.span,
        ast::Expr::TsNonNull(e) => e.span,
        ast::Expr::TsSatisfies(e) => e.span,
        ast::Expr::TsTypeAssertion(e) => e.span,
        ast::Expr::Unary(e) => e.span,
        ast::Expr::Update(e) => e.span,
        ast::Expr::Yield(e) => e.span,
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum BlockStmtOrExpr<S: Span> {
    BlockStmt(S),
    Expr(S),
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Scope<S: Span> {
    External,
    Module(S),
    ArrowFunction(BlockStmtOrExpr<S>),
    Function { function: S, block: Option<S> },
    Block(S),
    For { for_stmt: S, block: Option<S> },
    With { with: S, block: Option<S> },
}

impl From<&ast::Module> for Scope<sm::Span> {
    fn from(module: &ast::Module) -> Self {
        Self::Module(module.span)
    }
}

impl From<&ast::ArrowExpr> for Scope<sm::Span> {
    fn from(arrow_function: &ast::ArrowExpr) -> Self {
        Self::ArrowFunction(match &arrow_function.body {
            ast::BlockStmtOrExpr::BlockStmt(block_stmt) => {
                BlockStmtOrExpr::BlockStmt(block_stmt.span)
            }
            ast::BlockStmtOrExpr::Expr(expr) => BlockStmtOrExpr::Expr(ast_expr_span(&expr)),
        })
    }
}

impl From<&ast::Function> for Scope<sm::Span> {
    fn from(function: &ast::Function) -> Self {
        Self::Function {
            function: function.span,
            block: function.body.as_ref().map(|block| block.span),
        }
    }
}

impl From<&ast::BlockStmt> for Scope<sm::Span> {
    fn from(block: &ast::BlockStmt) -> Self {
        Self::Block(block.span)
    }
}

impl From<&ast::ForStmt> for Scope<sm::Span> {
    fn from(for_stmt: &ast::ForStmt) -> Self {
        match for_stmt.body.as_ref() {
            ast::Stmt::Block(block) => Self::For {
                for_stmt: for_stmt.span,
                block: Some(block.span),
            },
            _ => Self::For {
                for_stmt: for_stmt.span,
                block: None,
            },
        }
    }
}

impl From<&ast::ForInStmt> for Scope<sm::Span> {
    fn from(for_stmt: &ast::ForInStmt) -> Self {
        match for_stmt.body.as_ref() {
            ast::Stmt::Block(block) => Self::For {
                for_stmt: for_stmt.span,
                block: Some(block.span),
            },
            _ => Self::For {
                for_stmt: for_stmt.span,
                block: None,
            },
        }
    }
}

impl From<&ast::ForOfStmt> for Scope<sm::Span> {
    fn from(for_stmt: &ast::ForOfStmt) -> Self {
        match for_stmt.body.as_ref() {
            ast::Stmt::Block(block) => Self::For {
                for_stmt: for_stmt.span,
                block: Some(block.span),
            },
            _ => Self::For {
                for_stmt: for_stmt.span,
                block: None,
            },
        }
    }
}

impl From<&ast::WithStmt> for Scope<sm::Span> {
    fn from(with: &ast::WithStmt) -> Self {
        match with.body.as_ref() {
            ast::Stmt::Block(block) => Self::With {
                with: with.span,
                block: Some(block.span),
            },
            _ => Self::With {
                with: with.span,
                block: None,
            },
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct NestedScope<S: Span> {
    pub scope: Scope<S>,
    pub inner: Vec<NestedScope<S>>,
}

impl<S: Span> NestedScope<S> {
    fn new(scope: Scope<S>) -> Self {
        Self {
            scope,
            inner: vec![],
        }
    }
}

#[derive(Debug)]
struct CommonAncestorScopes<'a> {
    common_ancestors: Vec<&'a Scope<sm::Span>>,
    uncommon_ancestors: (Vec<&'a Scope<sm::Span>>, Vec<&'a Scope<sm::Span>>),
}

#[derive(Debug)]
struct ScopeBuilder {
    top_scope: NestedScope<sm::Span>,
    stack: Vec<usize>,
}

impl ScopeBuilder {
    fn new() -> Self {
        Self {
            top_scope: NestedScope::new(Scope::External),
            stack: vec![],
        }
    }

    fn push<'a, N>(&mut self, node: &'a N)
    where
        Scope<sm::Span>: From<&'a N>,
    {
        let mut scope = &mut self.top_scope;
        for idx in self.stack.iter() {
            scope = scope.inner.get_mut(*idx).expect("indexed scope");
        }
        let idx = scope.inner.len();
        scope.inner.push(NestedScope::new(Scope::from(node)));
        self.stack.push(idx);
    }

    fn pop(&mut self) {
        self.stack
            .pop()
            .expect("scope builder stack contains a scope index");
    }

    fn current(&self) -> &NestedScope<sm::Span> {
        let mut scope = &self.top_scope;
        for idx in self.stack.iter() {
            scope = &scope.inner[*idx];
        }
        scope
    }

    fn common_ancestors<'a>(
        &'a self,
        scope1: &Scope<sm::Span>,
        scope2: &Scope<sm::Span>,
    ) -> CommonAncestorScopes<'a> {
        let mut ancestors1 = self.ancestors_reversed(scope1);
        let mut ancestors2 = self.ancestors_reversed(scope2);

        let mut common_ancestors = vec![];
        let mut uncommon_ancestors = (vec![], vec![]);
        loop {
            match (ancestors1.pop(), ancestors2.pop()) {
                (Some(ancestor1), Some(ancestor2)) => {
                    if ancestor1 != ancestor2 {
                        uncommon_ancestors.0.push(ancestor1);
                        uncommon_ancestors.1.push(ancestor2);
                    } else {
                        common_ancestors.push(ancestor1);
                    }
                }
                (Some(ancestor1), None) => {
                    uncommon_ancestors.0.push(ancestor1);
                }
                (None, Some(ancestor2)) => {
                    uncommon_ancestors.1.push(ancestor2);
                }
                (None, None) => {
                    break;
                }
            }
        }

        CommonAncestorScopes {
            common_ancestors,
            uncommon_ancestors,
        }
    }

    fn ancestors<'a>(&'a self, scope: &Scope<sm::Span>) -> Vec<&'a Scope<sm::Span>> {
        let mut ancestors_reversed = self.ancestors_reversed(scope);
        ancestors_reversed.reverse();
        ancestors_reversed
    }

    fn ancestors_reversed<'a>(&'a self, scope: &Scope<sm::Span>) -> Vec<&'a Scope<sm::Span>> {
        match self.ancestors_recursive(scope, &self.top_scope) {
            Some(scopes) => scopes,
            None => vec![],
        }
    }

    fn ancestors_recursive<'a>(
        &'a self,
        scope: &Scope<sm::Span>,
        nested_scope: &'a NestedScope<sm::Span>,
    ) -> Option<Vec<&'a Scope<sm::Span>>> {
        if &nested_scope.scope == scope {
            return Some(vec![&nested_scope.scope]);
        }

        for inner_scope in nested_scope.inner.iter() {
            match self.ancestors_recursive(&scope, inner_scope) {
                Some(mut scopes) => {
                    scopes.push(&nested_scope.scope);
                    return Some(scopes);
                }
                None => {}
            }
        }

        None
    }

    fn build(self) -> NestedScope<sm::Span> {
        assert!(self.stack.len() == 0);
        self.top_scope
    }
}

#[derive(Debug)]
pub struct ScopesAndVariables<S: Span> {
    pub top_scope: NestedScope<S>,
    pub lexical_scopes: AHashMap<ast::Id, Scope<S>>,
    pub dynamic_scopes: AHashMap<ast::Id, Vec<Scope<S>>>,
    pub closure_usages: AHashMap<Scope<S>, AHashSet<ast::Id>>,
}

#[derive(Debug)]
struct VariableBinder {
    unresolved_mark: Mark,
    external_scope: Scope<sm::Span>,
    scope_builder: ScopeBuilder,
    lexical_scopes: AHashMap<ast::Id, Scope<sm::Span>>,
    dynamic_scopes: AHashMap<ast::Id, Vec<Scope<sm::Span>>>,
    closure_usages: AHashMap<Scope<sm::Span>, AHashSet<ast::Id>>,
}

impl VariableBinder {
    fn new(unresolved_mark: Mark) -> Self {
        let scope_builder = ScopeBuilder::new();
        Self {
            unresolved_mark,
            external_scope: scope_builder.top_scope.scope.clone(),
            scope_builder,
            lexical_scopes: Default::default(),
            dynamic_scopes: Default::default(),
            closure_usages: Default::default(),
        }
    }

    fn visit_ident(&mut self, n: &ast::Ident) {
        let (atom, syntax_context) = n.to_id();

        // Contextless identifiers do not contain unqualified variable references.
        // E.g., in `function a(b) { c.d; }` `a`, `b`, and `c` have a non-empty context, but `d`
        // does not.
        if syntax_context != SyntaxContext::empty() {
            let id = (atom, syntax_context);
            let current_scope = { self.scope_builder.current().scope.clone() };
            let (lexical_scope, dynamic_scopes) = if syntax_context.has_mark(self.unresolved_mark) {
                // Unbound varibles are marked with `unresolved_mark`. Use module span to identify
                // lexical scope, and consider all of current scopes ancestors (including itself)
                // as candidates for dynamic scopes.
                (
                    self.external_scope.clone(),
                    self.scope_builder.ancestors(&current_scope),
                )
            } else {
                // Variable is bound in some lexical scope in this module.
                //
                // Note: Use blocks to bound scope of borrows, and avoid multiple incompatible
                // borrows.
                if let Some(previous_scope) = { self.lexical_scopes.get(&id).map(Clone::clone) } {
                    // Variable was used in an already visited scope. Redefine its lexical scope
                    // as the nearest common ancestor between the current scope and previously
                    // bound lexical scope.
                    let (new_scope, uncommon_current_scope_ancestors) = {
                        let CommonAncestorScopes {
                            mut common_ancestors,
                            uncommon_ancestors: (uncommon_current_scope_ancestors, _),
                        } = self
                            .scope_builder
                            .common_ancestors(&current_scope, &previous_scope);

                        (
                            common_ancestors
                                .pop()
                                .expect("common ancestor betwen scopes in same module")
                                .clone(),
                            uncommon_current_scope_ancestors,
                        )
                    };

                    // Consider all scopes between (but not including) lexical scope and
                    // (including) current scope as candidates for dynamic scopes.
                    (new_scope, uncommon_current_scope_ancestors)
                } else {
                    // Variable is not already bound to a lexical scope. Bind it to the current
                    // scope. Consider all of current scope's ancestors (including itself) as
                    // candidates for dynamic scopes.
                    let ancestors = self.scope_builder.ancestors(&current_scope);
                    (current_scope.clone(), ancestors)
                }
            };

            let dynamic_scopes = dynamic_scopes
                .into_iter()
                .filter_map(|scope| match scope {
                    // Filter candidate dynamic scopes, retaining only `with (expr) { ... }` scopes.
                    with_scope @ Scope::With { .. } => Some(with_scope.clone()),
                    _ => None,
                })
                .collect::<Vec<_>>();

            self.lexical_scopes
                .insert(id.clone(), lexical_scope.clone());

            if dynamic_scopes.len() > 0 {
                match self.dynamic_scopes.entry(id.clone()) {
                    Entry::Vacant(entry) => {
                        entry.insert(dynamic_scopes.clone());
                    }
                    Entry::Occupied(mut entry) => {
                        entry.get_mut().extend(dynamic_scopes.clone().into_iter());
                    }
                }
            }

            let mut scopes = dynamic_scopes.iter().collect::<Vec<_>>();
            scopes.push(&lexical_scope);
            for scope in scopes.into_iter() {
                if scope != &current_scope {
                    match self.closure_usages.entry(current_scope.clone()) {
                        Entry::Vacant(entry) => {
                            let closure_usages: AHashSet<ast::Id> =
                                AHashSet::<ast::Id>::from_iter([id.clone()].into_iter());
                            entry.insert(closure_usages);
                        }
                        Entry::Occupied(mut entry) => {
                            entry.get_mut().insert(id.clone());
                        }
                    }
                }
            }
        }
    }

    fn build(self) -> ScopesAndVariables<sm::Span> {
        ScopesAndVariables {
            top_scope: self.scope_builder.build(),
            lexical_scopes: self.lexical_scopes,
            dynamic_scopes: self.dynamic_scopes,
            closure_usages: self.closure_usages,
        }
    }
}

#[derive(Debug)]
pub struct ScopeAnalysisState {
    syntax_metadata: SyntaxMetadata,
    variable_binder: VariableBinder,
}

impl ScopeAnalysisState {
    pub fn new(syntax_metadata: SyntaxMetadata) -> Self {
        Self {
            variable_binder: VariableBinder::new(syntax_metadata.unresolved_mark.clone()),
            syntax_metadata,
        }
    }

    fn variable_binder(&mut self) -> &mut VariableBinder {
        &mut self.variable_binder
    }

    fn scope_builder(&mut self) -> &mut ScopeBuilder {
        &mut self.variable_binder().scope_builder
    }
}

impl From<ScopeAnalysisState> for Result<ScopeAnalysis> {
    fn from(state: ScopeAnalysisState) -> Self {
        let scopes_and_variables = state.variable_binder.build();
        Ok(ScopeAnalysis {
            syntax_metadata: state.syntax_metadata,
            scopes_and_variables,
        })
    }
}

impl Visit for ScopeAnalysisState {
    fn visit_arrow_expr(&mut self, n: &ast::ArrowExpr) {
        self.scope_builder().push(n);
        n.visit_children_with(self);
        self.scope_builder().pop();
    }

    fn visit_block_stmt(&mut self, n: &ast::BlockStmt) {
        match self.scope_builder().current().scope {
            Scope::ArrowFunction(BlockStmtOrExpr::BlockStmt(block_stmt)) => {
                if n.span == block_stmt {
                    // No extra block scope for `(...) => { ... }`, just ArrowFunction scope.
                    n.visit_children_with(self)
                } else {
                    self.scope_builder().push(n);
                    n.visit_children_with(self);
                    self.scope_builder().pop();
                }
            }
            Scope::For {
                block: Some(block), ..
            } => {
                if n.span == block {
                    // No extra block scope for `for (...) { ... }`, just For scope.
                    n.visit_children_with(self);
                } else {
                    self.scope_builder().push(n);
                    n.visit_children_with(self);
                    self.scope_builder().pop();
                }
            }
            Scope::Function {
                block: Some(block), ..
            } => {
                if n.span == block {
                    // No extra block scope for `function (...) { ... }`, just Function scope.
                    n.visit_children_with(self);
                } else {
                    self.scope_builder().push(n);
                    n.visit_children_with(self);
                    self.scope_builder().pop();
                }
            }
            Scope::With {
                block: Some(block), ..
            } => {
                if n.span == block {
                    // No extra block scope for `with (...) { ... }`, just With scope.
                    n.visit_children_with(self);
                } else {
                    self.scope_builder().push(n);
                    n.visit_children_with(self);
                    self.scope_builder().pop();
                }
            }
            _ => {
                self.scope_builder().push(n);
                n.visit_children_with(self);
                self.scope_builder().pop();
            }
        }
    }

    fn visit_for_in_stmt(&mut self, n: &ast::ForInStmt) {
        self.scope_builder().push(n);
        n.visit_children_with(self);
        self.scope_builder().pop();
    }

    fn visit_for_of_stmt(&mut self, n: &ast::ForOfStmt) {
        self.scope_builder().push(n);
        n.visit_children_with(self);
        self.scope_builder().pop();
    }

    fn visit_for_stmt(&mut self, n: &ast::ForStmt) {
        self.scope_builder().push(n);
        n.visit_children_with(self);
        self.scope_builder().pop();
    }

    fn visit_function(&mut self, n: &ast::Function) {
        println!("function:  {:#?}", n);
        self.scope_builder().push(n);
        n.visit_children_with(self);
        self.scope_builder().pop();
    }

    fn visit_ident(&mut self, n: &ast::Ident) {
        self.variable_binder.visit_ident(n);
        n.visit_children_with(self)
    }

    fn visit_module(&mut self, n: &ast::Module) {
        self.variable_binder.scope_builder.push(n);
        n.visit_children_with(self);
        self.variable_binder.scope_builder.pop();
    }

    fn visit_with_stmt(&mut self, n: &ast::WithStmt) {
        n.obj.visit_with(self);
        self.scope_builder().push(n);
        n.body.visit_with(self);
        self.scope_builder().pop();
    }
}

#[cfg(test)]
pub mod test {
    use super::BlockStmtOrExpr;
    use super::Scope;
    use super::Span;

    impl<S: Span> Scope<S> {
        pub fn strip(&self) -> Scope<()> {
            match self {
                Self::ArrowFunction(BlockStmtOrExpr::BlockStmt(_)) => {
                    Scope::<()>::ArrowFunction(BlockStmtOrExpr::BlockStmt(()))
                }
                Self::ArrowFunction(BlockStmtOrExpr::Expr(_)) => {
                    Scope::<()>::ArrowFunction(BlockStmtOrExpr::Expr(()))
                }
                Self::Block(_) => Scope::<()>::Block(()),
                Self::External => Scope::<()>::External,
                Self::For { block, .. } => Scope::<()>::For {
                    for_stmt: (),
                    block: block.map(|_| ()),
                },
                Self::Function { block, .. } => Scope::<()>::Function {
                    function: (),
                    block: block.map(|_| ()),
                },
                Self::Module(_) => Scope::<()>::Module(()),
                Self::With { block, .. } => Scope::<()>::With {
                    with: (),
                    block: block.map(|_| ()),
                },
            }
        }

        pub fn assert_external(&self) {
            match self {
                Self::External => {}
                _ => {
                    panic!("expected external scope");
                }
            }
        }

        pub fn assert_module(&self) {
            match self {
                Self::Module(_) => {}
                _ => {
                    panic!("expected module scope");
                }
            }
        }

        pub fn assert_function(&self) {
            match self {
                Self::Function { .. } => {}
                _ => {
                    panic!("expected function scope");
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::hash_map::Entry;

    use super::analyze_scopes;
    use super::BlockStmtOrExpr;
    use super::Scope;
    use super::ScopeAnalysis;
    use crate::parse::parse_str;
    use color_eyre::Result;
    use swc_common::collections::AHashMap;
    use swc_common::collections::AHashSet;
    use swc_common::source_map as sm;
    use swc_common::GLOBALS;
    use swc_ecma_ast as ast;
    use swc_ecma_parser::Syntax;
    use swc_ecma_visit::Visit;
    use swc_ecma_visit::VisitWith;

    #[derive(Default)]
    struct IdCounter {
        counts: AHashMap<ast::Id, u32>,
    }

    impl Visit for IdCounter {
        fn visit_ident(&mut self, n: &ast::Ident) {
            let id = n.to_id();
            match self.counts.entry(id) {
                Entry::Vacant(entry) => {
                    entry.insert(1);
                }
                Entry::Occupied(mut entry) => {
                    let count = { entry.get() } + 1;
                    entry.insert(count);
                }
            }
        }
    }

    fn count_ids(swc_module: &ast::Module) -> IdCounter {
        let mut id_counter = IdCounter::default();
        swc_module.visit_with(&mut id_counter);
        id_counter
    }

    fn noop<'a, I>(_: &'a I) {}

    fn analyze_source_str(source_str: &str) -> Result<ScopeAnalysis> {
        Ok(analyze_source_str_with(source_str, noop)?.1)
    }

    fn analyze_source_str_with<F, R>(source_str: &str, f: F) -> Result<(R, ScopeAnalysis)>
    where
        F: for<'a> FnOnce(&'a ast::Module) -> R,
    {
        GLOBALS.set(&Default::default(), || -> Result<(R, ScopeAnalysis)> {
            let module = parse_str(
                source_str,
                "test.ts",
                Syntax::Typescript(Default::default()),
                Default::default(),
            )?;

            Ok((f(&module.swc_module), analyze_scopes(&module)?))
        })
    }

    fn id_count_test<F>(source_str: &str, expected_total_count: u32, get_expected_count: F)
    where
        F: Fn(&ast::Id, &Scope<sm::Span>) -> u32,
    {
        let (id_counts, module_analysis) =
            analyze_source_str_with(source_str, count_ids).expect("module analysis");
        assert_eq!(0, module_analysis.scopes_and_variables.dynamic_scopes.len());
        let variables_and_scopes = &module_analysis.scopes_and_variables.lexical_scopes;

        let mut actual_total_count = 0;
        for (id, scope) in variables_and_scopes.into_iter() {
            let actual_count = *id_counts.counts.get(id).expect("id");
            let expected_count = get_expected_count(id, scope);
            assert_eq!(expected_count, actual_count);
            actual_total_count += actual_count;
        }
        assert_eq!(expected_total_count, actual_total_count);
    }

    #[test]
    fn test_shadow() {
        id_count_test(
            "const a = null; (function(a) { a; })();",
            3,
            |_: &ast::Id, scope: &Scope<sm::Span>| -> u32 {
                match scope {
                    Scope::Module(_) => 1,
                    Scope::Function { .. } => 2,
                    scope => {
                        panic!("unexpected scope in variables_and_scopes: {:?}", scope);
                    }
                }
            },
        );

        id_count_test(
            "const a = null; (a => a)();",
            3,
            |_: &ast::Id, scope: &Scope<sm::Span>| -> u32 {
                match scope {
                    Scope::Module(_) => 1,
                    Scope::ArrowFunction { .. } => 2,
                    scope => {
                        panic!("unexpected scope in variables_and_scopes: {:?}", scope);
                    }
                }
            },
        );

        id_count_test(
            "const a = null; { let a; a; }",
            3,
            |_: &ast::Id, scope: &Scope<sm::Span>| -> u32 {
                match scope {
                    Scope::Module(_) => 1,
                    Scope::Block { .. } => 2,
                    scope => {
                        panic!("unexpected scope in variables_and_scopes: {:?}", scope);
                    }
                }
            },
        );

        id_count_test(
            "const a = null; for (let a = 0; a < 2; a++) { a; }",
            5,
            |_: &ast::Id, scope: &Scope<sm::Span>| -> u32 {
                match scope {
                    Scope::Module(_) => 1,
                    Scope::For { .. } => 4,
                    scope => {
                        panic!("unexpected scope in variables_and_scopes: {:?}", scope);
                    }
                }
            },
        );
    }

    #[test]
    fn test_unbound() {
        let source_str = "a;";
        let module_analysis = analyze_source_str(source_str).expect("module analysis");
        let top_scope = &module_analysis.scopes_and_variables.top_scope;
        top_scope.scope.assert_external();
        assert_eq!(1, top_scope.inner.len());
        let module_scope = top_scope.inner.iter().next().expect("module scope");
        module_scope.scope.assert_module();
        assert_eq!(0, module_scope.inner.len());
        let scopes_and_variables = &module_analysis.scopes_and_variables;
        assert_eq!(1, scopes_and_variables.lexical_scopes.len());
        assert_eq!(0, scopes_and_variables.dynamic_scopes.len());
        let ((a, _), a_scope) = scopes_and_variables
            .lexical_scopes
            .iter()
            .next()
            .expect("variable and scope");
        assert_eq!(&*a, "a");
        assert_eq!(a_scope, &top_scope.scope);
    }

    #[test]
    fn test_hoisted_var() {
        let source_str = "(function() { a; })(); var a;";
        let module_analysis = analyze_source_str(source_str).expect("module analysis");
        let top_scope = &module_analysis.scopes_and_variables.top_scope;
        top_scope.scope.assert_external();
        assert_eq!(1, top_scope.inner.len());
        let module_scope = top_scope.inner.iter().next().expect("module scope");
        module_scope.scope.assert_module();
        assert_eq!(1, module_scope.inner.len());
        let function_scope = module_scope.inner.iter().next().expect("function scope");
        function_scope.scope.assert_function();
        assert_eq!(0, function_scope.inner.len());
        let scopes_and_variables = &module_analysis.scopes_and_variables;
        assert_eq!(1, scopes_and_variables.lexical_scopes.len());
        assert_eq!(0, scopes_and_variables.dynamic_scopes.len());
        let ((a, _), a_scope) = scopes_and_variables
            .lexical_scopes
            .iter()
            .next()
            .expect("variable and scope");
        assert_eq!(&*a, "a");
        assert_eq!(a_scope, &module_scope.scope);
    }

    #[test]
    fn test_hoisted_function() {
        let source_str = "(function() { a; })(); function a () {}";
        let module_analysis = analyze_source_str(source_str).expect("module analysis");
        let top_scope = &module_analysis.scopes_and_variables.top_scope;
        top_scope.scope.assert_external();
        assert_eq!(1, top_scope.inner.len());
        let module_scope = top_scope.inner.iter().next().expect("module scope");
        module_scope.scope.assert_module();
        assert_eq!(2, module_scope.inner.len());
        let mut functions_iter = module_scope.inner.iter();
        let function_scope = functions_iter.next().expect("function scope");
        function_scope.scope.assert_function();
        assert_eq!(0, function_scope.inner.len());
        let function_scope = functions_iter.next().expect("function scope");
        function_scope.scope.assert_function();
        assert_eq!(0, function_scope.inner.len());
        let scopes_and_variables = &module_analysis.scopes_and_variables;
        assert_eq!(1, scopes_and_variables.lexical_scopes.len());
        assert_eq!(0, scopes_and_variables.dynamic_scopes.len());
        let ((a, _), a_scope) = scopes_and_variables
            .lexical_scopes
            .iter()
            .next()
            .expect("variable and scope");
        assert_eq!(&*a, "a");
        assert_eq!(a_scope, &module_scope.scope);
    }

    #[test]
    fn test_mutli_with1() {
        let source_str = r#"
        (function() {
            with (_) {
                (() => {
                    {
                        with (_) {
                            (function() {
                                {
                                    a;
                                }
                            })();
                        }
                    }
                })();
            }

            var a;

            with (_) {
                (function() {
                    {
                        (() => a)();
                    }
                })();
            }
        })();
        "#;
        let module_analysis = analyze_source_str(source_str).expect("module analysis");
        let a_lexical_scope = &module_analysis
            .scopes_and_variables
            .top_scope // External
            .inner[0] // Module
            .inner[0]; // function() { ... var a; ... }
        let with1 = &a_lexical_scope.inner[0];
        let with2 = &with1.inner[0] // () => { ... }
            .inner[0] // { ... } between () => { ... } and second with (_) { ... }
            .inner[0];
        let with3 = &a_lexical_scope.inner[1];
        let scopes_and_variables = &module_analysis.scopes_and_variables;
        assert_eq!(2, scopes_and_variables.lexical_scopes.len()); // `a` and unresolved `_`.
        assert_eq!(2, scopes_and_variables.dynamic_scopes.len()); // `a` and second `_`.

        for ((atom, _), lexical_scope) in scopes_and_variables.lexical_scopes.iter() {
            if &*atom == "_" {
                lexical_scope.assert_external();
            } else if &*atom == "a" {
                assert_eq!(&a_lexical_scope.scope, lexical_scope);
            } else {
                panic!("unexpected name");
            }
        }

        for ((atom, _), dynamic_scopes) in scopes_and_variables.dynamic_scopes.iter() {
            if &*atom == "_" {
                assert_eq!(&vec![with1.scope.clone()], dynamic_scopes);
            } else if &*atom == "a" {
                assert_eq!(
                    &vec![
                        with1.scope.clone(),
                        with2.scope.clone(),
                        with3.scope.clone(),
                    ],
                    dynamic_scopes
                );
            } else {
                panic!("unexpected name");
            }
        }
    }

    #[test]
    fn test_mutli_with2() {
        let source_str = r#"
        (function() {
            with (_) {
                (function() {
                    {
                        (() => a)();
                    }
                })();
            }

            var a;

            with (_) {
                (() => {
                    {
                        with (_) {
                            (function() {
                                {
                                    a;
                                }
                            })();
                        }
                    }
                })();
            }
        })();
        "#;
        let module_analysis = analyze_source_str(source_str).expect("module analysis");
        let a_lexical_scope = &module_analysis
            .scopes_and_variables
            .top_scope // External
            .inner[0] // Module
            .inner[0]; // function() { ... var a; ... }
        let with1 = &a_lexical_scope.inner[0];
        let with2 = &a_lexical_scope.inner[1];
        let with3 = &with2.inner[0] // () => { ... }
            .inner[0] // { ... } between () => { ... } and second with (_) { ... }
            .inner[0];
        let scopes_and_variables = &module_analysis.scopes_and_variables;
        assert_eq!(2, scopes_and_variables.lexical_scopes.len()); // `a` and unresolved `_`.
        assert_eq!(2, scopes_and_variables.dynamic_scopes.len()); // `a` and second `_`.

        for ((atom, _), lexical_scope) in scopes_and_variables.lexical_scopes.iter() {
            if &*atom == "_" {
                lexical_scope.assert_external();
            } else if &*atom == "a" {
                assert_eq!(&a_lexical_scope.scope, lexical_scope);
            } else {
                panic!("unexpected name");
            }
        }

        for ((atom, _), dynamic_scopes) in scopes_and_variables.dynamic_scopes.iter() {
            if &*atom == "_" {
                assert_eq!(&vec![with2.scope.clone()], dynamic_scopes);
            } else if &*atom == "a" {
                assert_eq!(
                    &vec![
                        with1.scope.clone(),
                        with2.scope.clone(),
                        with3.scope.clone(),
                    ],
                    dynamic_scopes
                );
            } else {
                panic!("unexpected name");
            }
        }
    }

    #[test]
    fn test_mutli_with3() {
        let source_str = r#"
        (function() {
            with (_) {
                (function() {
                    {
                        (() => a)();
                    }
                })();
            }

            with (_) {
                (() => {
                    {
                        with (_) {
                            (function() {
                                {
                                    a;
                                }
                            })();
                        }
                    }
                })();
            }
        })();
        "#;
        let module_analysis = analyze_source_str(source_str).expect("module analysis");
        let outermost_function_scope = &module_analysis
            .scopes_and_variables
            .top_scope // External
            .inner[0] // Module
            .inner[0]; // outermost function() { ... }
        let with1 = &outermost_function_scope.inner[0]; // first with (_) { ... }
        let with2 = &outermost_function_scope.inner[1]; // second with (_) { ... }
        let with3 = &with2.inner[0] // () => { ... }
            .inner[0] // { ... } between () => { ... } and second with (_) { ... }
            .inner[0];
        let scopes_and_variables = &module_analysis.scopes_and_variables;
        assert_eq!(2, scopes_and_variables.lexical_scopes.len()); // `a` and unresolved `_`.
        assert_eq!(2, scopes_and_variables.dynamic_scopes.len()); // `a` and second `_`.

        for ((atom, _), lexical_scope) in scopes_and_variables.lexical_scopes.iter() {
            if &*atom == "_" {
                lexical_scope.assert_external();
            } else if &*atom == "a" {
                lexical_scope.assert_external();
            } else {
                panic!("unexpected name");
            }
        }

        for ((atom, _), dynamic_scopes) in scopes_and_variables.dynamic_scopes.iter() {
            if &*atom == "_" {
                assert_eq!(&vec![with2.scope.clone()], dynamic_scopes);
            } else if &*atom == "a" {
                assert_eq!(
                    &vec![
                        with1.scope.clone(),
                        with2.scope.clone(),
                        with3.scope.clone(),
                    ],
                    dynamic_scopes
                );
            } else {
                panic!("unexpected name");
            }
        }
    }

    fn a_in_closure_usages_test(source_str: &str, scope: Scope<()>) {
        let actual_closure_usages = analyze_source_str(source_str)
            .expect("module analysis")
            .scopes_and_variables
            .closure_usages
            .into_iter()
            .map(|(scope, ids)| {
                (
                    scope.strip(),
                    ids.into_iter()
                        .map(|(atom, _syntax_context)| atom.to_string())
                        .collect::<AHashSet<_>>(),
                )
            })
            .collect::<AHashMap<_, _>>();
        let mut expected_closure_usages: AHashMap<Scope<()>, AHashSet<String>> = Default::default();
        let mut expected_usages: AHashSet<String> = Default::default();
        expected_usages.insert("a".to_string());
        expected_closure_usages.insert(scope, expected_usages);
        assert_eq!(expected_closure_usages, actual_closure_usages);
    }

    fn none_closure_usages_test(source_str: &str) {
        let actual_closure_usages = analyze_source_str(source_str)
            .expect("module analysis")
            .scopes_and_variables
            .closure_usages
            .into_iter()
            .map(|(scope, ids)| {
                (
                    scope.strip(),
                    ids.into_iter()
                        .map(|(atom, _syntax_context)| atom.to_string())
                        .collect::<AHashSet<_>>(),
                )
            })
            .collect::<AHashMap<_, _>>();
        let expected_closure_usages: AHashMap<Scope<()>, AHashSet<String>> = Default::default();
        assert_eq!(expected_closure_usages, actual_closure_usages);
    }

    #[test]
    fn test_closure_usage() {
        type S = Scope<()>;

        a_in_closure_usages_test(
            "() => { a; }",
            S::ArrowFunction(BlockStmtOrExpr::BlockStmt(())),
        );
        none_closure_usages_test("(a) => { a; }");
        a_in_closure_usages_test("() => a", S::ArrowFunction(BlockStmtOrExpr::Expr(())));
        none_closure_usages_test("(a) => a");
        a_in_closure_usages_test("{ a; }", S::Block(()));
        a_in_closure_usages_test(
            "for (let i = 0; i < 42; i++) { a; }",
            S::For {
                for_stmt: (),
                block: Some(()),
            },
        );
        a_in_closure_usages_test(
            "for (let i = 0; i < 42; i++) a;",
            S::For {
                for_stmt: (),
                block: None,
            },
        );
        a_in_closure_usages_test(
            "function x(p, q, r) { a; }",
            S::Function {
                function: (),
                block: Some(()),
            },
        );
        a_in_closure_usages_test("a;", S::Module(()));
        a_in_closure_usages_test(
            "with ({}) { a; }",
            S::With {
                with: (),
                block: Some(()),
            },
        );
        a_in_closure_usages_test(
            "with ({}) a;",
            S::With {
                with: (),
                block: None,
            },
        );
    }
}
