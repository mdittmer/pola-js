use std::fmt::Debug;
use std::path::Path;

use crate::heap::Constant;
use crate::parse::parse_file;
use crate::parse::parse_file_with_handler;
use crate::parse::parse_str;
use crate::parse::parse_str_with_handler;
use crate::parse::Module;
use crate::parse::SyntaxMetadata;
use color_eyre::Result;
use num_bigint::BigInt as BigIntValue;
use swc_atoms::Atom;
use swc_atoms::JsWord;
use swc_common::collections::AHashMap;
use swc_common::collections::AHashSet;
use swc_common::errors::Handler;
use swc_common::source_map::Span;
use swc_common::Mark;
use swc_common::SyntaxContext;
use swc_ecma_ast as ast;
use swc_ecma_ast::Module as SWCModule;
use swc_ecma_parser::Syntax;
use swc_ecma_visit::Visit;
use swc_ecma_visit::VisitWith;

#[derive(Debug)]
pub struct ModuleAnalysis {
    syntax_metadata: SyntaxMetadata,
    scopes_and_variables: ScopesAndVariables,
}

pub fn analyze_file<P: AsRef<Path>>(
    source_path: P,
    syntax: Syntax,
    target: ast::EsVersion,
) -> Result<ModuleAnalysis> {
    let module = parse_file(source_path, syntax, target)?;
    analyze_module(&module)
}

pub fn analyze_file_with_handler<P: AsRef<Path>>(
    source_path: P,
    syntax: Syntax,
    target: ast::EsVersion,
    handler: Handler,
) -> Result<ModuleAnalysis> {
    let module = parse_file_with_handler(source_path, syntax, target, handler)?;
    analyze_module(&module)
}

pub fn analyze_str(
    source_str: &str,
    source_path: &str,
    syntax: Syntax,
    target: ast::EsVersion,
) -> Result<ModuleAnalysis> {
    let module = parse_str(source_str, source_path, syntax, target)?;
    analyze_module(&module)
}

pub fn analyze_str_with_handler(
    source_str: &str,
    source_path: &str,
    syntax: Syntax,
    target: ast::EsVersion,
    handler: Handler,
) -> Result<ModuleAnalysis> {
    let module = parse_str_with_handler(source_str, source_path, syntax, target, handler)?;
    analyze_module(&module)
}

#[tracing::instrument(skip(module))]
fn analyze_module(module: &Module) -> Result<ModuleAnalysis> {
    let mut state = ModuleAnalysisState::new(module.syntax_metadata.clone());
    module.swc_module.visit_with(&mut state);

    state.into()
}

fn expr_span(expr: &ast::Expr) -> Span {
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

#[derive(Debug, Eq, PartialEq)]
enum BlockStmtOrExpr {
    BlockStmt(Span),
    Expr(Span),
}

#[derive(Debug, Eq, PartialEq)]
enum KindedScope {
    Module(Span),
    ArrowFunction(BlockStmtOrExpr),
    Function(Span),
    Block(Span),
    With(Span),
}

impl From<&SWCModule> for KindedScope {
    fn from(module: &SWCModule) -> Self {
        Self::Module(module.span)
    }
}

impl From<&ast::ArrowExpr> for KindedScope {
    fn from(arrow_function: &ast::ArrowExpr) -> Self {
        Self::ArrowFunction(match &arrow_function.body {
            ast::BlockStmtOrExpr::BlockStmt(block_stmt) => {
                BlockStmtOrExpr::BlockStmt(block_stmt.span)
            }
            ast::BlockStmtOrExpr::Expr(expr) => BlockStmtOrExpr::Expr(expr_span(&expr)),
        })
    }
}

impl From<&ast::Function> for KindedScope {
    fn from(function: &ast::Function) -> Self {
        Self::Function(function.span)
    }
}

impl From<&ast::BlockStmt> for KindedScope {
    fn from(block: &ast::BlockStmt) -> Self {
        Self::Block(block.span)
    }
}

impl From<&ast::WithStmt> for KindedScope {
    fn from(with: &ast::WithStmt) -> Self {
        Self::With(with.span)
    }
}

#[derive(Debug, Eq, PartialEq)]
struct NestedScope {
    kinded: KindedScope,
    inner: Vec<NestedScope>,
}

impl NestedScope {
    fn new(kinded: KindedScope) -> Self {
        Self {
            kinded,
            inner: vec![],
        }
    }

    fn span(&self) -> &Span {
        match &self.kinded {
            KindedScope::ArrowFunction(BlockStmtOrExpr::BlockStmt(s)) => s,
            KindedScope::ArrowFunction(BlockStmtOrExpr::Expr(s)) => s,
            KindedScope::Block(s) => s,
            KindedScope::Function(s) => s,
            KindedScope::Module(s) => s,
            KindedScope::With(s) => s,
        }
    }
}

#[derive(Debug)]
struct CommonAncestorScopes<'a> {
    common_ancestors: Vec<&'a NestedScope>,
    uncommon_ancestors: (Vec<&'a NestedScope>, Vec<&'a NestedScope>),
}

#[derive(Debug)]
struct ScopeBuilder {
    top_scope: NestedScope,
    stack: Vec<usize>,
}

impl ScopeBuilder {
    fn new(module: &SWCModule) -> Self {
        let top_scope = NestedScope::new(module.into());
        Self {
            top_scope,
            stack: vec![],
        }
    }

    fn push<'a, N>(&mut self, node: &'a N)
    where
        KindedScope: From<&'a N>,
    {
        let mut scope = &mut self.top_scope;
        for idx in self.stack.iter() {
            scope = scope.inner.get_mut(*idx).expect("indexed scope");
        }
        let idx = scope.inner.len();
        scope.inner.push(NestedScope::new(KindedScope::from(node)));
        self.stack.push(idx);
    }

    fn pop(&mut self) {
        self.stack
            .pop()
            .expect("scope builder stack contains a scope index");
    }

    fn current(&self) -> &NestedScope {
        let mut scope = &self.top_scope;
        for idx in self.stack.iter() {
            scope = &scope.inner[*idx];
        }
        scope
    }

    fn common_ancestors<'a>(&'a self, scope1: &Span, scope2: &Span) -> CommonAncestorScopes<'a> {
        let mut ancestors1 = self.ancestors(scope1);
        let mut ancestors2 = self.ancestors(scope2);

        let mut common_ancestors = vec![];
        let mut uncommon_ancestors = (vec![], vec![]);
        while let (Some(ancestor1), Some(ancestor2)) = (ancestors1.pop(), ancestors2.pop()) {
            if ancestor1 != ancestor2 {
                uncommon_ancestors.0.push(ancestor1);
                uncommon_ancestors.1.push(ancestor2);
            } else {
                common_ancestors.push(ancestor1);
            }
        }

        CommonAncestorScopes {
            common_ancestors,
            uncommon_ancestors,
        }
    }

    fn ancestors<'a>(&'a self, span: &Span) -> Vec<&'a NestedScope> {
        match self.ancestors_recursive(span, &self.top_scope) {
            Some(scopes) => scopes,
            None => vec![],
        }
    }

    fn ancestors_recursive<'a>(
        &'a self,
        span: &Span,
        scope: &'a NestedScope,
    ) -> Option<Vec<&'a NestedScope>> {
        if scope.span() == span {
            return Some(vec![scope]);
        }

        for scope in scope.inner.iter() {
            match self.ancestors_recursive(span, scope) {
                Some(mut spans) => {
                    spans.push(scope);
                    return Some(spans);
                }
                None => {}
            }
        }

        None
    }

    fn build(self) -> NestedScope {
        assert!(self.stack.len() == 0);
        self.top_scope
    }
}

#[derive(Debug)]
struct ScopesAndVariables {
    top_scope: NestedScope,
    lexical_scopes: AHashMap<ast::Id, Span>,
    dynamic_scopes: AHashMap<ast::Id, Vec<Span>>,
}

#[derive(Debug)]
struct VariableBinder {
    unresolved_mark: Mark,
    module: Span,
    scope_builder: ScopeBuilder,
    lexical_scopes: AHashMap<ast::Id, Span>,
    dynamic_scopes: AHashMap<ast::Id, Vec<Span>>,
}

impl VariableBinder {
    fn new(unresolved_mark: Mark, module: &SWCModule) -> Self {
        let scope_builder = ScopeBuilder::new(module);
        Self {
            unresolved_mark,
            module: module.span,
            scope_builder,
            lexical_scopes: Default::default(),
            dynamic_scopes: Default::default(),
        }
    }

    fn visit_ident(&mut self, n: &ast::Ident) {
        let (atom, syntax_context) = n.to_id();

        // Contextless identifiers do not contain unqualified variable references.
        // E.g., in `function a(b) { c.d; }` `a`, `b`, and `c` have a non-empty context, but `d`
        // does not.
        if syntax_context != SyntaxContext::empty() {
            let id = (atom, syntax_context);
            let current_scope_span = { *self.scope_builder.current().span() };
            let dynamic_scopes = if syntax_context.has_mark(self.unresolved_mark) {
                // Unbound varibles are marked with `unresolved_mark`. Use module span to identify
                // lexical scope, and consider all of current scopes ancestors (including itself)
                // as candidates for dynamic scopes.
                self.lexical_scopes.insert(id.clone(), self.module.into());
                self.scope_builder.ancestors(&current_scope_span)
            } else {
                // Variable is bound in some lexical scope in this module.
                //
                // Note: Use blocks to bound scope of borrows, and avoid multiple incompatible
                // borrows.
                if let Some(previous_scope_span) =
                    { self.lexical_scopes.get(&id).map(Clone::clone) }
                {
                    // Variable was used in an already visited scope. Redefine its lexical scope
                    // as the nearest common ancestor between the current scope and previously
                    // bound lexical scope.
                    let (new_scope_span, uncommon_current_scope_ancestors) = {
                        let CommonAncestorScopes {
                            mut common_ancestors,
                            uncommon_ancestors: (uncommon_current_scope_ancestors, _),
                        } = self
                            .scope_builder
                            .common_ancestors(&current_scope_span, &previous_scope_span);

                        (
                            *common_ancestors
                                .pop()
                                .expect("common ancestor betwen scopes in same module")
                                .span(),
                            uncommon_current_scope_ancestors,
                        )
                    };
                    self.lexical_scopes.insert(id.clone(), new_scope_span);

                    // Consider all scopes between (but not including) lexical scope and
                    // (including) current scope as candidates for dynamic scopes.
                    uncommon_current_scope_ancestors
                } else {
                    // Variable is not already bound to a lexical scope. Bind it to the current
                    // scope. Consider all of current scope's ancestors (including itself) as
                    // candidates for dynamic scopes.
                    let ancestors = self.scope_builder.ancestors(&current_scope_span);
                    self.lexical_scopes.insert(id.clone(), current_scope_span);
                    ancestors
                }
            }
            .into_iter()
            .filter_map(|scope| match scope {
                // Filter candidate dynamic scopes, retaining only `with (expr) { ... }` scopes.
                NestedScope {
                    kinded: KindedScope::With(with_span),
                    ..
                } => Some(*with_span),
                _ => None,
            })
            .collect::<Vec<_>>();
            if dynamic_scopes.len() > 0 {
                self.dynamic_scopes.insert(id, dynamic_scopes);
            }
        }
    }

    fn build(self) -> ScopesAndVariables {
        ScopesAndVariables {
            top_scope: self.scope_builder.build(),
            lexical_scopes: self.lexical_scopes,
            dynamic_scopes: self.dynamic_scopes,
        }
    }
}

#[derive(Debug)]
struct ModuleAnalysisState {
    syntax_metadata: SyntaxMetadata,
    module: Option<Span>,
    variable_binder: Option<VariableBinder>,
    str_check: AHashSet<Constant>,
}

impl ModuleAnalysisState {
    fn new(syntax_metadata: SyntaxMetadata) -> Self {
        Self {
            syntax_metadata,
            module: None,
            variable_binder: None,
            str_check: Default::default(),
        }
    }

    fn variable_binder(&mut self) -> &mut VariableBinder {
        self.variable_binder.as_mut().expect("variable binder")
    }

    fn scope_builder(&mut self) -> &mut ScopeBuilder {
        &mut self.variable_binder().scope_builder
    }
}

impl From<ModuleAnalysisState> for Result<ModuleAnalysis> {
    fn from(state: ModuleAnalysisState) -> Self {
        let scopes_and_variables = state.variable_binder.expect("variable binder").build();
        Ok(ModuleAnalysis {
            syntax_metadata: state.syntax_metadata,
            scopes_and_variables,
        })
    }
}

impl Visit for ModuleAnalysisState {
    fn visit_accessibility(&mut self, n: &ast::Accessibility) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_array_lit(&mut self, n: &ast::ArrayLit) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_array_pat(&mut self, n: &ast::ArrayPat) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_arrow_expr(&mut self, n: &ast::ArrowExpr) {
        self.scope_builder().push(n);
        n.visit_children_with(self);
        self.scope_builder().pop();
    }
    fn visit_assign_expr(&mut self, n: &ast::AssignExpr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_assign_op(&mut self, n: &ast::AssignOp) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_assign_pat(&mut self, n: &ast::AssignPat) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_assign_pat_prop(&mut self, n: &ast::AssignPatProp) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_assign_prop(&mut self, n: &ast::AssignProp) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_atom(&mut self, n: &Atom) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_auto_accessor(&mut self, n: &ast::AutoAccessor) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_await_expr(&mut self, n: &ast::AwaitExpr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_big_int(&mut self, n: &ast::BigInt) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_big_int_value(&mut self, n: &BigIntValue) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_bin_expr(&mut self, n: &ast::BinExpr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_binary_op(&mut self, n: &ast::BinaryOp) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_binding_ident(&mut self, n: &ast::BindingIdent) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_block_stmt(&mut self, n: &ast::BlockStmt) {
        match self.scope_builder().current() {
            NestedScope {
                kinded: KindedScope::ArrowFunction(BlockStmtOrExpr::BlockStmt(block_stmt)),
                ..
            } => {
                assert_eq!(&n.span, block_stmt);
                n.visit_children_with(self)
            }
            _ => {
                self.scope_builder().push(n);
                n.visit_children_with(self);
                self.scope_builder().pop();
            }
        }
    }
    fn visit_block_stmt_or_expr(&mut self, n: &ast::BlockStmtOrExpr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_bool(&mut self, n: &ast::Bool) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_break_stmt(&mut self, n: &ast::BreakStmt) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_call_expr(&mut self, n: &ast::CallExpr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_callee(&mut self, n: &ast::Callee) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_catch_clause(&mut self, n: &ast::CatchClause) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_class(&mut self, n: &ast::Class) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_class_decl(&mut self, n: &ast::ClassDecl) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_class_expr(&mut self, n: &ast::ClassExpr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_class_member(&mut self, n: &ast::ClassMember) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_class_members(&mut self, n: &[ast::ClassMember]) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_class_method(&mut self, n: &ast::ClassMethod) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_class_prop(&mut self, n: &ast::ClassProp) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_computed_prop_name(&mut self, n: &ast::ComputedPropName) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_cond_expr(&mut self, n: &ast::CondExpr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_constructor(&mut self, n: &ast::Constructor) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_continue_stmt(&mut self, n: &ast::ContinueStmt) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_debugger_stmt(&mut self, n: &ast::DebuggerStmt) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_decl(&mut self, n: &ast::Decl) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_decorator(&mut self, n: &ast::Decorator) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_decorators(&mut self, n: &[ast::Decorator]) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_default_decl(&mut self, n: &ast::DefaultDecl) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_do_while_stmt(&mut self, n: &ast::DoWhileStmt) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_empty_stmt(&mut self, n: &ast::EmptyStmt) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_export_all(&mut self, n: &ast::ExportAll) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_export_decl(&mut self, n: &ast::ExportDecl) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_export_default_decl(&mut self, n: &ast::ExportDefaultDecl) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_export_default_expr(&mut self, n: &ast::ExportDefaultExpr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_export_default_specifier(&mut self, n: &ast::ExportDefaultSpecifier) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_export_named_specifier(&mut self, n: &ast::ExportNamedSpecifier) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_export_namespace_specifier(&mut self, n: &ast::ExportNamespaceSpecifier) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_export_specifier(&mut self, n: &ast::ExportSpecifier) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_export_specifiers(&mut self, n: &[ast::ExportSpecifier]) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_expr(&mut self, n: &ast::Expr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_expr_or_spread(&mut self, n: &ast::ExprOrSpread) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_expr_or_spreads(&mut self, n: &[ast::ExprOrSpread]) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_expr_stmt(&mut self, n: &ast::ExprStmt) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_exprs(&mut self, n: &[Box<ast::Expr>]) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_fn_decl(&mut self, n: &ast::FnDecl) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_fn_expr(&mut self, n: &ast::FnExpr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_for_in_stmt(&mut self, n: &ast::ForInStmt) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_for_of_stmt(&mut self, n: &ast::ForOfStmt) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_for_stmt(&mut self, n: &ast::ForStmt) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_function(&mut self, n: &ast::Function) {
        self.scope_builder().push(n);
        n.visit_children_with(self);
        self.scope_builder().pop();
    }
    fn visit_getter_prop(&mut self, n: &ast::GetterProp) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ident(&mut self, n: &ast::Ident) {
        self.variable_binder().visit_ident(n);
        n.visit_children_with(self)
    }
    fn visit_if_stmt(&mut self, n: &ast::IfStmt) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_import(&mut self, n: &ast::Import) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_import_decl(&mut self, n: &ast::ImportDecl) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_import_default_specifier(&mut self, n: &ast::ImportDefaultSpecifier) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_import_named_specifier(&mut self, n: &ast::ImportNamedSpecifier) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_import_specifier(&mut self, n: &ast::ImportSpecifier) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_import_specifiers(&mut self, n: &[ast::ImportSpecifier]) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_import_star_as_specifier(&mut self, n: &ast::ImportStarAsSpecifier) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_invalid(&mut self, n: &ast::Invalid) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_js_word(&mut self, n: &JsWord) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_jsx_attr(&mut self, n: &ast::JSXAttr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_jsx_attr_name(&mut self, n: &ast::JSXAttrName) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_jsx_attr_or_spread(&mut self, n: &ast::JSXAttrOrSpread) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_jsx_attr_or_spreads(&mut self, n: &[ast::JSXAttrOrSpread]) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_jsx_attr_value(&mut self, n: &ast::JSXAttrValue) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_jsx_closing_element(&mut self, n: &ast::JSXClosingElement) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_jsx_closing_fragment(&mut self, n: &ast::JSXClosingFragment) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_jsx_element(&mut self, n: &ast::JSXElement) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_jsx_element_child(&mut self, n: &ast::JSXElementChild) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_jsx_element_children(&mut self, n: &[ast::JSXElementChild]) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_jsx_element_name(&mut self, n: &ast::JSXElementName) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_jsx_empty_expr(&mut self, n: &ast::JSXEmptyExpr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_jsx_expr(&mut self, n: &ast::JSXExpr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_jsx_expr_container(&mut self, n: &ast::JSXExprContainer) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_jsx_fragment(&mut self, n: &ast::JSXFragment) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_jsx_member_expr(&mut self, n: &ast::JSXMemberExpr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_jsx_namespaced_name(&mut self, n: &ast::JSXNamespacedName) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_jsx_object(&mut self, n: &ast::JSXObject) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_jsx_opening_element(&mut self, n: &ast::JSXOpeningElement) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_jsx_opening_fragment(&mut self, n: &ast::JSXOpeningFragment) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_jsx_spread_child(&mut self, n: &ast::JSXSpreadChild) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_jsx_text(&mut self, n: &ast::JSXText) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_key(&mut self, n: &ast::Key) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_key_value_pat_prop(&mut self, n: &ast::KeyValuePatProp) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_key_value_prop(&mut self, n: &ast::KeyValueProp) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_labeled_stmt(&mut self, n: &ast::LabeledStmt) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_lit(&mut self, n: &ast::Lit) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_member_expr(&mut self, n: &ast::MemberExpr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_member_prop(&mut self, n: &ast::MemberProp) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_meta_prop_expr(&mut self, n: &ast::MetaPropExpr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_meta_prop_kind(&mut self, n: &ast::MetaPropKind) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_method_kind(&mut self, n: &ast::MethodKind) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_method_prop(&mut self, n: &ast::MethodProp) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_module(&mut self, n: &SWCModule) {
        self.module = Some(n.span);
        self.variable_binder = Some(VariableBinder::new(self.syntax_metadata.unresolved_mark, n));
        n.visit_children_with(self)
    }
    fn visit_module_decl(&mut self, n: &ast::ModuleDecl) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_module_export_name(&mut self, n: &ast::ModuleExportName) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_module_item(&mut self, n: &ast::ModuleItem) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_module_items(&mut self, n: &[ast::ModuleItem]) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_named_export(&mut self, n: &ast::NamedExport) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_new_expr(&mut self, n: &ast::NewExpr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_null(&mut self, n: &ast::Null) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_number(&mut self, n: &ast::Number) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_object_lit(&mut self, n: &ast::ObjectLit) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_object_pat(&mut self, n: &ast::ObjectPat) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_object_pat_prop(&mut self, n: &ast::ObjectPatProp) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_object_pat_props(&mut self, n: &[ast::ObjectPatProp]) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_opt_accessibility(&mut self, n: Option<&ast::Accessibility>) {
        // TODO: Implement analysis visitor.
        n.map(|n| {
            n.visit_with(self);
        });
    }
    fn visit_opt_atom(&mut self, n: Option<&Atom>) {
        // TODO: Implement analysis visitor.
        n.map(|n| {
            n.visit_with(self);
        });
    }
    fn visit_opt_block_stmt(&mut self, n: Option<&ast::BlockStmt>) {
        // TODO: Implement analysis visitor.
        n.map(|n| {
            n.visit_with(self);
        });
    }
    fn visit_opt_call(&mut self, n: &ast::OptCall) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_opt_catch_clause(&mut self, n: Option<&ast::CatchClause>) {
        // TODO: Implement analysis visitor.
        n.map(|n| {
            n.visit_with(self);
        });
    }
    fn visit_opt_chain_base(&mut self, n: &ast::OptChainBase) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_opt_chain_expr(&mut self, n: &ast::OptChainExpr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_opt_expr(&mut self, n: Option<&Box<ast::Expr>>) {
        // TODO: Implement analysis visitor.
        n.map(|n| {
            n.visit_with(self);
        });
    }
    fn visit_opt_expr_or_spread(&mut self, n: Option<&ast::ExprOrSpread>) {
        // TODO: Implement analysis visitor.
        n.map(|n| {
            n.visit_with(self);
        });
    }
    fn visit_opt_expr_or_spreads(&mut self, n: Option<&[ast::ExprOrSpread]>) {
        // TODO: Implement analysis visitor.
        n.map(|n| {
            n.visit_with(self);
        });
    }
    fn visit_opt_ident(&mut self, n: Option<&ast::Ident>) {
        // TODO: Implement analysis visitor.
        n.map(|n| {
            n.visit_with(self);
        });
    }
    fn visit_opt_jsx_attr_value(&mut self, n: Option<&ast::JSXAttrValue>) {
        // TODO: Implement analysis visitor.
        n.map(|n| {
            n.visit_with(self);
        });
    }
    fn visit_opt_jsx_closing_element(&mut self, n: Option<&ast::JSXClosingElement>) {
        // TODO: Implement analysis visitor.
        n.map(|n| {
            n.visit_with(self);
        });
    }
    fn visit_opt_module_export_name(&mut self, n: Option<&ast::ModuleExportName>) {
        // TODO: Implement analysis visitor.
        n.map(|n| {
            n.visit_with(self);
        });
    }
    fn visit_opt_module_items(&mut self, n: Option<&[ast::ModuleItem]>) {
        // TODO: Implement analysis visitor.
        n.map(|n| {
            n.visit_with(self);
        });
    }
    fn visit_opt_object_lit(&mut self, n: Option<&Box<ast::ObjectLit>>) {
        // TODO: Implement analysis visitor.
        n.map(|n| {
            n.visit_with(self);
        });
    }
    fn visit_opt_pat(&mut self, n: Option<&ast::Pat>) {
        // TODO: Implement analysis visitor.
        n.map(|n| {
            n.visit_with(self);
        });
    }
    fn visit_opt_span(&mut self, n: Option<&Span>) {
        // TODO: Implement analysis visitor.
        n.map(|n| {
            n.visit_with(self);
        });
    }
    fn visit_opt_stmt(&mut self, n: Option<&Box<ast::Stmt>>) {
        // TODO: Implement analysis visitor.
        n.map(|n| {
            n.visit_with(self);
        });
    }
    fn visit_opt_str(&mut self, n: Option<&Box<ast::Str>>) {
        // TODO: Implement analysis visitor.
        n.map(|n| {
            n.visit_with(self);
        });
    }
    fn visit_opt_true_plus_minus(&mut self, n: Option<&ast::TruePlusMinus>) {
        // TODO: Implement analysis visitor.
        n.map(|n| {
            n.visit_with(self);
        });
    }
    fn visit_opt_ts_entity_name(&mut self, n: Option<&ast::TsEntityName>) {
        // TODO: Implement analysis visitor.
        n.map(|n| {
            n.visit_with(self);
        });
    }
    fn visit_opt_ts_namespace_body(&mut self, n: Option<&ast::TsNamespaceBody>) {
        // TODO: Implement analysis visitor.
        n.map(|n| {
            n.visit_with(self);
        });
    }
    fn visit_opt_ts_type(&mut self, n: Option<&Box<ast::TsType>>) {
        // TODO: Implement analysis visitor.
        n.map(|n| {
            n.visit_with(self);
        });
    }
    fn visit_opt_ts_type_ann(&mut self, n: Option<&Box<ast::TsTypeAnn>>) {
        // TODO: Implement analysis visitor.
        n.map(|n| {
            n.visit_with(self);
        });
    }
    fn visit_opt_ts_type_param_decl(&mut self, n: Option<&Box<ast::TsTypeParamDecl>>) {
        // TODO: Implement analysis visitor.
        n.map(|n| {
            n.visit_with(self);
        });
    }
    fn visit_opt_ts_type_param_instantiation(
        &mut self,
        n: Option<&Box<ast::TsTypeParamInstantiation>>,
    ) {
        // TODO: Implement analysis visitor.
        n.map(|n| {
            n.visit_with(self);
        });
    }
    fn visit_opt_var_decl_or_expr(&mut self, n: Option<&ast::VarDeclOrExpr>) {
        // TODO: Implement analysis visitor.
        n.map(|n| {
            n.visit_with(self);
        });
    }
    fn visit_opt_vec_expr_or_spreads(&mut self, n: &[Option<ast::ExprOrSpread>]) {
        // TODO: Implement analysis visitor.
        n.iter().for_each(|n| {
            n.as_ref().map(|n| {
                n.visit_children_with(self);
            });
        })
    }
    fn visit_opt_vec_pats(&mut self, n: &[Option<ast::Pat>]) {
        // TODO: Implement analysis visitor.
        n.iter().for_each(|n| {
            n.as_ref().map(|n| {
                n.visit_children_with(self);
            });
        })
    }
    fn visit_param(&mut self, n: &ast::Param) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_param_or_ts_param_prop(&mut self, n: &ast::ParamOrTsParamProp) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_param_or_ts_param_props(&mut self, n: &[ast::ParamOrTsParamProp]) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_params(&mut self, n: &[ast::Param]) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_paren_expr(&mut self, n: &ast::ParenExpr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_pat(&mut self, n: &ast::Pat) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_pat_or_expr(&mut self, n: &ast::PatOrExpr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_pats(&mut self, n: &[ast::Pat]) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_private_method(&mut self, n: &ast::PrivateMethod) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_private_name(&mut self, n: &ast::PrivateName) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_private_prop(&mut self, n: &ast::PrivateProp) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_program(&mut self, n: &ast::Program) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_prop(&mut self, n: &ast::Prop) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_prop_name(&mut self, n: &ast::PropName) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_prop_or_spread(&mut self, n: &ast::PropOrSpread) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_prop_or_spreads(&mut self, n: &[ast::PropOrSpread]) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_regex(&mut self, n: &ast::Regex) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_reserved_unused(&mut self, n: &ast::ReservedUnused) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_rest_pat(&mut self, n: &ast::RestPat) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_return_stmt(&mut self, n: &ast::ReturnStmt) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_script(&mut self, n: &ast::Script) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_seq_expr(&mut self, n: &ast::SeqExpr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_setter_prop(&mut self, n: &ast::SetterProp) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_span(&mut self, n: &Span) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_spread_element(&mut self, n: &ast::SpreadElement) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_static_block(&mut self, n: &ast::StaticBlock) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_stmt(&mut self, n: &ast::Stmt) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_stmts(&mut self, n: &[ast::Stmt]) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_str(&mut self, n: &ast::Str) {
        self.str_check.insert(Constant::str(n.value.clone()));
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_super(&mut self, n: &ast::Super) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_super_prop(&mut self, n: &ast::SuperProp) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_super_prop_expr(&mut self, n: &ast::SuperPropExpr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_switch_case(&mut self, n: &ast::SwitchCase) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_switch_cases(&mut self, n: &[ast::SwitchCase]) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_switch_stmt(&mut self, n: &ast::SwitchStmt) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_tagged_tpl(&mut self, n: &ast::TaggedTpl) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_this_expr(&mut self, n: &ast::ThisExpr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_throw_stmt(&mut self, n: &ast::ThrowStmt) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_tpl(&mut self, n: &ast::Tpl) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_tpl_element(&mut self, n: &ast::TplElement) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_tpl_elements(&mut self, n: &[ast::TplElement]) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_true_plus_minus(&mut self, n: &ast::TruePlusMinus) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_try_stmt(&mut self, n: &ast::TryStmt) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_array_type(&mut self, n: &ast::TsArrayType) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_as_expr(&mut self, n: &ast::TsAsExpr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_call_signature_decl(&mut self, n: &ast::TsCallSignatureDecl) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_conditional_type(&mut self, n: &ast::TsConditionalType) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_const_assertion(&mut self, n: &ast::TsConstAssertion) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_construct_signature_decl(&mut self, n: &ast::TsConstructSignatureDecl) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_constructor_type(&mut self, n: &ast::TsConstructorType) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_entity_name(&mut self, n: &ast::TsEntityName) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_enum_decl(&mut self, n: &ast::TsEnumDecl) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_enum_member(&mut self, n: &ast::TsEnumMember) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_enum_member_id(&mut self, n: &ast::TsEnumMemberId) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_enum_members(&mut self, n: &[ast::TsEnumMember]) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_export_assignment(&mut self, n: &ast::TsExportAssignment) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_expr_with_type_args(&mut self, n: &ast::TsExprWithTypeArgs) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_expr_with_type_args_vec(&mut self, n: &[ast::TsExprWithTypeArgs]) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_external_module_ref(&mut self, n: &ast::TsExternalModuleRef) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_fn_or_constructor_type(&mut self, n: &ast::TsFnOrConstructorType) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_fn_param(&mut self, n: &ast::TsFnParam) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_fn_params(&mut self, n: &[ast::TsFnParam]) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_fn_type(&mut self, n: &ast::TsFnType) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_getter_signature(&mut self, n: &ast::TsGetterSignature) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_import_equals_decl(&mut self, n: &ast::TsImportEqualsDecl) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_import_type(&mut self, n: &ast::TsImportType) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_index_signature(&mut self, n: &ast::TsIndexSignature) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_indexed_access_type(&mut self, n: &ast::TsIndexedAccessType) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_infer_type(&mut self, n: &ast::TsInferType) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_instantiation(&mut self, n: &ast::TsInstantiation) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_interface_body(&mut self, n: &ast::TsInterfaceBody) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_interface_decl(&mut self, n: &ast::TsInterfaceDecl) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_intersection_type(&mut self, n: &ast::TsIntersectionType) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_keyword_type(&mut self, n: &ast::TsKeywordType) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_keyword_type_kind(&mut self, n: &ast::TsKeywordTypeKind) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_lit(&mut self, n: &ast::TsLit) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_lit_type(&mut self, n: &ast::TsLitType) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_mapped_type(&mut self, n: &ast::TsMappedType) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_method_signature(&mut self, n: &ast::TsMethodSignature) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_module_block(&mut self, n: &ast::TsModuleBlock) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_module_decl(&mut self, n: &ast::TsModuleDecl) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_module_name(&mut self, n: &ast::TsModuleName) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_module_ref(&mut self, n: &ast::TsModuleRef) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_namespace_body(&mut self, n: &ast::TsNamespaceBody) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_namespace_decl(&mut self, n: &ast::TsNamespaceDecl) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_namespace_export_decl(&mut self, n: &ast::TsNamespaceExportDecl) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_non_null_expr(&mut self, n: &ast::TsNonNullExpr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_optional_type(&mut self, n: &ast::TsOptionalType) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_param_prop(&mut self, n: &ast::TsParamProp) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_param_prop_param(&mut self, n: &ast::TsParamPropParam) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_parenthesized_type(&mut self, n: &ast::TsParenthesizedType) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_property_signature(&mut self, n: &ast::TsPropertySignature) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_qualified_name(&mut self, n: &ast::TsQualifiedName) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_rest_type(&mut self, n: &ast::TsRestType) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_satisfies_expr(&mut self, n: &ast::TsSatisfiesExpr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_setter_signature(&mut self, n: &ast::TsSetterSignature) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_this_type(&mut self, n: &ast::TsThisType) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_this_type_or_ident(&mut self, n: &ast::TsThisTypeOrIdent) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_tpl_lit_type(&mut self, n: &ast::TsTplLitType) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_tuple_element(&mut self, n: &ast::TsTupleElement) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_tuple_elements(&mut self, n: &[ast::TsTupleElement]) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_tuple_type(&mut self, n: &ast::TsTupleType) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_type(&mut self, n: &ast::TsType) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_type_alias_decl(&mut self, n: &ast::TsTypeAliasDecl) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_type_ann(&mut self, n: &ast::TsTypeAnn) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_type_assertion(&mut self, n: &ast::TsTypeAssertion) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_type_element(&mut self, n: &ast::TsTypeElement) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_type_elements(&mut self, n: &[ast::TsTypeElement]) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_type_lit(&mut self, n: &ast::TsTypeLit) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_type_operator(&mut self, n: &ast::TsTypeOperator) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_type_operator_op(&mut self, n: &ast::TsTypeOperatorOp) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_type_param(&mut self, n: &ast::TsTypeParam) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_type_param_decl(&mut self, n: &ast::TsTypeParamDecl) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_type_param_instantiation(&mut self, n: &ast::TsTypeParamInstantiation) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_type_params(&mut self, n: &[ast::TsTypeParam]) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_type_predicate(&mut self, n: &ast::TsTypePredicate) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_type_query(&mut self, n: &ast::TsTypeQuery) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_type_query_expr(&mut self, n: &ast::TsTypeQueryExpr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_type_ref(&mut self, n: &ast::TsTypeRef) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_types(&mut self, n: &[Box<ast::TsType>]) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_union_or_intersection_type(&mut self, n: &ast::TsUnionOrIntersectionType) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_union_type(&mut self, n: &ast::TsUnionType) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_unary_expr(&mut self, n: &ast::UnaryExpr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_unary_op(&mut self, n: &ast::UnaryOp) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_update_expr(&mut self, n: &ast::UpdateExpr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_update_op(&mut self, n: &ast::UpdateOp) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_var_decl(&mut self, n: &ast::VarDecl) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_var_decl_kind(&mut self, n: &ast::VarDeclKind) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_var_decl_or_expr(&mut self, n: &ast::VarDeclOrExpr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_var_decl_or_pat(&mut self, n: &ast::VarDeclOrPat) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_var_declarator(&mut self, n: &ast::VarDeclarator) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_var_declarators(&mut self, n: &[ast::VarDeclarator]) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_while_stmt(&mut self, n: &ast::WhileStmt) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_with_stmt(&mut self, n: &ast::WithStmt) {
        n.obj.visit_with(self);
        self.scope_builder().push(n);
        n.body.visit_with(self);
        self.scope_builder().pop();
    }
    fn visit_yield_expr(&mut self, n: &ast::YieldExpr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
}
