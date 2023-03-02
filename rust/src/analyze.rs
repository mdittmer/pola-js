use std::borrow::BorrowMut;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt::Debug;
use std::path::Path;

use crate::parse::parse_file;
use crate::parse::parse_file_with_handler;
use crate::parse::parse_str;
use crate::parse::parse_str_with_handler;
use crate::parse::Module;
use color_eyre::Result;
use num_bigint::BigInt as BigIntValue;
use swc_atoms::Atom;
use swc_atoms::JsWord;
use swc_common::errors::Handler;
use swc_common::source_map::Span;
use swc_common::SyntaxContext;
use swc_ecma_ast::Accessibility;
use swc_ecma_ast::ArrayLit;
use swc_ecma_ast::ArrayPat;
use swc_ecma_ast::ArrowExpr;
use swc_ecma_ast::AssignExpr;
use swc_ecma_ast::AssignOp;
use swc_ecma_ast::AssignPat;
use swc_ecma_ast::AssignPatProp;
use swc_ecma_ast::AssignProp;
use swc_ecma_ast::AutoAccessor;
use swc_ecma_ast::AwaitExpr;
use swc_ecma_ast::BigInt;
use swc_ecma_ast::BinExpr;
use swc_ecma_ast::BinaryOp;
use swc_ecma_ast::BindingIdent;
use swc_ecma_ast::BlockStmt;
use swc_ecma_ast::BlockStmtOrExpr;
use swc_ecma_ast::Bool;
use swc_ecma_ast::BreakStmt;
use swc_ecma_ast::CallExpr;
use swc_ecma_ast::Callee;
use swc_ecma_ast::CatchClause;
use swc_ecma_ast::Class;
use swc_ecma_ast::ClassDecl;
use swc_ecma_ast::ClassExpr;
use swc_ecma_ast::ClassMember;
use swc_ecma_ast::ClassMethod;
use swc_ecma_ast::ClassProp;
use swc_ecma_ast::ComputedPropName;
use swc_ecma_ast::CondExpr;
use swc_ecma_ast::Constructor;
use swc_ecma_ast::ContinueStmt;
use swc_ecma_ast::DebuggerStmt;
use swc_ecma_ast::Decl;
use swc_ecma_ast::Decorator;
use swc_ecma_ast::DefaultDecl;
use swc_ecma_ast::DoWhileStmt;
use swc_ecma_ast::EmptyStmt;
use swc_ecma_ast::EsVersion;
use swc_ecma_ast::ExportAll;
use swc_ecma_ast::ExportDecl;
use swc_ecma_ast::ExportDefaultDecl;
use swc_ecma_ast::ExportDefaultExpr;
use swc_ecma_ast::ExportDefaultSpecifier;
use swc_ecma_ast::ExportNamedSpecifier;
use swc_ecma_ast::ExportNamespaceSpecifier;
use swc_ecma_ast::ExportSpecifier;
use swc_ecma_ast::Expr;
use swc_ecma_ast::ExprOrSpread;
use swc_ecma_ast::ExprStmt;
use swc_ecma_ast::FnDecl;
use swc_ecma_ast::FnExpr;
use swc_ecma_ast::ForInStmt;
use swc_ecma_ast::ForOfStmt;
use swc_ecma_ast::ForStmt;
use swc_ecma_ast::Function;
use swc_ecma_ast::GetterProp;
use swc_ecma_ast::Id;
use swc_ecma_ast::Ident;
use swc_ecma_ast::IfStmt;
use swc_ecma_ast::Import;
use swc_ecma_ast::ImportDecl;
use swc_ecma_ast::ImportDefaultSpecifier;
use swc_ecma_ast::ImportNamedSpecifier;
use swc_ecma_ast::ImportSpecifier;
use swc_ecma_ast::ImportStarAsSpecifier;
use swc_ecma_ast::Invalid;
use swc_ecma_ast::JSXAttr;
use swc_ecma_ast::JSXAttrName;
use swc_ecma_ast::JSXAttrOrSpread;
use swc_ecma_ast::JSXAttrValue;
use swc_ecma_ast::JSXClosingElement;
use swc_ecma_ast::JSXClosingFragment;
use swc_ecma_ast::JSXElement;
use swc_ecma_ast::JSXElementChild;
use swc_ecma_ast::JSXElementName;
use swc_ecma_ast::JSXEmptyExpr;
use swc_ecma_ast::JSXExpr;
use swc_ecma_ast::JSXExprContainer;
use swc_ecma_ast::JSXFragment;
use swc_ecma_ast::JSXMemberExpr;
use swc_ecma_ast::JSXNamespacedName;
use swc_ecma_ast::JSXObject;
use swc_ecma_ast::JSXOpeningElement;
use swc_ecma_ast::JSXOpeningFragment;
use swc_ecma_ast::JSXSpreadChild;
use swc_ecma_ast::JSXText;
use swc_ecma_ast::Key;
use swc_ecma_ast::KeyValuePatProp;
use swc_ecma_ast::KeyValueProp;
use swc_ecma_ast::LabeledStmt;
use swc_ecma_ast::Lit;
use swc_ecma_ast::MemberExpr;
use swc_ecma_ast::MemberProp;
use swc_ecma_ast::MetaPropExpr;
use swc_ecma_ast::MetaPropKind;
use swc_ecma_ast::MethodKind;
use swc_ecma_ast::MethodProp;
use swc_ecma_ast::Module as SWCModule;
use swc_ecma_ast::ModuleDecl;
use swc_ecma_ast::ModuleExportName;
use swc_ecma_ast::ModuleItem;
use swc_ecma_ast::NamedExport;
use swc_ecma_ast::NewExpr;
use swc_ecma_ast::Null;
use swc_ecma_ast::Number;
use swc_ecma_ast::ObjectLit;
use swc_ecma_ast::ObjectPat;
use swc_ecma_ast::ObjectPatProp;
use swc_ecma_ast::OptCall;
use swc_ecma_ast::OptChainBase;
use swc_ecma_ast::OptChainExpr;
use swc_ecma_ast::Param;
use swc_ecma_ast::ParamOrTsParamProp;
use swc_ecma_ast::ParenExpr;
use swc_ecma_ast::Pat;
use swc_ecma_ast::PatOrExpr;
use swc_ecma_ast::PrivateMethod;
use swc_ecma_ast::PrivateName;
use swc_ecma_ast::PrivateProp;
use swc_ecma_ast::Program;
use swc_ecma_ast::Prop;
use swc_ecma_ast::PropName;
use swc_ecma_ast::PropOrSpread;
use swc_ecma_ast::Regex;
use swc_ecma_ast::ReservedUnused;
use swc_ecma_ast::RestPat;
use swc_ecma_ast::ReturnStmt;
use swc_ecma_ast::Script;
use swc_ecma_ast::SeqExpr;
use swc_ecma_ast::SetterProp;
use swc_ecma_ast::SpreadElement;
use swc_ecma_ast::StaticBlock;
use swc_ecma_ast::Stmt;
use swc_ecma_ast::Str;
use swc_ecma_ast::Super;
use swc_ecma_ast::SuperProp;
use swc_ecma_ast::SuperPropExpr;
use swc_ecma_ast::SwitchCase;
use swc_ecma_ast::SwitchStmt;
use swc_ecma_ast::TaggedTpl;
use swc_ecma_ast::ThisExpr;
use swc_ecma_ast::ThrowStmt;
use swc_ecma_ast::Tpl;
use swc_ecma_ast::TplElement;
use swc_ecma_ast::TruePlusMinus;
use swc_ecma_ast::TryStmt;
use swc_ecma_ast::TsArrayType;
use swc_ecma_ast::TsAsExpr;
use swc_ecma_ast::TsCallSignatureDecl;
use swc_ecma_ast::TsConditionalType;
use swc_ecma_ast::TsConstAssertion;
use swc_ecma_ast::TsConstructSignatureDecl;
use swc_ecma_ast::TsConstructorType;
use swc_ecma_ast::TsEntityName;
use swc_ecma_ast::TsEnumDecl;
use swc_ecma_ast::TsEnumMember;
use swc_ecma_ast::TsEnumMemberId;
use swc_ecma_ast::TsExportAssignment;
use swc_ecma_ast::TsExprWithTypeArgs;
use swc_ecma_ast::TsExternalModuleRef;
use swc_ecma_ast::TsFnOrConstructorType;
use swc_ecma_ast::TsFnParam;
use swc_ecma_ast::TsFnType;
use swc_ecma_ast::TsGetterSignature;
use swc_ecma_ast::TsImportEqualsDecl;
use swc_ecma_ast::TsImportType;
use swc_ecma_ast::TsIndexSignature;
use swc_ecma_ast::TsIndexedAccessType;
use swc_ecma_ast::TsInferType;
use swc_ecma_ast::TsInstantiation;
use swc_ecma_ast::TsInterfaceBody;
use swc_ecma_ast::TsInterfaceDecl;
use swc_ecma_ast::TsIntersectionType;
use swc_ecma_ast::TsKeywordType;
use swc_ecma_ast::TsKeywordTypeKind;
use swc_ecma_ast::TsLit;
use swc_ecma_ast::TsLitType;
use swc_ecma_ast::TsMappedType;
use swc_ecma_ast::TsMethodSignature;
use swc_ecma_ast::TsModuleBlock;
use swc_ecma_ast::TsModuleDecl;
use swc_ecma_ast::TsModuleName;
use swc_ecma_ast::TsModuleRef;
use swc_ecma_ast::TsNamespaceBody;
use swc_ecma_ast::TsNamespaceDecl;
use swc_ecma_ast::TsNamespaceExportDecl;
use swc_ecma_ast::TsNonNullExpr;
use swc_ecma_ast::TsOptionalType;
use swc_ecma_ast::TsParamProp;
use swc_ecma_ast::TsParamPropParam;
use swc_ecma_ast::TsParenthesizedType;
use swc_ecma_ast::TsPropertySignature;
use swc_ecma_ast::TsQualifiedName;
use swc_ecma_ast::TsRestType;
use swc_ecma_ast::TsSatisfiesExpr;
use swc_ecma_ast::TsSetterSignature;
use swc_ecma_ast::TsThisType;
use swc_ecma_ast::TsThisTypeOrIdent;
use swc_ecma_ast::TsTplLitType;
use swc_ecma_ast::TsTupleElement;
use swc_ecma_ast::TsTupleType;
use swc_ecma_ast::TsType;
use swc_ecma_ast::TsTypeAliasDecl;
use swc_ecma_ast::TsTypeAnn;
use swc_ecma_ast::TsTypeAssertion;
use swc_ecma_ast::TsTypeElement;
use swc_ecma_ast::TsTypeLit;
use swc_ecma_ast::TsTypeOperator;
use swc_ecma_ast::TsTypeOperatorOp;
use swc_ecma_ast::TsTypeParam;
use swc_ecma_ast::TsTypeParamDecl;
use swc_ecma_ast::TsTypeParamInstantiation;
use swc_ecma_ast::TsTypePredicate;
use swc_ecma_ast::TsTypeQuery;
use swc_ecma_ast::TsTypeQueryExpr;
use swc_ecma_ast::TsTypeRef;
use swc_ecma_ast::TsUnionOrIntersectionType;
use swc_ecma_ast::TsUnionType;
use swc_ecma_ast::UnaryExpr;
use swc_ecma_ast::UnaryOp;
use swc_ecma_ast::UpdateExpr;
use swc_ecma_ast::UpdateOp;
use swc_ecma_ast::VarDecl;
use swc_ecma_ast::VarDeclKind;
use swc_ecma_ast::VarDeclOrExpr;
use swc_ecma_ast::VarDeclOrPat;
use swc_ecma_ast::VarDeclarator;
use swc_ecma_ast::WhileStmt;
use swc_ecma_ast::WithStmt;
use swc_ecma_ast::YieldExpr;
use swc_ecma_parser::Syntax;
use swc_ecma_visit::Visit;
use swc_ecma_visit::VisitWith;

#[derive(Debug)]
pub struct FileAnalysis {
    module_scope: Scope,
    variable_uses_in_functions: HashMap<Id, Span>,
}

pub fn analyze_file<P: AsRef<Path>>(
    source_path: P,
    syntax: Syntax,
    target: EsVersion,
) -> Result<FileAnalysis> {
    let module = parse_file(source_path, syntax, target)?;
    analyze_module(&module)
}

pub fn analyze_file_with_handler<P: AsRef<Path>>(
    source_path: P,
    syntax: Syntax,
    target: EsVersion,
    handler: Handler,
) -> Result<FileAnalysis> {
    let module = parse_file_with_handler(source_path, syntax, target, handler)?;
    analyze_module(&module)
}

pub fn analyze_str(
    source_str: &str,
    source_path: &str,
    syntax: Syntax,
    target: EsVersion,
) -> Result<FileAnalysis> {
    let module = parse_str(source_str, source_path, syntax, target)?;
    analyze_module(&module)
}

pub fn analyze_str_with_handler(
    source_str: &str,
    source_path: &str,
    syntax: Syntax,
    target: EsVersion,
    handler: Handler,
) -> Result<FileAnalysis> {
    let module = parse_str_with_handler(source_str, source_path, syntax, target, handler)?;
    analyze_module(&module)
}

#[tracing::instrument(skip(module))]
fn analyze_module(module: &Module) -> Result<FileAnalysis> {
    let mut state = FileAnalysisState::new();
    module.swc_module.visit_with(&mut state);

    state.into()
}

#[derive(Debug, Eq, PartialEq)]
enum KindedScope {
    Module(Span),
    Function(Span),
    Block(Span),
    With(Span),
}

impl From<&SWCModule> for KindedScope {
    fn from(module: &SWCModule) -> Self {
        Self::Module(module.span)
    }
}

impl From<&Function> for KindedScope {
    fn from(function: &Function) -> Self {
        Self::Function(function.span)
    }
}

impl From<&BlockStmt> for KindedScope {
    fn from(block: &BlockStmt) -> Self {
        Self::Block(block.span)
    }
}

impl From<&WithStmt> for KindedScope {
    fn from(with: &WithStmt) -> Self {
        Self::With(with.span)
    }
}

#[derive(Debug, Eq, PartialEq)]
struct Scope {
    kinded: KindedScope,
    inner: Vec<Scope>,
}

impl Scope {
    fn new(kinded: KindedScope) -> Self {
        Self {
            kinded,
            inner: vec![],
        }
    }
}

#[derive(Debug)]
struct ScopeBuilder {
    top_scope: Scope,
    stack: Vec<usize>,
}

impl ScopeBuilder {
    fn new(module: &SWCModule) -> Self {
        let top_scope = Scope::new(module.into());
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
        scope.inner.push(Scope::new(KindedScope::from(node)));
        self.stack.push(idx);
    }

    fn pop(&mut self) {
        self.stack
            .pop()
            .expect("scope builder stack contains a scope index");
    }

    fn function_scope_id(&mut self) -> Span {
        let mut scopes = vec![&self.top_scope];
        let mut scope = &self.top_scope;
        for idx in self.stack.iter() {
            scope = &scope.inner.get(*idx).expect("indexed scope");
            scopes.push(scope);
        }
        while let Some(scope) = scopes.pop() {
            match scope.kinded {
                KindedScope::Function(id) => {
                    return id;
                }
                _ => {}
            }
        }

        if let KindedScope::Module(id) = &self.top_scope.kinded {
            return *id;
        } else {
            panic!("top scope of scope builder is not module scope");
        }
    }

    fn build(self) -> Scope {
        assert!(self.stack.len() == 0);
        self.top_scope
    }
}

#[derive(Debug)]
struct FileAnalysisState {
    scope_builder: Option<ScopeBuilder>,
    variable_uses_in_functions: HashMap<Id, Span>,
}

impl FileAnalysisState {
    fn new() -> Self {
        Self {
            scope_builder: None,
            variable_uses_in_functions: HashMap::new(),
        }
    }

    fn scope_builder(&mut self) -> &mut ScopeBuilder {
        self.scope_builder.as_mut().expect("scope builder")
    }
}

impl From<FileAnalysisState> for Result<FileAnalysis> {
    fn from(state: FileAnalysisState) -> Self {
        let scope_builder = state.scope_builder.expect("scope builder");
        Ok(FileAnalysis {
            module_scope: scope_builder.build(),
            variable_uses_in_functions: state.variable_uses_in_functions,
        })
    }
}

impl Visit for FileAnalysisState {
    fn visit_accessibility(&mut self, n: &Accessibility) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_array_lit(&mut self, n: &ArrayLit) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_array_pat(&mut self, n: &ArrayPat) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_arrow_expr(&mut self, n: &ArrowExpr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_assign_expr(&mut self, n: &AssignExpr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_assign_op(&mut self, n: &AssignOp) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_assign_pat(&mut self, n: &AssignPat) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_assign_pat_prop(&mut self, n: &AssignPatProp) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_assign_prop(&mut self, n: &AssignProp) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_atom(&mut self, n: &Atom) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_auto_accessor(&mut self, n: &AutoAccessor) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_await_expr(&mut self, n: &AwaitExpr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_big_int(&mut self, n: &BigInt) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_big_int_value(&mut self, n: &BigIntValue) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_bin_expr(&mut self, n: &BinExpr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_binary_op(&mut self, n: &BinaryOp) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_binding_ident(&mut self, n: &BindingIdent) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_block_stmt(&mut self, n: &BlockStmt) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_block_stmt_or_expr(&mut self, n: &BlockStmtOrExpr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_bool(&mut self, n: &Bool) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_break_stmt(&mut self, n: &BreakStmt) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_call_expr(&mut self, n: &CallExpr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_callee(&mut self, n: &Callee) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_catch_clause(&mut self, n: &CatchClause) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_class(&mut self, n: &Class) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_class_decl(&mut self, n: &ClassDecl) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_class_expr(&mut self, n: &ClassExpr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_class_member(&mut self, n: &ClassMember) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_class_members(&mut self, n: &[ClassMember]) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_class_method(&mut self, n: &ClassMethod) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_class_prop(&mut self, n: &ClassProp) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_computed_prop_name(&mut self, n: &ComputedPropName) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_cond_expr(&mut self, n: &CondExpr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_constructor(&mut self, n: &Constructor) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_continue_stmt(&mut self, n: &ContinueStmt) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_debugger_stmt(&mut self, n: &DebuggerStmt) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_decl(&mut self, n: &Decl) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_decorator(&mut self, n: &Decorator) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_decorators(&mut self, n: &[Decorator]) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_default_decl(&mut self, n: &DefaultDecl) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_do_while_stmt(&mut self, n: &DoWhileStmt) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_empty_stmt(&mut self, n: &EmptyStmt) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_export_all(&mut self, n: &ExportAll) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_export_decl(&mut self, n: &ExportDecl) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_export_default_decl(&mut self, n: &ExportDefaultDecl) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_export_default_expr(&mut self, n: &ExportDefaultExpr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_export_default_specifier(&mut self, n: &ExportDefaultSpecifier) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_export_named_specifier(&mut self, n: &ExportNamedSpecifier) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_export_namespace_specifier(&mut self, n: &ExportNamespaceSpecifier) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_export_specifier(&mut self, n: &ExportSpecifier) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_export_specifiers(&mut self, n: &[ExportSpecifier]) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_expr(&mut self, n: &Expr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_expr_or_spread(&mut self, n: &ExprOrSpread) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_expr_or_spreads(&mut self, n: &[ExprOrSpread]) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_expr_stmt(&mut self, n: &ExprStmt) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_exprs(&mut self, n: &[Box<Expr>]) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_fn_decl(&mut self, n: &FnDecl) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_fn_expr(&mut self, n: &FnExpr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_for_in_stmt(&mut self, n: &ForInStmt) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_for_of_stmt(&mut self, n: &ForOfStmt) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_for_stmt(&mut self, n: &ForStmt) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_function(&mut self, n: &Function) {
        self.scope_builder().push(n);
        n.visit_children_with(self);
        self.scope_builder().pop();
    }
    fn visit_getter_prop(&mut self, n: &GetterProp) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ident(&mut self, n: &Ident) {
        let (atom, syntax_context) = n.to_id();
        if syntax_context != SyntaxContext::empty() {
            let function_id = self.scope_builder().function_scope_id();
            self.variable_uses_in_functions
                .insert((atom, syntax_context), function_id);
        }
        n.visit_children_with(self)
    }
    fn visit_if_stmt(&mut self, n: &IfStmt) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_import(&mut self, n: &Import) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_import_decl(&mut self, n: &ImportDecl) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_import_default_specifier(&mut self, n: &ImportDefaultSpecifier) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_import_named_specifier(&mut self, n: &ImportNamedSpecifier) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_import_specifier(&mut self, n: &ImportSpecifier) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_import_specifiers(&mut self, n: &[ImportSpecifier]) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_import_star_as_specifier(&mut self, n: &ImportStarAsSpecifier) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_invalid(&mut self, n: &Invalid) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_js_word(&mut self, n: &JsWord) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_jsx_attr(&mut self, n: &JSXAttr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_jsx_attr_name(&mut self, n: &JSXAttrName) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_jsx_attr_or_spread(&mut self, n: &JSXAttrOrSpread) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_jsx_attr_or_spreads(&mut self, n: &[JSXAttrOrSpread]) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_jsx_attr_value(&mut self, n: &JSXAttrValue) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_jsx_closing_element(&mut self, n: &JSXClosingElement) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_jsx_closing_fragment(&mut self, n: &JSXClosingFragment) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_jsx_element(&mut self, n: &JSXElement) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_jsx_element_child(&mut self, n: &JSXElementChild) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_jsx_element_children(&mut self, n: &[JSXElementChild]) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_jsx_element_name(&mut self, n: &JSXElementName) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_jsx_empty_expr(&mut self, n: &JSXEmptyExpr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_jsx_expr(&mut self, n: &JSXExpr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_jsx_expr_container(&mut self, n: &JSXExprContainer) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_jsx_fragment(&mut self, n: &JSXFragment) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_jsx_member_expr(&mut self, n: &JSXMemberExpr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_jsx_namespaced_name(&mut self, n: &JSXNamespacedName) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_jsx_object(&mut self, n: &JSXObject) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_jsx_opening_element(&mut self, n: &JSXOpeningElement) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_jsx_opening_fragment(&mut self, n: &JSXOpeningFragment) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_jsx_spread_child(&mut self, n: &JSXSpreadChild) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_jsx_text(&mut self, n: &JSXText) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_key(&mut self, n: &Key) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_key_value_pat_prop(&mut self, n: &KeyValuePatProp) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_key_value_prop(&mut self, n: &KeyValueProp) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_labeled_stmt(&mut self, n: &LabeledStmt) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_lit(&mut self, n: &Lit) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_member_expr(&mut self, n: &MemberExpr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_member_prop(&mut self, n: &MemberProp) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_meta_prop_expr(&mut self, n: &MetaPropExpr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_meta_prop_kind(&mut self, n: &MetaPropKind) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_method_kind(&mut self, n: &MethodKind) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_method_prop(&mut self, n: &MethodProp) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_module(&mut self, n: &SWCModule) {
        self.scope_builder = Some(ScopeBuilder::new(n));
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_module_decl(&mut self, n: &ModuleDecl) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_module_export_name(&mut self, n: &ModuleExportName) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_module_item(&mut self, n: &ModuleItem) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_module_items(&mut self, n: &[ModuleItem]) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_named_export(&mut self, n: &NamedExport) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_new_expr(&mut self, n: &NewExpr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_null(&mut self, n: &Null) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_number(&mut self, n: &Number) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_object_lit(&mut self, n: &ObjectLit) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_object_pat(&mut self, n: &ObjectPat) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_object_pat_prop(&mut self, n: &ObjectPatProp) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_object_pat_props(&mut self, n: &[ObjectPatProp]) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_opt_accessibility(&mut self, n: Option<&Accessibility>) {
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
    fn visit_opt_block_stmt(&mut self, n: Option<&BlockStmt>) {
        // TODO: Implement analysis visitor.
        n.map(|n| {
            n.visit_with(self);
        });
    }
    fn visit_opt_call(&mut self, n: &OptCall) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_opt_catch_clause(&mut self, n: Option<&CatchClause>) {
        // TODO: Implement analysis visitor.
        n.map(|n| {
            n.visit_with(self);
        });
    }
    fn visit_opt_chain_base(&mut self, n: &OptChainBase) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_opt_chain_expr(&mut self, n: &OptChainExpr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_opt_expr(&mut self, n: Option<&Box<Expr>>) {
        // TODO: Implement analysis visitor.
        n.map(|n| {
            n.visit_with(self);
        });
    }
    fn visit_opt_expr_or_spread(&mut self, n: Option<&ExprOrSpread>) {
        // TODO: Implement analysis visitor.
        n.map(|n| {
            n.visit_with(self);
        });
    }
    fn visit_opt_expr_or_spreads(&mut self, n: Option<&[ExprOrSpread]>) {
        // TODO: Implement analysis visitor.
        n.map(|n| {
            n.visit_with(self);
        });
    }
    fn visit_opt_ident(&mut self, n: Option<&Ident>) {
        // TODO: Implement analysis visitor.
        n.map(|n| {
            n.visit_with(self);
        });
    }
    fn visit_opt_jsx_attr_value(&mut self, n: Option<&JSXAttrValue>) {
        // TODO: Implement analysis visitor.
        n.map(|n| {
            n.visit_with(self);
        });
    }
    fn visit_opt_jsx_closing_element(&mut self, n: Option<&JSXClosingElement>) {
        // TODO: Implement analysis visitor.
        n.map(|n| {
            n.visit_with(self);
        });
    }
    fn visit_opt_module_export_name(&mut self, n: Option<&ModuleExportName>) {
        // TODO: Implement analysis visitor.
        n.map(|n| {
            n.visit_with(self);
        });
    }
    fn visit_opt_module_items(&mut self, n: Option<&[ModuleItem]>) {
        // TODO: Implement analysis visitor.
        n.map(|n| {
            n.visit_with(self);
        });
    }
    fn visit_opt_object_lit(&mut self, n: Option<&Box<ObjectLit>>) {
        // TODO: Implement analysis visitor.
        n.map(|n| {
            n.visit_with(self);
        });
    }
    fn visit_opt_pat(&mut self, n: Option<&Pat>) {
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
    fn visit_opt_stmt(&mut self, n: Option<&Box<Stmt>>) {
        // TODO: Implement analysis visitor.
        n.map(|n| {
            n.visit_with(self);
        });
    }
    fn visit_opt_str(&mut self, n: Option<&Box<Str>>) {
        // TODO: Implement analysis visitor.
        n.map(|n| {
            n.visit_with(self);
        });
    }
    fn visit_opt_true_plus_minus(&mut self, n: Option<&TruePlusMinus>) {
        // TODO: Implement analysis visitor.
        n.map(|n| {
            n.visit_with(self);
        });
    }
    fn visit_opt_ts_entity_name(&mut self, n: Option<&TsEntityName>) {
        // TODO: Implement analysis visitor.
        n.map(|n| {
            n.visit_with(self);
        });
    }
    fn visit_opt_ts_namespace_body(&mut self, n: Option<&TsNamespaceBody>) {
        // TODO: Implement analysis visitor.
        n.map(|n| {
            n.visit_with(self);
        });
    }
    fn visit_opt_ts_type(&mut self, n: Option<&Box<TsType>>) {
        // TODO: Implement analysis visitor.
        n.map(|n| {
            n.visit_with(self);
        });
    }
    fn visit_opt_ts_type_ann(&mut self, n: Option<&Box<TsTypeAnn>>) {
        // TODO: Implement analysis visitor.
        n.map(|n| {
            n.visit_with(self);
        });
    }
    fn visit_opt_ts_type_param_decl(&mut self, n: Option<&Box<TsTypeParamDecl>>) {
        // TODO: Implement analysis visitor.
        n.map(|n| {
            n.visit_with(self);
        });
    }
    fn visit_opt_ts_type_param_instantiation(&mut self, n: Option<&Box<TsTypeParamInstantiation>>) {
        // TODO: Implement analysis visitor.
        n.map(|n| {
            n.visit_with(self);
        });
    }
    fn visit_opt_var_decl_or_expr(&mut self, n: Option<&VarDeclOrExpr>) {
        // TODO: Implement analysis visitor.
        n.map(|n| {
            n.visit_with(self);
        });
    }
    fn visit_opt_vec_expr_or_spreads(&mut self, n: &[Option<ExprOrSpread>]) {
        // TODO: Implement analysis visitor.
        n.iter().for_each(|n| {
            n.as_ref().map(|n| {
                n.visit_children_with(self);
            });
        })
    }
    fn visit_opt_vec_pats(&mut self, n: &[Option<Pat>]) {
        // TODO: Implement analysis visitor.
        n.iter().for_each(|n| {
            n.as_ref().map(|n| {
                n.visit_children_with(self);
            });
        })
    }
    fn visit_param(&mut self, n: &Param) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_param_or_ts_param_prop(&mut self, n: &ParamOrTsParamProp) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_param_or_ts_param_props(&mut self, n: &[ParamOrTsParamProp]) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_params(&mut self, n: &[Param]) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_paren_expr(&mut self, n: &ParenExpr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_pat(&mut self, n: &Pat) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_pat_or_expr(&mut self, n: &PatOrExpr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_pats(&mut self, n: &[Pat]) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_private_method(&mut self, n: &PrivateMethod) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_private_name(&mut self, n: &PrivateName) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_private_prop(&mut self, n: &PrivateProp) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_program(&mut self, n: &Program) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_prop(&mut self, n: &Prop) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_prop_name(&mut self, n: &PropName) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_prop_or_spread(&mut self, n: &PropOrSpread) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_prop_or_spreads(&mut self, n: &[PropOrSpread]) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_regex(&mut self, n: &Regex) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_reserved_unused(&mut self, n: &ReservedUnused) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_rest_pat(&mut self, n: &RestPat) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_return_stmt(&mut self, n: &ReturnStmt) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_script(&mut self, n: &Script) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_seq_expr(&mut self, n: &SeqExpr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_setter_prop(&mut self, n: &SetterProp) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_span(&mut self, n: &Span) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_spread_element(&mut self, n: &SpreadElement) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_static_block(&mut self, n: &StaticBlock) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_stmt(&mut self, n: &Stmt) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_stmts(&mut self, n: &[Stmt]) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_str(&mut self, n: &Str) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_super(&mut self, n: &Super) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_super_prop(&mut self, n: &SuperProp) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_super_prop_expr(&mut self, n: &SuperPropExpr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_switch_case(&mut self, n: &SwitchCase) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_switch_cases(&mut self, n: &[SwitchCase]) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_switch_stmt(&mut self, n: &SwitchStmt) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_tagged_tpl(&mut self, n: &TaggedTpl) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_this_expr(&mut self, n: &ThisExpr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_throw_stmt(&mut self, n: &ThrowStmt) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_tpl(&mut self, n: &Tpl) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_tpl_element(&mut self, n: &TplElement) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_tpl_elements(&mut self, n: &[TplElement]) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_true_plus_minus(&mut self, n: &TruePlusMinus) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_try_stmt(&mut self, n: &TryStmt) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_array_type(&mut self, n: &TsArrayType) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_as_expr(&mut self, n: &TsAsExpr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_call_signature_decl(&mut self, n: &TsCallSignatureDecl) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_conditional_type(&mut self, n: &TsConditionalType) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_const_assertion(&mut self, n: &TsConstAssertion) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_construct_signature_decl(&mut self, n: &TsConstructSignatureDecl) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_constructor_type(&mut self, n: &TsConstructorType) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_entity_name(&mut self, n: &TsEntityName) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_enum_decl(&mut self, n: &TsEnumDecl) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_enum_member(&mut self, n: &TsEnumMember) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_enum_member_id(&mut self, n: &TsEnumMemberId) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_enum_members(&mut self, n: &[TsEnumMember]) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_export_assignment(&mut self, n: &TsExportAssignment) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_expr_with_type_args(&mut self, n: &TsExprWithTypeArgs) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_expr_with_type_args_vec(&mut self, n: &[TsExprWithTypeArgs]) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_external_module_ref(&mut self, n: &TsExternalModuleRef) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_fn_or_constructor_type(&mut self, n: &TsFnOrConstructorType) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_fn_param(&mut self, n: &TsFnParam) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_fn_params(&mut self, n: &[TsFnParam]) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_fn_type(&mut self, n: &TsFnType) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_getter_signature(&mut self, n: &TsGetterSignature) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_import_equals_decl(&mut self, n: &TsImportEqualsDecl) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_import_type(&mut self, n: &TsImportType) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_index_signature(&mut self, n: &TsIndexSignature) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_indexed_access_type(&mut self, n: &TsIndexedAccessType) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_infer_type(&mut self, n: &TsInferType) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_instantiation(&mut self, n: &TsInstantiation) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_interface_body(&mut self, n: &TsInterfaceBody) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_interface_decl(&mut self, n: &TsInterfaceDecl) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_intersection_type(&mut self, n: &TsIntersectionType) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_keyword_type(&mut self, n: &TsKeywordType) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_keyword_type_kind(&mut self, n: &TsKeywordTypeKind) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_lit(&mut self, n: &TsLit) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_lit_type(&mut self, n: &TsLitType) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_mapped_type(&mut self, n: &TsMappedType) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_method_signature(&mut self, n: &TsMethodSignature) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_module_block(&mut self, n: &TsModuleBlock) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_module_decl(&mut self, n: &TsModuleDecl) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_module_name(&mut self, n: &TsModuleName) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_module_ref(&mut self, n: &TsModuleRef) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_namespace_body(&mut self, n: &TsNamespaceBody) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_namespace_decl(&mut self, n: &TsNamespaceDecl) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_namespace_export_decl(&mut self, n: &TsNamespaceExportDecl) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_non_null_expr(&mut self, n: &TsNonNullExpr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_optional_type(&mut self, n: &TsOptionalType) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_param_prop(&mut self, n: &TsParamProp) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_param_prop_param(&mut self, n: &TsParamPropParam) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_parenthesized_type(&mut self, n: &TsParenthesizedType) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_property_signature(&mut self, n: &TsPropertySignature) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_qualified_name(&mut self, n: &TsQualifiedName) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_rest_type(&mut self, n: &TsRestType) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_satisfies_expr(&mut self, n: &TsSatisfiesExpr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_setter_signature(&mut self, n: &TsSetterSignature) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_this_type(&mut self, n: &TsThisType) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_this_type_or_ident(&mut self, n: &TsThisTypeOrIdent) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_tpl_lit_type(&mut self, n: &TsTplLitType) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_tuple_element(&mut self, n: &TsTupleElement) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_tuple_elements(&mut self, n: &[TsTupleElement]) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_tuple_type(&mut self, n: &TsTupleType) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_type(&mut self, n: &TsType) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_type_alias_decl(&mut self, n: &TsTypeAliasDecl) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_type_ann(&mut self, n: &TsTypeAnn) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_type_assertion(&mut self, n: &TsTypeAssertion) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_type_element(&mut self, n: &TsTypeElement) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_type_elements(&mut self, n: &[TsTypeElement]) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_type_lit(&mut self, n: &TsTypeLit) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_type_operator(&mut self, n: &TsTypeOperator) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_type_operator_op(&mut self, n: &TsTypeOperatorOp) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_type_param(&mut self, n: &TsTypeParam) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_type_param_decl(&mut self, n: &TsTypeParamDecl) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_type_param_instantiation(&mut self, n: &TsTypeParamInstantiation) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_type_params(&mut self, n: &[TsTypeParam]) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_type_predicate(&mut self, n: &TsTypePredicate) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_type_query(&mut self, n: &TsTypeQuery) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_type_query_expr(&mut self, n: &TsTypeQueryExpr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_type_ref(&mut self, n: &TsTypeRef) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_types(&mut self, n: &[Box<TsType>]) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_union_or_intersection_type(&mut self, n: &TsUnionOrIntersectionType) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_ts_union_type(&mut self, n: &TsUnionType) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_unary_expr(&mut self, n: &UnaryExpr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_unary_op(&mut self, n: &UnaryOp) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_update_expr(&mut self, n: &UpdateExpr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_update_op(&mut self, n: &UpdateOp) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_var_decl(&mut self, n: &VarDecl) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_var_decl_kind(&mut self, n: &VarDeclKind) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_var_decl_or_expr(&mut self, n: &VarDeclOrExpr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_var_decl_or_pat(&mut self, n: &VarDeclOrPat) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_var_declarator(&mut self, n: &VarDeclarator) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_var_declarators(&mut self, n: &[VarDeclarator]) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_while_stmt(&mut self, n: &WhileStmt) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
    fn visit_with_stmt(&mut self, n: &WithStmt) {
        self.scope_builder().push(n);
        n.visit_children_with(self);
        self.scope_builder().pop();
    }
    fn visit_yield_expr(&mut self, n: &YieldExpr) {
        // TODO: Implement analysis visitor.
        n.visit_children_with(self)
    }
}
