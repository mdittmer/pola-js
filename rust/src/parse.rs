use std::fmt::Debug;
use std::path::Path;

use color_eyre::Report;
use color_eyre::Result;
use color_eyre::Section;
use eyre::eyre;
use eyre::WrapErr;
use swc_common::errors::ColorConfig;
use swc_common::errors::Handler;
use swc_common::sync::Lrc;
use swc_common::FileName;
use swc_common::Mark;
use swc_common::SourceFile;
use swc_common::SourceMap;
use swc_common::GLOBALS;
use swc_ecma_ast::EsVersion;
use swc_ecma_ast::Module as SWCModule;
use swc_ecma_parser::error::SyntaxError;
use swc_ecma_parser::lexer::Lexer;
use swc_ecma_parser::Parser;
use swc_ecma_parser::StringInput;
use swc_ecma_parser::Syntax;
use swc_ecma_transforms_base::hygiene::hygiene_with_config;
use swc_ecma_transforms_base::hygiene::Config;
use swc_ecma_transforms_base::resolver;
use swc_ecma_visit::VisitMutWith;
use thiserror::Error;

#[derive(Debug)]
pub struct Module {
    pub swc_module: SWCModule,
    pub hygiene_top_level_mark: Mark,
    pub resolver_top_level_mark: Mark,
    pub unresolved_mark: Mark,
}

#[tracing::instrument(skip(source_path), fields(path = source_path.as_ref().to_string_lossy().to_string()))]
pub fn parse_file<P: AsRef<Path>>(
    source_path: P,
    syntax: Syntax,
    target: EsVersion,
) -> Result<Module> {
    let source_map: Lrc<SourceMap> = Default::default();
    let source_file = source_map.load_file(source_path.as_ref())?;
    parse_module(source_map, source_file, syntax, target, None)
        .with_context(|| format!("parsing file {:?}", source_path.as_ref()))
}

#[tracing::instrument(skip(source_path, handler), fields(path = source_path.as_ref().to_string_lossy().to_string()))]
pub fn parse_file_with_handler<P: AsRef<Path>>(
    source_path: P,
    syntax: Syntax,
    target: EsVersion,
    handler: Handler,
) -> Result<Module> {
    let source_map: Lrc<SourceMap> = Default::default();
    let source_file = source_map.load_file(source_path.as_ref())?;
    parse_module(source_map, source_file, syntax, target, Some(handler))
        .with_context(|| format!("parsing file {:?}", source_path.as_ref()))
}

#[tracing::instrument(skip(source_str))]
pub fn parse_str(
    source_str: &str,
    source_path: &str,
    syntax: Syntax,
    target: EsVersion,
) -> Result<Module> {
    let source_map: Lrc<SourceMap> = Default::default();
    let source_file =
        source_map.new_source_file(FileName::Custom(source_path.into()), source_str.into());
    parse_module(source_map, source_file, syntax, target, None)
        .with_context(|| format!("parsing string as path {:?}", source_path))
}

#[tracing::instrument(skip(handler))]
pub fn parse_str_with_handler(
    source_str: &str,
    source_path: &str,
    syntax: Syntax,
    target: EsVersion,
    handler: Handler,
) -> Result<Module> {
    let source_map: Lrc<SourceMap> = Default::default();
    let source_file =
        source_map.new_source_file(FileName::Custom(source_path.into()), source_str.into());
    parse_module(source_map, source_file, syntax, target, Some(handler))
        .with_context(|| format!("parsing string as path {:?}", source_path))
}

pub fn parse_module(
    source_map: Lrc<SourceMap>,
    source_file: Lrc<SourceFile>,
    syntax: Syntax,
    target: EsVersion,
    handler: Option<Handler>,
) -> Result<Module> {
    let emit_diagnostics = handler.is_some();
    let handler = handler.unwrap_or_else(|| default_handler(&source_map));

    let lexer = Lexer::new(
        syntax,
        target,
        StringInput::from(&*source_file),
        // Do not store comments.
        None,
    );
    let mut parser = Parser::new_from(lexer);

    let errors: Vec<ParseError> = parser
        .take_errors()
        .into_iter()
        .map(|error| {
            let error = if emit_diagnostics {
                error.clone().into_diagnostic(&handler).emit();
                error
            } else {
                error
            };
            ParseError(error.into_kind())
        })
        .collect();
    if errors.len() > 0 {
        return Err(join_errors(errors));
    }

    let mut swc_module = parser.parse_module().map_err(|error| {
        let error = if emit_diagnostics {
            error.clone().into_diagnostic(&handler).emit();
            error
        } else {
            error
        };
        ParseError(error.into_kind())
    })?;

    Ok(GLOBALS.set(&Default::default(), || {
        let hygiene_top_level_mark = Mark::new();
        let resolver_top_level_mark = Mark::new();
        let unresolved_mark = Mark::new();

        swc_module.visit_mut_with(&mut hygiene_with_config(Config {
            top_level_mark: hygiene_top_level_mark,
            ..Default::default()
        }));
        swc_module.visit_mut_with(&mut resolver(
            unresolved_mark,
            resolver_top_level_mark,
            false,
        ));

        Module {
            swc_module,
            hygiene_top_level_mark,
            resolver_top_level_mark,
            unresolved_mark,
        }
    }))
}

fn default_handler(source_map: &Lrc<SourceMap>) -> Handler {
    Handler::with_tty_emitter(ColorConfig::Auto, true, false, Some(source_map.clone()))
}

#[derive(Debug, Error)]
#[error("swc parse error: {0:?}")]
struct ParseError(SyntaxError);

fn join_errors<E: std::error::Error + Send + Sync + 'static>(results: Vec<E>) -> Report {
    results
        .into_iter()
        .fold(eyre!("encountered multiple errors"), |report, error| {
            report.error(error)
        })
}

#[cfg(test)]
mod tests {
    use super::parse_file;
    use super::parse_str;
    use color_eyre::Result;
    use swc_ecma_parser::Syntax;

    /// Path to sandbox testdata, realtive to test runner path.
    const SANDBOX_TS_PATH: &str = "testdata/sandbox.ts";

    /// Contents of sandbox test data.
    const SANDBOX_TS_STR: &str = include_str!("../testdata/sandbox.ts");

    #[test]
    fn test_sandbox_ts() -> Result<()> {
        parse_str(
            SANDBOX_TS_STR,
            SANDBOX_TS_PATH,
            Syntax::Typescript(Default::default()),
            Default::default(),
        )?;
        parse_file(
            SANDBOX_TS_PATH,
            Syntax::Typescript(Default::default()),
            Default::default(),
        )?;
        Ok(())
    }
}
