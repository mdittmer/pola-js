use color_eyre::eyre::Result;
use pola_js::analyze::analyze_str;
use pola_js::parse::parse_str;
use swc_common::GLOBALS;
use swc_ecma_parser::Syntax;

const SANDBOX_TS_STR: &str = include_str!("../testdata/sandbox.ts");

fn main() -> Result<()> {
    color_eyre::install()?;

    GLOBALS.set(&Default::default(), || {
        println!("Parse");
        println!(
            "{:#?}",
            parse_str(
                SANDBOX_TS_STR,
                "sandbox.ts",
                Syntax::Typescript(Default::default()),
                Default::default(),
            )
        );

        println!("Analyze");
        println!(
            "{:#?}",
            analyze_str(
                SANDBOX_TS_STR,
                "sandbox.ts",
                Syntax::Typescript(Default::default()),
                Default::default(),
            )
        );
    });

    Ok(())
}
