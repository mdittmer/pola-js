use color_eyre::eyre::Result;
use pola_js::analyze::analyze_module;
use pola_js::parse::parse_str;
use swc_common::GLOBALS;
use swc_ecma_parser::Syntax;

const SANDBOX_TS_STR: &str = include_str!("../testdata/sandbox.ts");

fn main() -> Result<()> {
    color_eyre::install()?;

    GLOBALS.set(&Default::default(), || {
        let parsed_module = parse_str(
            SANDBOX_TS_STR,
            "sandbox.ts",
            Syntax::Typescript(Default::default()),
            Default::default(),
        )
        .expect("parsed module");

        // println!("Parsed module");
        // println!("{:#?}", parsed_module);

        let analysis_result = analyze_module(&parsed_module).expect("analysis result");
        println!("Analysis result");
        println!("{:#?}", analysis_result);
    });

    Ok(())
}
