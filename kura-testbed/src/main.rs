use std::sync::Arc;

fn main() -> miette::Result<()> {
    let file = include_str!("../../samples/sample-01.kura").to_string();
    let file_arc = Arc::new(file);

    let lexer = kura_lexer::Lexer::new(file_arc.as_ref(), file_arc.clone());
    let parser = kura_parser::Parser::new(file_arc.as_ref(), lexer);
    let ast = parser.parse()?;
    let mut typer = kura_typer::Typer::new();
    typer.typecheck_ast(ast);

    Ok(())
}
