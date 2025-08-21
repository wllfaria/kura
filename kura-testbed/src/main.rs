use std::sync::Arc;

fn main() -> miette::Result<()> {
    let file = include_str!("../../samples/sample-01.kura").to_string();
    let file_arc = Arc::new(file);

    let lexer = kura_lexer::Lexer::new(file_arc.as_ref(), file_arc.clone());
    let ast = kura_parser::Parser::new(file_arc.as_ref(), lexer).parse()?;
    let typed_ast = kura_typer::Typer::new(file_arc.clone()).check(ast)?;
    println!("{typed_ast:#?}");

    Ok(())
}
