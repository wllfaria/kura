use kura_lexer::Lexer;
use kura_parser::Parser;
use kura_typer::Typer;

fn main() -> Result<(), std::io::Error> {
    loop {
        let mut command = String::new();
        std::io::stdin().read_line(&mut command)?;
        if command.contains("quit") {
            break Ok(());
        }
        let lexer = Lexer::new(&command);
        let parser = Parser::new(&command, lexer);
        let statements = parser.parse().unwrap();
        let tc = Typer::new(statements);
        let typed_statements = tc.type_check().unwrap();
        println!("{typed_statements}");
    }
}
