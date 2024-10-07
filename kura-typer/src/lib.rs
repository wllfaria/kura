use kura_parser::Statement;

#[derive(Debug)]
pub struct Typer<'typer> {
    pub statements: Vec<Statement<'typer>>,
}

impl<'typer> Typer<'typer> {
    pub fn new(statements: Vec<Statement<'typer>>) -> Self {
        Self { statements }
    }

    pub fn type_check(mut self) -> Result<&'static str, String> {
        for statement in self.statements.iter_mut() {
            match statement {
                Statement::Fun { .. } => {}
                Statement::FunArgument { .. } => {}
            }
        }

        Ok("lol")
    }
}
