use std::fmt::Write;

pub struct ASTWriter<W: Write + ToString>
{
    output: W,
    indent: usize,
}

impl<W: Write + ToString> ASTWriter<W>
{
    pub fn new(output: W) -> Self
    {
        ASTWriter { output, indent: 0 }
    }

    pub fn indent(&mut self) -> &mut Self
    {
        self.increase_indent();
        self
    }

    pub fn dedent(&mut self) -> &mut Self
    {
        self.decrease_indent();
        self
    }

    /// Chainable shorthand for `write_line`
    pub fn wrtln(&mut self, string: &str) -> &mut Self
    {
        self.write_line(string);
        self
    }

    /// Chainable shorthand for `write`
    pub fn wrt(&mut self, string: &str) -> &mut Self
    {
        self.write(string);
        self
    }

    pub fn increase_indent(&mut self)
    {
        self.indent += 1
    }

    pub fn write_then_increase_indent(&mut self, string: &str)
    {
        self.write_line(string);
        self.indent += 1
    }

    pub fn decrease_indent(&mut self)
    {
        self.indent -= 1;
    }

    pub fn decrease_indent_then_write(&mut self, string: &str)
    {
        self.indent -= 1;
        self.write_line(string);
    }

    pub fn write_line(&mut self, string: &str)
    {
        let indent = " ".repeat(self.indent * 4);

        let string = string.replace("\n", &("\n".to_string() + &indent));

        writeln!(&mut self.output, "{}{}", &indent, &string);
    }

    pub fn write(&mut self, string: &str)
    {
        let indent = " ".repeat(self.indent * 4);

        let string = string.replace("\n", &("\n".to_string() + &indent));

        write!(&mut self.output, "{}", &string);
    }

    pub fn into_string(self) -> String
    {
        self.output.to_string()
    }

    pub fn checkpoint(&self) -> ASTWriter<String>
    {
        ASTWriter {
            output: String::new(),
            indent: self.indent,
        }
    }

    pub fn merge_checkpoint(&mut self, checkpoint: ASTWriter<String>)
    {
        write!(&mut self.output, "{}", &checkpoint.output);
    }
}
