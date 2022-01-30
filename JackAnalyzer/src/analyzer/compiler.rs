use super::ast::{Node, Program};
use std::io::{BufWriter, Write};

// TODO: consider changing this struct's name
pub struct CompilationEngine<'a, T: Write> {
    buf_writer: &'a mut BufWriter<T>,
}

impl<'a, T: Write> CompilationEngine<'a, T> {
    pub fn new(buf_writer: &'a mut BufWriter<T>) -> Self {
        CompilationEngine { buf_writer }
    }

    pub fn compile(&mut self, program_ast: Program) {
        let xml = program_ast.to_xml();
        xml.write(&mut self.buf_writer)
            .expect("failed to write xml");
    }
}
