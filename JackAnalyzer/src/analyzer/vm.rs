use super::symbol_table::{ClassAttribute, LocalAttribute};
use std::io::{self, Write};

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct VmClass {
    subroutines: Vec<Subroutine>,
}
impl VmClass {
    pub fn new(subroutines: Vec<Subroutine>) -> Self {
        Self { subroutines }
    }

    pub fn compile(&self) -> String {
        self.subroutines
            .iter()
            .map(|s| s.compile())
            .collect::<Vec<String>>()
            .join("\n")
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum SubroutineType {
    Constructor(usize),
    Method,
    Function,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Subroutine {
    class_name: String,
    subroutine_type: SubroutineType,
    subroutine_name: String,
    local_var_count: usize,
    commands: Vec<Command>,
}

impl Subroutine {
    pub fn new<S: Into<String>>(
        class_name: S,
        subroutine_type: SubroutineType,
        subroutine_name: S,
        local_var_count: usize,
        commands: Vec<Command>,
    ) -> Self {
        Self {
            class_name: class_name.into(),
            subroutine_type,
            subroutine_name: subroutine_name.into(),
            local_var_count,
            commands,
        }
    }

    fn compile(&self) -> String {
        let define = format!(
            "function {}.{} {}",
            self.class_name, self.subroutine_name, self.local_var_count
        );

        use SubroutineType::*;
        let this_config = match self.subroutine_type {
            Constructor(field_count) => Some(format!(
                "{}\n{}\n{}",
                Command::Push(Segment::Const, field_count).compile(),
                "call Memory.alloc 1",
                Command::Pop(Segment::Pointer, 0).compile()
            )),
            Method => Some(format!(
                "{}\n{}",
                Command::Push(Segment::Arg, 0).compile(),
                Command::Pop(Segment::Pointer, 0).compile()
            )),
            Function => None,
        };

        let commands = self
            .commands
            .iter()
            .map(|c| c.compile())
            .collect::<Vec<String>>()
            .join("\n");

        match this_config {
            Some(this_config) => format!("{}\n{}\n{}", define, this_config, commands),
            None => format!("{}\n{}", define, commands),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Segment {
    Const,
    Arg,
    Local,
    Static,
    This,
    That,
    Pointer,
    Temp,
}
impl ToString for Segment {
    fn to_string(&self) -> String {
        use Segment::*;
        let s = match self {
            Const => "constant",
            Arg => "argument",
            Local => "local",
            Static => "static",
            This => "this",
            That => "that",
            Pointer => "pointer",
            Temp => "temp",
        };
        s.to_string()
    }
}
impl Segment {
    pub fn from_class_attr(class_attr: &ClassAttribute) -> Self {
        match class_attr {
            ClassAttribute::Static | ClassAttribute::Function => Segment::Static,
            ClassAttribute::Field | ClassAttribute::Constructor | ClassAttribute::Method => {
                Segment::This
            }
        }
    }

    pub fn from_local_attr(local_attr: &LocalAttribute) -> Self {
        match local_attr {
            LocalAttribute::Argument => Segment::Arg,
            LocalAttribute::Var => Segment::Local,
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum ArthmeticCommand {
    Add,
    Sub,
    Neg,
    Eq,
    Gt,
    Lt,
    And,
    Or,
    Not,
}

impl ToString for ArthmeticCommand {
    fn to_string(&self) -> String {
        use ArthmeticCommand::*;
        let s = match self {
            Add => "add",
            Sub => "sub",
            Neg => "neg",
            Eq => "eq",
            Gt => "gt",
            Lt => "lt",
            And => "and",
            Or => "or",
            Not => "not",
        };
        s.to_string()
    }
}

type Index = usize;

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Command {
    Push(Segment, Index),
    Pop(Segment, Index),
    Arthmetic(ArthmeticCommand),
    Label(String),
    GoTo(String),
    If(String),
    Call(String, String, usize),
    Return,
}

impl Command {
    fn compile(&self) -> String {
        use Command::*;
        fn config_seg_and_index(command: &str, segment: &Segment, index: &usize) -> String {
            format!(
                "{}{} {}",
                command,
                format!(" {}", segment.to_string()),
                index
            )
        }

        match self {
            Push(segment, index) => config_seg_and_index("push", segment, index),
            Pop(segment, index) => config_seg_and_index("pop", segment, index),
            Arthmetic(command) => command.to_string(),
            Label(label) => format!("label {}", label),
            GoTo(label) => format!("goto {}", label),
            If(label) => format!("if-goto {}", label),
            Call(parent_name, subroutin_name, local_var_count) => {
                format!(
                    "call {}{} {}",
                    format!("{}.", parent_name),
                    subroutin_name,
                    local_var_count
                )
            }
            Return => "return".to_string(),
        }
    }
}

pub trait VmWriter {
    fn write_vm(&mut self, vm: &VmClass) -> io::Result<()>;
}

impl<W: Write> VmWriter for W {
    fn write_vm(&mut self, vm: &VmClass) -> io::Result<()> {
        writeln!(self, "{}", vm.compile())?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::analyzer::vm::*;

    #[test]
    fn simple_main_class() {
        let class = VmClass::new(vec![
            Subroutine::new(
                "Main",
                SubroutineType::Constructor(2),
                "new",
                0,
                vec![Command::Push(Segment::Pointer, 0), Command::Return],
            ),
            Subroutine::new(
                "Main",
                SubroutineType::Method,
                "method",
                0,
                vec![Command::Push(Segment::Const, 0), Command::Return],
            ),
            Subroutine::new(
                "Main",
                SubroutineType::Function,
                "main",
                0,
                vec![
                    Command::Push(Segment::Const, 1),
                    Command::Push(Segment::Const, 2),
                    Command::Push(Segment::Const, 3),
                    Command::Call("Math".to_string(), "multiply".to_string(), 2),
                    Command::Arthmetic(ArthmeticCommand::Add),
                    Command::Call("Output".to_string(), "printInt".to_string(), 1),
                    Command::Push(Segment::Const, 0),
                    Command::Return,
                ],
            ),
        ]);

        let actual = class.compile();

        assert_eq!(
            "function Main.new 0
push constant 2
call Memory.alloc 1
pop pointer 0
push pointer 0
return
function Main.method 0
push argument 0
pop pointer 0
push constant 0
return
function Main.main 0
push constant 1
push constant 2
push constant 3
call Math.multiply 2
add
call Output.printInt 1
push constant 0
return",
            actual
        );
    }

    #[test]
    fn if_else() {
        let class = VmClass::new(vec![Subroutine::new(
            "Main",
            SubroutineType::Function,
            "main",
            0,
            vec![
                Command::Push(Segment::Const, 1),
                Command::Push(Segment::Const, 2),
                Command::Arthmetic(ArthmeticCommand::Lt),
                Command::If("IF_TRUE".to_string()),
                Command::GoTo("IF_FALSE".to_string()),
                Command::Label("IF_TRUE".to_string()),
                Command::Push(Segment::Const, 1),
                Command::Return,
                Command::Label("IF_FALSE".to_string()),
                Command::Push(Segment::Const, 0),
                Command::Return,
            ],
        )]);

        let actual = class.compile();

        assert_eq!(
            "function Main.main 0
push constant 1
push constant 2
lt
if-goto IF_TRUE
goto IF_FALSE
label IF_TRUE
push constant 1
return
label IF_FALSE
push constant 0
return",
            actual
        );
    }
}
