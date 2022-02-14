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

pub struct Subroutine {
    class_name: String,
    subroutine_name: String,
    local_var_count: usize,
    commands: Vec<Command>,
}

impl Subroutine {
    pub fn new<S: Into<String>>(
        class_name: S,
        subroutine_name: S,
        local_var_count: usize,
        commands: Vec<Command>,
    ) -> Self {
        Self {
            class_name: class_name.into(),
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

        let this_config = format!(
            "{}\n{}",
            Command::Push(Segment::Arg, 0).compile(),
            Command::Pop(Segment::Pointer, 0).compile()
        );

        let commands = self
            .commands
            .iter()
            .map(|c| c.compile())
            .collect::<Vec<String>>()
            .join("\n");

        format!("{}\n{}\n{}\n", define, this_config, commands)
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
    None,
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
            None => "",
        };
        s.to_string()
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
enum ArthmeticCommand {
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
    Call(Option<String>, String, usize),
    Return,
}

impl Command {
    fn compile(&self) -> String {
        use Command::*;
        fn config_seg_and_index(command: &str, segment: &Segment, index: &usize) -> String {
            format!(
                "{}{} {}",
                command,
                match segment {
                    Segment::None => "".to_string(),
                    seg => format!(" {}", seg.to_string()),
                },
                index
            )
        };

        match self {
            Push(segment, index) => config_seg_and_index("push", segment, index),
            Pop(segment, index) => config_seg_and_index("pop", segment, index),
            Arthmetic(command) => command.to_string(),
            Call(parent_name, subroutin_name, local_var_count) => {
                let parent_name = match parent_name {
                    Some(name) => format!("{}.", name),
                    None => "".to_string(),
                };
                format!("call {}{} {}", parent_name, subroutin_name, local_var_count)
            }
            Return => "return".to_string(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::analyzer::vm::*;
    //use std::io::Cursor;

    #[test]
    fn simple_main_class() {
        let class = VmClass::new(vec![Subroutine::new(
            "Main",
            "main",
            0,
            vec![
                Command::Push(Segment::Const, 1),
                Command::Push(Segment::Const, 2),
                Command::Push(Segment::Const, 3),
                Command::Call(Some("Math".to_string()), "multiply".to_string(), 2),
                Command::Arthmetic(ArthmeticCommand::Add),
                Command::Call(Some("Output".to_string()), "printInt".to_string(), 1),
                Command::Push(Segment::None, 0),
                Command::Return,
            ],
        )]);

        let actual = class.compile();

        assert_eq!(
            "function Main.main 0
push argument 0
pop pointer 0
push constant 1
push constant 2
push constant 3
call Math.multiply 2
add
call Output.printInt 1
push 0
return
",
            actual
        );
    }
}
