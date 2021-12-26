#[derive(PartialEq, Debug)]
pub enum CommandType {
    // binary arithmetic
    Add,
    Sub,
    Eq,
    Gt,
    Lt,
    And,
    Or,
    // unary arithmetic
    Neg,
    Not,
    // memory access
    CPush(Segment, usize),
}

impl CommandType {
    pub fn from_command(command: &str) -> Self {
        let trimed = command[..command.find("//").unwrap_or(command.len())].trim();

        match trimed {
            "add" => CommandType::Add,
            "sub" => CommandType::Sub,
            "eq" => CommandType::Eq,
            "gt" => CommandType::Gt,
            "lt" => CommandType::Lt,
            "and" => CommandType::And,
            "or" => CommandType::Or,
            "neg" => CommandType::Neg,
            "not" => CommandType::Not,
            trimed if trimed.starts_with("push") => {
                let (_, params) = trimed.split_once(" ").unwrap();
                let (segment, index) = params.split_once(" ").unwrap();
                CommandType::CPush(
                    Segment::from_command(segment),
                    index.parse::<usize>().unwrap(),
                )
            }
            _ => panic!("unexpected command type {}", command),
        }
    }

    pub fn to_binary_code(&self) -> String {
        match self {
            CommandType::Add => {
                "@SP\nM=M-1\nA=M\nD=M\n@SP\nM=M-1\nA=M\nM=D+M\n@SP\nM=M+1".to_string()
            }
            CommandType::CPush(Segment::Constant, index) => {
                format!("@{}\nD=A\n@SP\nA=M\nM=D\n@SP\nM=M+1\n", index)
            }
            _ => panic!("unexpected command"),
        }
    }
}

#[derive(PartialEq, Debug)]
pub enum Segment {
    Argument,
    Local,
    Static,
    Constant,
    This,
    That,
    Pointer,
    Temp,
}

impl Segment {
    pub fn from_command(segment_str: &str) -> Self {
        let trimed = segment_str.trim();

        match trimed {
            "argument" => Segment::Argument,
            "local" => Segment::Local,
            "static" => Segment::Static,
            "constant" => Segment::Constant,
            "this" => Segment::This,
            "that" => Segment::That,
            "pointer" => Segment::Pointer,
            "temp" => Segment::Temp,
            _ => panic!("unexpected segment {}", trimed),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::translator::command_type::*;

    #[test]
    fn push_constant() {
        let actual = CommandType::from_command("push constant 7");
        assert_eq!(actual, CommandType::CPush(Segment::Constant, 7));
    }

    #[test]
    fn add() {
        let actual = CommandType::from_command("add");
        assert_eq!(actual, CommandType::Add);
    }

    #[test]
    fn trim_comments() {
        let actual = CommandType::from_command("push constant 7 // this is comments");
        assert_eq!(actual, CommandType::CPush(Segment::Constant, 7));
    }
}
