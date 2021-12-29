use uuid::Uuid;

#[derive(PartialEq, Debug)]
pub enum CommandType {
    BinaryArithmetic(BinaryArithmetic),
    UnaryArithmetic(UnaryArithmetic),
    CPush(Segment, usize),
    CPop(Segment, usize),
    Blank,
}

impl CommandType {
    pub fn from_command(command: &str) -> Self {
        let trimed = command[..command.find("//").unwrap_or(command.len())].trim();

        match trimed {
            "add" => CommandType::BinaryArithmetic(BinaryArithmetic::Add),
            "sub" => CommandType::BinaryArithmetic(BinaryArithmetic::Sub),
            "eq" => CommandType::BinaryArithmetic(BinaryArithmetic::Eq),
            "gt" => CommandType::BinaryArithmetic(BinaryArithmetic::Gt),
            "lt" => CommandType::BinaryArithmetic(BinaryArithmetic::Lt),
            "and" => CommandType::BinaryArithmetic(BinaryArithmetic::And),
            "or" => CommandType::BinaryArithmetic(BinaryArithmetic::Or),
            "neg" => CommandType::UnaryArithmetic(UnaryArithmetic::Neg),
            "not" => CommandType::UnaryArithmetic(UnaryArithmetic::Not),
            trimed if trimed.starts_with("push") => {
                let (_, params) = trimed.split_once(" ").unwrap();
                let (segment, index) = params.split_once(" ").unwrap();
                CommandType::CPush(
                    Segment::from_command(segment),
                    index.parse::<usize>().unwrap(),
                )
            }
            trimed if trimed.starts_with("pop") => {
                let (_, params) = trimed.split_once(" ").unwrap();
                let (segment, index) = params.split_once(" ").unwrap();
                CommandType::CPop(
                    Segment::from_command(segment),
                    index.parse::<usize>().unwrap(),
                )
            }
            _ => panic!("unexpected command type {}", command),
        }
    }

    pub fn to_assembly_code(&self) -> String {
        match self {
            CommandType::BinaryArithmetic(binary) => binary.to_assembly_code(),
            CommandType::UnaryArithmetic(unary) => unary.to_assembly_code(),
            CommandType::CPush(segment, index) => match segment {
                Segment::Constant => format!("@{}\nD=A\n@SP\nA=M\nM=D\n@SP\nM=M+1\n", index),
                segment => format!(
                    "{}\n@{}\nA=D+A\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n",
                    segment.get_ram_base_address_command(),
                    index
                ),
            },
            CommandType::CPop(segment, index) => format!(
                "{}\n@{}\nD=D+A\n@R13\nM=D\n@SP\nM=M-1\n@SP\nA=M\nD=M\nM=0\n@13\nA=M\nM=D\n",
                segment.get_ram_base_address_command(),
                index
            ),
            CommandType::Blank => "".to_string(),
            _ => panic!("unexpected command"),
        }
    }
}

#[derive(PartialEq, Debug)]
pub enum BinaryArithmetic {
    Add,
    Sub,
    Eq,
    Gt,
    Lt,
    And,
    Or,
}

impl BinaryArithmetic {
    pub fn to_assembly_code(&self) -> String {
        let specific_command = match self {
            BinaryArithmetic::Add => "M=D+M".to_string(),
            BinaryArithmetic::Sub => "M=M-D".to_string(),
            BinaryArithmetic::And => "M=D&M".to_string(),
            BinaryArithmetic::Or => "M=D|M".to_string(),
            logical_arithmetic => {
                let specific_command = match logical_arithmetic {
                    BinaryArithmetic::Eq => "D;JEQ",
                    BinaryArithmetic::Gt => "D;JLT",
                    BinaryArithmetic::Lt => "D;JGT",
                    _ => panic!("unreachable statement of BynaryArithmetic"),
                };
                let uuid = Uuid::new_v4();
                format!(
                    "D=D-M\n@SP\nA=M\nM=-1\n@{}\n{}\n@SP\nA=M\nM=0\n({})",
                    uuid, specific_command, uuid
                )
            }
        };

        format!(
            "@SP\nM=M-1\nA=M\nD=M\n@SP\nM=M-1\nA=M\n{}\n@SP\nM=M+1\n",
            specific_command
        )
    }
}

#[derive(PartialEq, Debug)]
pub enum UnaryArithmetic {
    Neg,
    Not,
}

impl UnaryArithmetic {
    pub fn to_assembly_code(&self) -> String {
        let specific_command = match self {
            UnaryArithmetic::Neg => "M=-M",
            UnaryArithmetic::Not => "M=!M",
        };

        format!("@SP\nM=M-1\nA=M\n{}\n@SP\nM=M+1\n", specific_command)
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

    pub fn get_ram_base_address_command(&self) -> &str {
        match self {
            Segment::Argument => "@ARG\nD=M",
            Segment::Local => "@LCL\nD=M",
            Segment::Static => "", // TODO
            Segment::Constant => panic!("constant doesn't have base address."),
            Segment::This => "@THIS\nD=M",
            Segment::That => "@THAT\nD=M",
            Segment::Pointer => "@R3\nD=A",
            Segment::Temp => "@R5\nD=A",
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
        assert_eq!(actual, CommandType::BinaryArithmetic(BinaryArithmetic::Add));
    }

    #[test]
    fn trim_comments() {
        let actual = CommandType::from_command("push constant 7 // this is comments");
        assert_eq!(actual, CommandType::CPush(Segment::Constant, 7));
    }
}
