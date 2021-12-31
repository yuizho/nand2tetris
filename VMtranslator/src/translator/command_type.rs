use uuid::Uuid;

const COMMAND_PUSH_DATA_TO_STACK: &str = "@SP\nA=M\nM=D\n@SP\nM=M+1";
const COMMAND_POP_DATA_FROM_STACK: &str = "@SP\nM=M-1\n@SP\nA=M\nD=M\nM=0";

#[derive(PartialEq, Debug)]
pub enum CommandType {
    BinaryArithmetic(BinaryArithmetic),
    UnaryArithmetic(UnaryArithmetic),
    CPush(Segment, usize),
    CPop(Segment, usize),
    Label(String),
    GoTo(String),
    IfGoTo(String),
    Blank,
}

impl CommandType {
    pub fn from_command(command: &str, vm_name: &str) -> Self {
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
                    Segment::from_command(segment, vm_name),
                    index.parse::<usize>().unwrap(),
                )
            }
            trimed if trimed.starts_with("pop") => {
                let (_, params) = trimed.split_once(" ").unwrap();
                let (segment, index) = params.split_once(" ").unwrap();
                CommandType::CPop(
                    Segment::from_command(segment, vm_name),
                    index.parse::<usize>().unwrap(),
                )
            }
            trimed if trimed.starts_with("label") => {
                let (_, name) = trimed.split_once(" ").unwrap();
                CommandType::Label(name.to_string())
            }
            trimed if trimed.starts_with("goto") => {
                let (_, label_name) = trimed.split_once(" ").unwrap();
                CommandType::GoTo(label_name.to_string())
            }
            trimed if trimed.starts_with("if-goto") => {
                let (_, label_name) = trimed.split_once(" ").unwrap();
                CommandType::IfGoTo(label_name.to_string())
            }
            _ => panic!("unexpected command type {}", command),
        }
    }

    pub fn to_assembly_code(&self) -> String {
        match self {
            CommandType::BinaryArithmetic(binary) => binary.to_assembly_code(),
            CommandType::UnaryArithmetic(unary) => unary.to_assembly_code(),
            CommandType::CPush(segment, index) => match segment {
                Segment::Constant => format!("@{}\nD=A\n{}\n", index, COMMAND_PUSH_DATA_TO_STACK),
                Segment::Static(vm_name) => {
                    format!(
                        "@{}.{}\nD=M\n{}\n",
                        vm_name, index, COMMAND_PUSH_DATA_TO_STACK
                    )
                }
                segment => format!(
                    "{}\n@{}\nA=D+A\nD=M\n{}\n",
                    segment.get_ram_base_address_command(),
                    index,
                    COMMAND_PUSH_DATA_TO_STACK
                ),
            },
            CommandType::CPop(segment, index) => match segment {
                Segment::Static(vm_name) => {
                    format!(
                        "{}\n@{}.{}\nM=D\n",
                        COMMAND_POP_DATA_FROM_STACK, vm_name, index
                    )
                }
                segment => format!(
                    "{}\n@{}\nD=D+A\n@R13\nM=D\n{}\n@13\nA=M\nM=D\n",
                    segment.get_ram_base_address_command(),
                    index,
                    COMMAND_POP_DATA_FROM_STACK
                ),
            },
            CommandType::Label(name) => format!("({})\n", name),
            // TODO: label needs prefix of function?
            CommandType::GoTo(label_name) => format!("@{}\n0;JMP\n", label_name),
            CommandType::IfGoTo(label_name) => {
                format!("{}\n@{}\nD;JNE\n", COMMAND_POP_DATA_FROM_STACK, label_name)
            }
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
    Static(String),
    Constant,
    This,
    That,
    Pointer,
    Temp,
}

impl Segment {
    pub fn from_command(segment_str: &str, vm_name: &str) -> Self {
        let trimed = segment_str.trim();

        match trimed {
            "argument" => Segment::Argument,
            "local" => Segment::Local,
            "static" => Segment::Static(vm_name.to_string()),
            "constant" => Segment::Constant,
            "this" => Segment::This,
            "that" => Segment::That,
            "pointer" => Segment::Pointer,
            "temp" => Segment::Temp,
            _ => panic!("unexpected segment {}", trimed),
        }
    }

    pub fn get_ram_base_address_command(&self) -> String {
        match self {
            Segment::Argument => format!("@ARG\nD=M"),
            Segment::Local => format!("@LCL\nD=M"),
            Segment::This => format!("@THIS\nD=M"),
            Segment::That => format!("@THAT\nD=M"),
            Segment::Pointer => format!("@R3\nD=A"),
            Segment::Temp => format!("@R5\nD=A"),
            segment => panic!("{:?} doesn't have base address.", segment),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::translator::command_type::*;

    #[test]
    fn push_constant() {
        let actual = CommandType::from_command("push constant 7", "test");
        assert_eq!(actual, CommandType::CPush(Segment::Constant, 7));
    }

    #[test]
    fn add() {
        let actual = CommandType::from_command("add", "test");
        assert_eq!(actual, CommandType::BinaryArithmetic(BinaryArithmetic::Add));
    }

    #[test]
    fn trim_comments() {
        let actual = CommandType::from_command("push constant 7 // this is comments", "test");
        assert_eq!(actual, CommandType::CPush(Segment::Constant, 7));
    }

    #[test]
    fn push_assebly_code() {
        let actual = CommandType::CPush(Segment::Argument, 2).to_assembly_code();
        assert_eq!(
            actual,
            "@ARG\nD=M\n@2\nA=D+A\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"
        );
    }

    #[test]
    fn pop_assebly_code() {
        let actual = CommandType::CPop(Segment::Argument, 2).to_assembly_code();
        assert_eq!(
            actual,
            "@ARG\nD=M\n@2\nD=D+A\n@R13\nM=D\n@SP\nM=M-1\n@SP\nA=M\nD=M\nM=0\n@13\nA=M\nM=D\n"
        );
    }
}
