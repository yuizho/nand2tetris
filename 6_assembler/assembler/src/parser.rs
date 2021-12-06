use std::collections::HashMap;
use std::fs::File;
use std::io::Read;

pub struct SymbolTable {
    symbols: HashMap<String, i32>,
    memory_index: i32,
}

impl SymbolTable {
    /**
     * create symbols hash map which just has reserved symbols.
     */
    pub fn create() -> Self {
        let mut symbols: HashMap<String, i32> = HashMap::new();
        // put reserved symbols
        for i in 0..17 {
            symbols.insert(format!("R{}", i), i);
        }
        symbols.insert("SP".to_string(), 0);
        symbols.insert("LCL".to_string(), 1);
        symbols.insert("ARG".to_string(), 2);
        symbols.insert("THIS".to_string(), 3);
        symbols.insert("THAT".to_string(), 4);
        symbols.insert("SCREEN".to_string(), 16384);
        symbols.insert("KBD".to_string(), 24576);

        SymbolTable {
            symbols,
            memory_index: 16,
        }
    }

    pub fn resolve_label_symbols(&mut self, contents: &Vec<String>) {
        let mut index = 0;
        for content in contents.iter() {
            let trimed = content.trim();

            if trimed.starts_with("//") || trimed.is_empty() {
                continue;
            }

            match &trimed[..1] {
                "(" => {
                    let symbol = trimed.replace("(", "").replace(")", "");
                    println!("label symbol is {},  code index is {}", symbol, index);
                    self.symbols.insert(symbol, index);
                }
                _ => index += 1,
            }
        }
    }

    pub fn put_variable_symbol(&mut self, symbol: String) {
        println!(
            "variable symbol is {},  memory address is {}",
            symbol, self.memory_index
        );
        self.symbols.insert(symbol, self.memory_index);
        self.memory_index += 1;
    }

    pub fn contains(&self, symbol: &String) -> bool {
        self.symbols.contains_key(symbol)
    }

    pub fn get_address(&self, symbol: &String) -> Option<i32> {
        self.symbols.get(symbol).map(|i| i.clone())
    }
}

pub struct Parser {
    contents: Vec<String>,
    current_line: usize,
    symbol_table: SymbolTable,
}

impl Parser {
    pub fn create(filename: &String) -> Self {
        let mut contents = String::new();
        let mut f = File::open(filename).expect("file not found");
        f.read_to_string(&mut contents).expect("filed to read file");
        let contents = contents.split("\n").map(|s| s.to_string()).collect();

        // create symbol table and resolve label symbols
        let mut symbol_table = SymbolTable::create();
        symbol_table.resolve_label_symbols(&contents);

        Parser {
            symbol_table,
            contents,
            current_line: 0,
        }
    }

    pub fn has_more_commands(&self) -> bool {
        self.contents.len() > self.current_line
    }

    pub fn advance(&mut self) {
        self.current_line += 1
    }

    /**
     * parse nimonic codes to CommandType obj with resolving variable symbols.
     */
    pub fn parse(&mut self) -> CommandType {
        // resolve variable symbols and parse
        CommandType::from_nimonic_code(&self.contents[self.current_line], &mut self.symbol_table)
    }
}

#[derive(Debug)]
pub enum CommandType {
    A(i32),
    C(Option<String>, String, Option<String>),
    Blank,
}

impl CommandType {
    // TODO: refactor
    fn from_nimonic_code(instruction: &String, symbol_table: &mut SymbolTable) -> Self {
        let removed_comments = &instruction[..instruction.find("//").unwrap_or(instruction.len())];
        let trimed = removed_comments.trim();
        if trimed.is_empty() || trimed.starts_with("(") {
            CommandType::Blank
        } else if trimed.starts_with("@") {
            let symbol = trimed[1..].to_string();
            if let Ok(address) = symbol.parse::<i32>() {
                return CommandType::A(address);
            }

            if !symbol_table.contains(&symbol) {
                symbol_table.put_variable_symbol(symbol.clone())
            }

            let address = symbol_table.get_address(&symbol).unwrap();
            CommandType::A(address)
        } else if trimed.contains("=") {
            let (dest, cmp) = trimed.split_once('=').unwrap();
            CommandType::C(Some(String::from(dest)), String::from(cmp), None)
        } else if trimed.contains(";") {
            let (cmp, jmp) = trimed.split_once(';').unwrap();
            CommandType::C(None, String::from(cmp), Some(String::from(jmp)))
        } else {
            panic!("unexpected syntax {}", instruction)
        }
    }

    pub fn to_binary_code(&self) -> Option<i32> {
        match self {
            CommandType::A(symbol) => Some(symbol.clone()),
            CommandType::C(dest, cmp, jmp) => Some(
                0b1110000000000000
                    + convert_comp(cmp.clone())
                    + convert_dest(&dest)
                    + convert_jump(&jmp),
            ),
            _ => None,
        }
    }
}

fn convert_comp(cmp: String) -> i32 {
    let trimed_cmp = cmp.replace(" ", "");
    match trimed_cmp.as_str() {
        "0" => 0b0101010000000,
        "1" => 0b0111111000000,
        "-1" => 0b0111010000000,
        "D" => 0b0001100000000,
        "A" => 0b0110000000000,
        "!D" => 0b0001101000000,
        "!A" => 0b0110001000000,
        "-D" => 0b0001111000000,
        "-A" => 0b0110011000000,
        "D+1" => 0b0011111000000,
        "A+1" => 0b0110111000000,
        "D-1" => 0b0001110000000,
        "A-1" => 0b0110010000000,
        "D+A" => 0b0000010000000,
        "D-A" => 0b0010011000000,
        "A-D" => 0b0000111000000,
        "D&A" => 0b0000000000000,
        "D|A" => 0b0010101000000,
        "M" => 0b1110000000000,
        "!M" => 0b1110001000000,
        "-M" => 0b1110011000000,
        "M+1" => 0b1110111000000,
        "M-1" => 0b1110010000000,
        "D+M" => 0b1000010000000,
        "D-M" => 0b1010011000000,
        "M-D" => 0b1000111000000,
        "D&M" => 0b1000000000000,
        "D|M" => 0b1010101000000,
        _ => panic!("unexpected cmp code is comming: {}", cmp),
    }
}

fn convert_dest(dest: &Option<String>) -> i32 {
    match dest.as_deref() {
        Some("M") => 0b001000,
        Some("D") => 0b010000,
        Some("MD") => 0b011000,
        Some("A") => 0b100000,
        Some("AM") => 0b101000,
        Some("AD") => 0b110000,
        Some("AMD") => 0b111000,
        None => 0b000000,
        Some(a) => panic!("unexpected dest code is comming: {}", a),
    }
}

fn convert_jump(jmp: &Option<String>) -> i32 {
    match jmp.as_deref() {
        Some("JGT") => 0b001,
        Some("JEQ") => 0b010,
        Some("JGE") => 0b011,
        Some("JLT") => 0b100,
        Some("JNE") => 0b101,
        Some("JLE") => 0b110,
        Some("JMP") => 0b111,
        None => 0b000,
        Some(a) => panic!("unexpected jump code is comming: {}", a),
    }
}
