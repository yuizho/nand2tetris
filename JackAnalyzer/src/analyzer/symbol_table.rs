use std::collections::HashMap;
use std::str::FromStr;

type Name = String;
type SymbolType = String;

#[derive(PartialEq, Eq, Debug)]
pub enum ClassAttribute {
    Static,
    Field,
    Constructor,
    Method,
    Function,
}
impl FromStr for ClassAttribute {
    type Err = String;

    fn from_str(input: &str) -> Result<ClassAttribute, Self::Err> {
        match input {
            "static" => Ok(Self::Static),
            "field" => Ok(Self::Field),
            "constructor" => Ok(Self::Constructor),
            "method" => Ok(Self::Method),
            "function" => Ok(Self::Function),
            s => Err(format!("unexpected str: {}", s)),
        }
    }
}
impl ToString for ClassAttribute {
    fn to_string(&self) -> String {
        use ClassAttribute::*;
        let s = match self {
            Static => "static",
            Field => "field",
            Constructor => "constructor",
            Method => "method",
            Function => "function",
        };
        s.to_string()
    }
}

#[derive(PartialEq, Eq, Debug)]
pub enum LocalAttribute {
    Argument,
    Var,
}
impl FromStr for LocalAttribute {
    type Err = String;

    fn from_str(input: &str) -> Result<LocalAttribute, Self::Err> {
        match input {
            "argument" => Ok(Self::Argument),
            "var" => Ok(Self::Var),
            s => Err(format!("unexpected str: {}", s)),
        }
    }
}
impl ToString for LocalAttribute {
    fn to_string(&self) -> String {
        use LocalAttribute::*;
        let s = match self {
            Argument => "argument",
            Var => "var",
        };
        s.to_string()
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct Symbol<T> {
    pub symbol_type: SymbolType,
    pub attribute: T,
    pub number: usize,
}

#[derive(PartialEq, Eq, Debug)]
pub struct SymbolTable<T: Eq + PartialEq + ToString> {
    symbols: HashMap<Name, Symbol<T>>,
}
impl<T: Eq + PartialEq + ToString> SymbolTable<T> {
    pub fn new() -> Self {
        SymbolTable {
            symbols: HashMap::new(),
        }
    }

    pub fn add_symbol(&mut self, name: Name, symbol_type: SymbolType, attribute: T) {
        let number = self.var_count(&attribute);
        self.symbols.insert(
            name,
            Symbol {
                symbol_type,
                attribute,
                number,
            },
        );
    }

    pub fn var_count(&self, attribute: &T) -> usize {
        self.symbols
            .iter()
            .filter(|(_, v)| &v.attribute == attribute)
            .count()
    }

    pub fn of(&self, name: &str) -> Option<&Symbol<T>> {
        self.symbols.get(name)
    }
}

#[cfg(test)]
mod tests {
    use crate::analyzer::symbol_table::*;

    #[test]
    fn class_symbol_table_var_count() {
        let mut symbol_table = SymbolTable::<ClassAttribute>::new();
        symbol_table.add_symbol(
            "first".to_string(),
            "String".to_string(),
            ClassAttribute::Field,
        );

        assert_eq!(0, symbol_table.var_count(&ClassAttribute::Static));
        assert_eq!(1, symbol_table.var_count(&ClassAttribute::Field));
    }

    #[test]
    fn class_symbol_table_of() {
        let mut symbol_table = SymbolTable::<ClassAttribute>::new();
        symbol_table.add_symbol(
            "first".to_string(),
            "String".to_string(),
            ClassAttribute::Field,
        );

        assert_eq!(
            Some(&Symbol {
                number: 0,
                symbol_type: "String".to_string(),
                attribute: ClassAttribute::Field,
            }),
            symbol_table.of("first")
        );
        assert_eq!(None, symbol_table.of("none"));
    }

    #[test]
    fn local_symbol_table_var_count() {
        let mut symbol_table = SymbolTable::<LocalAttribute>::new();
        symbol_table.add_symbol(
            "first".to_string(),
            "String".to_string(),
            LocalAttribute::Argument,
        );

        assert_eq!(0, symbol_table.var_count(&LocalAttribute::Var));
        assert_eq!(1, symbol_table.var_count(&LocalAttribute::Argument));
    }

    #[test]
    fn local_symbol_table_of() {
        let mut symbol_table = SymbolTable::<LocalAttribute>::new();
        symbol_table.add_symbol(
            "first".to_string(),
            "String".to_string(),
            LocalAttribute::Var,
        );

        assert_eq!(
            Some(&Symbol {
                number: 0,
                symbol_type: "String".to_string(),
                attribute: LocalAttribute::Var,
            }),
            symbol_table.of("first")
        );
        assert_eq!(None, symbol_table.of("none"));
    }
}
