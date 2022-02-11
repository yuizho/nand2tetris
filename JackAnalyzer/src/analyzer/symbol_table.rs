use std::collections::HashMap;
use std::str::FromStr;

type Name = String;
type SymbolType = String;

#[derive(PartialEq, Eq, Debug)]
pub enum ClassAttribute {
    Static,
    Field,
}
impl FromStr for ClassAttribute {
    type Err = String;

    fn from_str(input: &str) -> Result<ClassAttribute, Self::Err> {
        match input {
            "static" => Ok(Self::Static),
            "field" => Ok(Self::Field),
            s => Err(format!("unexpected str: {}", s)),
        }
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

#[derive(PartialEq, Eq, Debug)]
struct Symbol<T> {
    symbol_type: SymbolType,
    attribute: T,
    number: usize,
}

#[derive(PartialEq, Eq, Debug)]
pub struct SymbolTable<T: Eq + PartialEq> {
    symbols: HashMap<Name, Symbol<T>>,
}
impl<T: Eq + PartialEq> SymbolTable<T> {
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

    fn var_count(&self, attribute: &T) -> usize {
        self.symbols
            .iter()
            .filter(|(_, v)| &v.attribute == attribute)
            .count()
    }

    fn attr_of(&self, name: &str) -> Option<&T> {
        let symbol = self.symbols.get(name)?;
        Some(&symbol.attribute)
    }

    fn type_of(&self, name: &str) -> Option<&str> {
        let symbol = self.symbols.get(name)?;
        Some(&symbol.symbol_type)
    }

    fn index_of(&self, name: &str) -> Option<&usize> {
        let symbol = self.symbols.get(name)?;
        Some(&symbol.number)
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
    fn class_symbol_table_index_of() {
        let mut symbol_table = SymbolTable::<ClassAttribute>::new();
        symbol_table.add_symbol(
            "first".to_string(),
            "String".to_string(),
            ClassAttribute::Field,
        );

        assert_eq!(Some(&0), symbol_table.index_of("first"));
        assert_eq!(None, symbol_table.index_of("none"));
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
    fn local_symbol_table_index_of() {
        let mut symbol_table = SymbolTable::<LocalAttribute>::new();
        symbol_table.add_symbol(
            "first".to_string(),
            "String".to_string(),
            LocalAttribute::Var,
        );

        assert_eq!(Some(&0), symbol_table.index_of("first"));
        assert_eq!(None, symbol_table.index_of("none"));
    }
}
