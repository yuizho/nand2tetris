use std::collections::HashMap;

type Name = String;
type SymbolType = String;

#[derive(PartialEq, Eq, Debug)]
enum ClassAttribute {
    Static,
    Field,
}

#[derive(PartialEq, Eq, Debug)]
enum LocalAttribute {
    Argument,
    Var,
}

#[derive(PartialEq, Eq, Debug)]
struct ClassSymbol {
    symbol_type: SymbolType,
    attribute: ClassAttribute,
    number: usize,
}

#[derive(PartialEq, Eq, Debug)]
pub struct ClassSymbolTable {
    symbols: HashMap<Name, ClassSymbol>,
}
impl ClassSymbolTable {
    fn new() -> Self {
        ClassSymbolTable {
            symbols: HashMap::new(),
        }
    }

    fn add_symbol(&mut self, name: Name, symbol_type: SymbolType, attribute: ClassAttribute) {
        let number = self.var_count(&attribute);
        self.symbols.insert(
            name,
            ClassSymbol {
                symbol_type,
                attribute,
                number,
            },
        );
    }

    fn var_count(&self, attribute: &ClassAttribute) -> usize {
        self.symbols
            .iter()
            .filter(|(_, v)| &v.attribute == attribute)
            .count()
    }

    fn attr_of(&self, name: &str) -> Option<&ClassAttribute> {
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
        let mut symbol_table = ClassSymbolTable::new();
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
        let mut symbol_table = ClassSymbolTable::new();
        symbol_table.add_symbol(
            "first".to_string(),
            "String".to_string(),
            ClassAttribute::Field,
        );

        assert_eq!(Some(&0), symbol_table.index_of("first"));
        assert_eq!(None, symbol_table.index_of("none"));
    }
}
