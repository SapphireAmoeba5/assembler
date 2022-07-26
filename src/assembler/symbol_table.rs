use super::size::Size;
use std::collections::HashMap;

pub struct Label {
    pub name: String,
    pub address: u64,
    pub line: usize,
}

impl Label {
    pub fn new(name: String, address: u64, line: usize) -> Self {
        Self {
            name,
            address,
            line,
        }
    }
}

pub struct Constant {
    pub name: String,
    pub value: u64,
    pub line: usize,
    pub size: Size,
}

impl Constant {
    pub fn new(name: String, value: u64, line: usize, size: Size) -> Self {
        Self {
            name,
            value,
            line,
            size,
        }
    }
}

pub enum Symbol {
    SymbolLabel(Label),
    SymbolConstant(Constant),
}

impl Symbol {
    pub fn get_line(&self) -> usize {
        match self {
            Symbol::SymbolConstant(constant) => constant.line,
            Symbol::SymbolLabel(label) => label.line,
        }
    }
}

pub struct SymbolTable {
    table: HashMap<String, Symbol>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            table: HashMap::new(),
        }
    }

    pub fn add_label(&mut self, label: Label) -> bool {
        if self.table.contains_key(&label.name) {
            return false;
        }

        self.table
            .insert(label.name.clone(), Symbol::SymbolLabel(label));
        true
    }

    pub fn add_constant(&mut self, constant: Constant) -> bool {
        if self.table.contains_key(&constant.name) {
            return false;
        }

        self.table
            .insert(constant.name.clone(), Symbol::SymbolConstant(constant));
        true
    }

    pub fn insert(&mut self, symbol: Symbol) -> bool {
        match symbol {
            Symbol::SymbolLabel(label) => self.add_label(label),
            Symbol::SymbolConstant(constant) => self.add_constant(constant),
        }
    }

    pub fn get(&self, name: &str) -> Option<&Symbol> {
        self.table.get(name)
    }

    pub fn get_mut(&mut self, name: &str) -> Option<&mut Symbol> {
        self.table.get_mut(name)
    }

    pub fn get_label(&self, name: &str) -> Option<&Label> {
        match self.table.get(name) {
            Some(Symbol::SymbolLabel(label)) => Some(label),
            _ => None,
        }
    }

    pub fn get_label_mut(&mut self, name: &str) -> Option<&mut Label> {
        match self.table.get_mut(name) {
            Some(Symbol::SymbolLabel(label)) => Some(label),
            _ => None,
        }
    }

    pub fn get_constant(&self, name: &str) -> Option<&Constant> {
        match self.table.get(name) {
            Some(Symbol::SymbolConstant(constant)) => Some(constant),
            _ => None,
        }
    }

    pub fn get_constant_mut(&mut self, name: &str) -> Option<&mut Constant> {
        match self.table.get_mut(name) {
            Some(Symbol::SymbolConstant(constant)) => Some(constant),
            _ => None,
        }
    }
}
