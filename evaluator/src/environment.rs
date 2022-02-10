use std::collections::HashMap;

use crate::object;

#[derive(Debug, PartialEq, Clone)]
pub struct Environment {
    store: HashMap<String, object::Object>,
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
}

impl Environment {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
        }
    }

    pub fn set(&mut self, k: &str, v: object::Object) {
        self.store.insert(k.to_owned(), v);
    }

    pub fn get(&self, k: &str) -> Option<object::Object> {
        self.store.get(k).cloned()
    }
}
