pub struct AssemblerSource {
    source: String,
}

impl AssemblerSource {
    pub fn new(source: String) -> Self {
        Self {
            source
        }
    }

    pub fn get_source(&self) -> &str {
        &self.source
    }

    pub fn tokens(&self) -> TokenIter {
        TokenIter {
            source: self,
            position: 0,
        }
    }
}


pub struct TokenIter<'a> {
    source: &'a AssemblerSource,
    position: usize,
}

impl<'a> Iterator for TokenIter<'a> {
    type Item = &'a str;

    fn next(&self) -> Option<Self::Item> {

    }
}
