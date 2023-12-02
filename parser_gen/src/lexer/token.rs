use crate::parser::{GetEOF, NoData};

use super::source_file::Location;

#[derive(Debug, Clone)]
pub struct Token<T> {
    pub ty: T,
    pub loc: Location,
}

impl<T: NoData<U>, U> NoData<U> for Token<T> {
    fn no_data(&self) -> U {
        self.ty.no_data()
    }
}

impl<T: GetEOF<U>, U> GetEOF<U> for Token<T> {
    fn get_eof() -> U {
        T::get_eof()
    }
}
