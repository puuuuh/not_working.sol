use std::cmp::Reverse;
use syntax::{TextRange, TextSize};
use crate::semantics::child_container::ChildSource;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, salsa::Update)]
pub struct SpanMapRange(pub(crate) TextSize, pub(crate) TextSize);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, salsa::Update)]
pub struct SpanMap<'db> {
    map: Vec<(SpanMapRange, ChildSource<'db>)>,
}

impl<'db> SpanMap<'db> {
    pub fn new(mut data: Vec<(SpanMapRange, ChildSource<'db>)>) -> Self {
        data.sort();
        Self {
            map: data
        }
    }

    pub fn find(&self, range: TextRange) -> Option<ChildSource> {
        let mut res = None;
        for (cur_range, val) in &self.map {
            if cur_range.0 > range.start() {
                break;
            }
            if cur_range.1 >= range.end() {
                res = Some(*val)
            }
        }
        res
    }
}