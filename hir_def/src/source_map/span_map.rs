use std::cmp::Reverse;
use syntax::{TextRange, TextSize};

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord, salsa::Update)]
pub struct SpanMapRange(pub(crate) TextSize, pub(crate) TextSize);

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord, salsa::Update)]
pub struct SpanMap<T: salsa::Update> {
    map: Vec<(SpanMapRange, T)>,
}

impl<T: salsa::Update + Copy> SpanMap<T> {
    pub fn new(mut data: Vec<(SpanMapRange, T)>) -> Self {
        data.sort_by_key(|a| a.0);
        Self {
            map: data
        }
    }

    pub fn find(&self, range: TextRange) -> Option<T> {
        let mut res = None;
        let mut res_range = None;
        for (cur_range, val) in &self.map {
            if cur_range.0 > range.start() {
                break;
            }
            if cur_range.1 >= range.end() && res_range.is_none_or(|r: &SpanMapRange| r.1 > cur_range.1) {
                res = Some(*val);
                res_range = Some(cur_range);
            }
        }
        res
    }
}