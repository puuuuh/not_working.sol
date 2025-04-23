use std::cmp::Reverse;
use syntax::{TextRange, TextSize};

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct SpanMap<T: salsa::Update> {
    data: Vec<(TextSize, T)>,
}

unsafe impl<T: salsa::Update> salsa::Update for SpanMap<T> {
    unsafe fn maybe_update(old_pointer: *mut Self, new_value: Self) -> bool {
        let old = unsafe { &mut *old_pointer };

        if old.data.len() != new_value.data.len() {
            old.data.clear();
            old.data.extend(new_value.data);
            return true;
        }

        let mut changed = false;
        for (old_element, new_element) in old.data.iter_mut().zip(new_value.data) {
            changed |= old_element.0 != new_element.0;
            old_element.0 = new_element.0;
            changed |= unsafe { T::maybe_update(&mut old_element.1, new_element.1) };
        }

        changed
    }
}

impl<T: salsa::Update + Copy> SpanMap<T> {
    pub fn new(mut data: Vec<(TextRange, T)>) -> Self {
        data.sort_by(|a, b| {
            a.0.start().cmp(&b.0.start()).then(b.0.end().cmp(&a.0.end()))
        });

        let mut res = Vec::with_capacity(data.len());
        let mut stack: Vec<(TextSize, T)> = Vec::with_capacity(3);
        for (range, item) in data.iter() {
            let end = range.end() - TextSize::new(1);
            while let Some(closed_item) = stack.pop_if(|x| x.0 <= range.start()) {
                res.push((closed_item.0, closed_item.1))
            }
            while let Some(t) = res.pop_if(|x| x.0 > end) {
                stack.push(t);
            }
            if let Some(parent) = stack.last() {
                let new_last_pos = range.start() - TextSize::new(1);
                let last_pos = res.last().map(|l| l.0).unwrap_or_default();

                if last_pos < new_last_pos {
                    res.push((new_last_pos, parent.1));
                }
            }
            res.push((range.end(), *item));
        }

        Self {
            data: res
        }
    }

    pub fn find_pos(&self, pos: TextSize) -> Option<T> {
        self.data.get(match self.data.binary_search_by(|a| a.0.cmp(&pos)) {
            Ok(i) => i,
            Err(i) => i,
        }).map(|a| a.1)
    }

    pub fn find(&self, range: TextRange) -> Option<T> {
        self.find_pos(range.start())
    }
}