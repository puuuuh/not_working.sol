pub fn sources(data: &str) -> impl Iterator<Item = &str> {
    let mut next_start = 0;
    std::iter::from_fn(move || {
        let data = &data[next_start..];
        if data.is_empty() {
            return None;
        }
        let mut lines = data.split_inclusive('\n');
        let mut local_offset = 0;
        let mut local_end = 0;
        for l in &mut lines {
            if l.starts_with("==== Source:") {
                if local_offset != 0 {
                    let t = Some(&data[local_offset..local_end]);
                    next_start += local_end;
                    return t;
                } else {
                    local_offset = l.len();
                }
            }
            local_end += l.len();
        }
        next_start += local_end;
        Some(&data[local_offset..local_end])
    })
}
