mod split;

use std::{
    fs,
    path::{Path, PathBuf},
};

use parser::parser::{common::format_node, lexer::Lexer, Parser};

fn enum_files(dir: impl AsRef<Path>, cb: &mut impl FnMut(PathBuf)) {
    let files = std::fs::read_dir(dir).unwrap();
    for f in files {
        let f = f.unwrap();
        if f.metadata().unwrap().is_dir() {
            enum_files(f.path(), cb)
        } else {
            cb(f.path())
        }
    }
}

#[test]
fn parser_tests() {
    let mut n = 0;
    let start = std::time::Instant::now();
    enum_files("test_data\\", &mut |path| {
        if path.extension().map(|f| f == "sol") != Some(true) {
            return;
        }
        let Ok(data) = fs::read_to_string(&path) else {
            eprintln!("Invalid test file: {path}, skipped", path = path.display());
            return;
        };
        n += 1;
        let lexer = Lexer::new(&data);
        let mut pos = 0;
        let data = &data;
        let tokens = lexer
            .map(move |t| {
                let r = (t.kind, &data[pos..pos + t.len]);
                pos += t.len;
                r
            })
            .collect::<Vec<_>>();

        Parser::parse(&tokens);
    });
    dbg!(n);
    dbg!(start.elapsed());
}
