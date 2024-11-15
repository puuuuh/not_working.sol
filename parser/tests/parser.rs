mod split;

use std::path::{Path, PathBuf};

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

/*
#[test]
fn parser_tests() {
    let mut n = 0;
    let start = std::time::Instant::now();
    enum_files("C:\\Users\\user\\Documents\\s-game\\node_modules\\", &mut |path| {
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

        let res = Parser::parse(&tokens);
        let node = res.syntax_node();
        if !res.errors().is_empty() {
            let mut data = String::new();
            format_node(&mut data, 0, &mut node.children_with_tokens());
            println!("{data}");
            dbg!(res.errors());
            panic!()
        }
    });
    dbg!(n);
    dbg!(start.elapsed());
}
*/