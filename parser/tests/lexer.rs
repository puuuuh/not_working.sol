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
fn lexer_tests() {
    let mut n = 0;
    let start = std::time::Instant::now();
    enum_files("test_data/syntaxTests", &mut |path| {
        println!("{path:?}");
        let Ok(data) = fs::read_to_string(&path) else {
            eprintln!("Invalid test file: {path}, skipped", path = path.display());
            return;
        };
        eprintln!("{:?}", path);
        for data in split::sources(&data) {
            n += 1;
            let lexer = Lexer::new(data);
            let mut pos = 0;
            let tokens = lexer
                .map(move |t| {
                    let r = (t.kind, &data[pos..pos + t.len]);
                    pos += t.len;
                    n += 1;
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
        }
    });
    dbg!(start.elapsed());
}
*/
