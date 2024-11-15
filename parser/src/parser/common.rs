use rowan::{SyntaxElement, SyntaxElementChildren, SyntaxText};
use syntax::SolidityLang;

pub fn format_node(buf: &mut String, l: usize, list: &mut SyntaxElementChildren<SolidityLang>) {
    let prefix = "    ".repeat(l);
    for child in list {
        let text = match &child {
            SyntaxElement::Node(n) => "",
            SyntaxElement::Token(n) => n.text(),
        };
        buf.push_str(&format!(
            "{prefix}{:?}@{:?} {data:?}\n",
            child.kind(),
            child.text_range(),
            data = text
        ));
        if let Some(n) = child.as_node() {
            let mut nodes = n.children_with_tokens();
            format_node(buf, l + 1, &mut nodes);
        }
    }
}
