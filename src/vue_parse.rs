use std::{fs::File, io::Read};

mod vue;
use vue::parser::Parser as VueTemplateParser;

use crate::vue::parser::{Attribute, Element, Node};

use oxc::{allocator::Allocator, ast::ast::*, parser::Parser};

fn main() {
    const TEST_FILE_SRC: &str = "test.vue";

    let mut fs = File::open(TEST_FILE_SRC).unwrap();
    let mut file_contents = vec![];

    fs.read_to_end(&mut file_contents).unwrap();

    let file_text = String::from_utf8(file_contents).expect("file not valid utf8");

    let mut parser = VueTemplateParser::new(&file_text).expect("parser init err");
    let mut nodes = parser.parse().expect("parse error");
    let ast = nodes.remove(0);

    process_ast(ast);
}

fn process_ast(ast: Node) {
    match ast {
        Node::Element(e) => process_element(e),
        Node::Interpolation(expr) => process_expr(expr),
        Node::Text(_) => {}
    }
}

fn process_element(element: Element) {
    // If the tag name isn't a native dom element, add it to a list of used deps

    for attr in element.attributes {
        process_attribute(attr);
    }

    for node in element.children {
        process_ast(node);
    }
}

fn process_attribute(attr: Attribute) {
    let Attribute { name, value } = attr;

    if value.is_none() {
        if name.starts_with(":") {
            let ref_name = name.clone().split_off(1);
            println!("{ref_name}\n");
        } else {
            return;
        }
    }

    if name.starts_with("v-") || name.starts_with("@") || name.starts_with(":") {
        let value_str = value.unwrap();

        let parser_allocator = Allocator::new();
        let expr = Parser::new(&parser_allocator, &value_str, SourceType::cjs())
            .parse_expression()
            .expect("expr parse error");

        println!("{expr:?}\n");
    }
}

fn process_expr(expr: String) {
    let parser_allocator = Allocator::new();
    let expr = Parser::new(&parser_allocator, &expr, SourceType::cjs())
        .parse_expression()
        .expect("expr parse error");

    println!("{expr:?}\n");
}
