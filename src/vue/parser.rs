use crate::vue::lexer::{HTMLLexer, HTMLToken, LexerError};
use std::fmt;


/// Represents a node in the HTML/Vue template AST.
#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    /// An HTML element, like `<div>` or `<my-component>`.
    Element(Element),
    /// Text content between elements.
    Text(String),
    /// Represents a Vue interpolation, e.g., `{{ message }}`.
    Interpolation(String),
}

/// Represents an HTML element node.
#[derive(Debug, Clone, PartialEq)]
pub struct Element {
    pub tag_name: String,
    pub attributes: Vec<Attribute>,
    pub children: Vec<Node>,
}

/// Represents an attribute on an HTML element.
#[derive(Debug, Clone, PartialEq)]
pub struct Attribute {
    pub name: String,
    /// The value is optional to support boolean attributes like `disabled`.
    pub value: Option<String>,
}

// --- Parser ---

#[derive(Debug)]
pub struct ParserError {
    pub message: String,
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Parser error: {}", self.message)
    }
}

impl std::error::Error for ParserError {}

/// A list of HTML "void" elements that don't need a closing tag.
const VOID_ELEMENTS: &[&str] = &[
    "area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta", "param", "source",
    "track", "wbr",
];

/// The parser for converting a stream of HTMLTokens into an AST.
pub struct Parser {
    lexer: HTMLLexer,
    current_token: HTMLToken,
    /// A one-token lookahead buffer for non-destructive peeking.
    peeked_token: Option<HTMLToken>,
}

impl Parser {
    /// Creates a new parser from a string input.
    pub fn new(input: &str) -> Result<Self, LexerError> {
        let mut lexer = HTMLLexer::new(input);
        let current_token = lexer.next_token()?;
        Ok(Self {
            lexer,
            current_token,
            peeked_token: None,
        })
    }

    /// Peeks at the next token without consuming the current one.
    fn peek(&mut self) -> Result<&HTMLToken, ParserError> {
        if self.peeked_token.is_none() {
            self.peeked_token = Some(self.lexer.next_token().map_err(|e| ParserError {
                message: e.to_string(),
            })?);
        }
        Ok(self.peeked_token.as_ref().unwrap())
    }

    /// Consumes the current token and advances to the next one from the peek buffer or lexer.
    fn advance(&mut self) -> Result<(), ParserError> {
        if let Some(peeked) = self.peeked_token.take() {
            self.current_token = peeked;
        } else {
            self.current_token = self.lexer.next_token().map_err(|e| ParserError {
                message: e.to_string(),
            })?;
        }
        Ok(())
    }

    /// The main entry point for parsing. Produces a list of root nodes.
    pub fn parse(&mut self) -> Result<Vec<Node>, ParserError> {
        let mut nodes = Vec::new();
        while self.current_token != HTMLToken::EOF {
            if let Some(node) = self.parse_node()? {
                nodes.push(node);
            }
        }
        Ok(nodes)
    }

    /// Parses a single node. Returns `None` if the node should be skipped (e.g., a comment).
    fn parse_node(&mut self) -> Result<Option<Node>, ParserError> {
        match &self.current_token {
            HTMLToken::OpenBracket => self.parse_element().map(Some),
            HTMLToken::Text(text) => {
                let node = if text.starts_with("{{") && text.ends_with("}}") {
                    let expression = text[2..text.len() - 2].trim().to_string();
                    Node::Interpolation(expression)
                } else {
                    Node::Text(text.clone())
                };
                self.advance()?;
                Ok(Some(node))
            }
            HTMLToken::Comment(_) => {
                self.advance()?;
                Ok(None)
            }
            _ => {
                // Skip unexpected tokens at the root level, like whitespace-only text nodes
                if let HTMLToken::Text(t) = &self.current_token {
                    if t.trim().is_empty() {
                        self.advance()?;
                        return Ok(None);
                    }
                }
                Err(ParserError {
                    message: format!("Unexpected token at root: {:?}", self.current_token),
                })
            }
        }
    }

    /// Parses an element, including its tag name, attributes, and children.
    fn parse_element(&mut self) -> Result<Node, ParserError> {
        self.advance()?; // Consume '<'

        let tag_name = match &self.current_token {
            HTMLToken::Ident(name) => name.clone(),
            _ => {
                return Err(ParserError {
                    message: "Expected tag name after '<'".to_string(),
                });
            }
        };
        self.advance()?;

        let attributes = self.parse_attributes()?;

        if self.current_token == HTMLToken::Slash {
            self.advance()?; // Consume '/'
            if self.current_token != HTMLToken::CloseBracket {
                return Err(ParserError {
                    message: "Expected '>' after '/' in self-closing tag".to_string(),
                });
            }
            self.advance()?; // Consume '>'
            Ok(Node::Element(Element {
                tag_name,
                attributes,
                children: vec![],
            }))
        } else if self.current_token == HTMLToken::CloseBracket {
            self.advance()?; // Consume '>'

            let children = if VOID_ELEMENTS.contains(&tag_name.as_str()) {
                vec![]
            } else {
                self.parse_children(&tag_name)?
            };

            Ok(Node::Element(Element {
                tag_name,
                attributes,
                children,
            }))
        } else {
            Err(ParserError {
                message: format!(
                    "Expected '>' or '/>' to close tag, found {:?}",
                    self.current_token
                ),
            })
        }
    }

    /// Parses the attributes of an element.
    fn parse_attributes(&mut self) -> Result<Vec<Attribute>, ParserError> {
        let mut attributes = Vec::new();
        while let HTMLToken::Ident(name) = &self.current_token {
            let attr_name = name.clone();
            self.advance()?;

            if self.current_token == HTMLToken::Assign {
                self.advance()?; // Consume '='
                let value = match &self.current_token {
                    HTMLToken::StrLit(val) => val.clone(),
                    _ => {
                        return Err(ParserError {
                            message: "Expected string literal for attribute value".to_string(),
                        });
                    }
                };
                self.advance()?;
                attributes.push(Attribute {
                    name: attr_name,
                    value: Some(value),
                });
            } else {
                attributes.push(Attribute {
                    name: attr_name,
                    value: None,
                });
            }
        }
        Ok(attributes)
    }

    /// Parses the child nodes of an element until a closing tag is found.
    fn parse_children(&mut self, parent_tag: &str) -> Result<Vec<Node>, ParserError> {
        let mut children = Vec::new();
        loop {
            if self.current_token == HTMLToken::OpenBracket {
                if let Ok(HTMLToken::Slash) = self.peek() {
                    self.advance()?; // Consume '<'
                    self.advance()?; // Consume '/'

                    match &self.current_token {
                        HTMLToken::Ident(tag) if tag == parent_tag => {
                            self.advance()?; // Consume tag name
                            if self.current_token != HTMLToken::CloseBracket {
                                return Err(ParserError {
                                    message: format!("Expected '>' to close tag '{}'", parent_tag),
                                });
                            }
                            self.advance()?; // Consume '>'
                            return Ok(children);
                        }
                        _ => {
                            return Err(ParserError {
                                message: format!(
                                    "Mismatched closing tag. Expected '</{}>' but found '</{:?}>'",
                                    parent_tag, self.current_token
                                ),
                            });
                        }
                    }
                }
            }

            if self.current_token == HTMLToken::EOF {
                return Err(ParserError {
                    message: format!("Unclosed tag '{}'", parent_tag),
                });
            }

            if let Some(node) = self.parse_node()? {
                children.push(node);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Helper to filter out whitespace-only text nodes from a parsed AST for easier testing.
    fn clean_ast(nodes: Vec<Node>) -> Vec<Node> {
        nodes
            .into_iter()
            .filter_map(|n| match n {
                Node::Text(t) if t.trim().is_empty() => None,
                Node::Element(mut el) => {
                    el.children = clean_ast(el.children);
                    Some(Node::Element(el))
                }
                _ => Some(n),
            })
            .collect()
    }

    #[test]
    fn test_parse_simple_element() {
        let mut parser = Parser::new("<div></div>").unwrap();
        let ast = parser.parse().unwrap();
        assert_eq!(
            clean_ast(ast),
            vec![Node::Element(Element {
                tag_name: "div".to_string(),
                attributes: vec![],
                children: vec![],
            })]
        );
    }

    #[test]
    fn test_parse_element_with_text() {
        let mut parser = Parser::new("<h1>Hello</h1>").unwrap();
        let ast = parser.parse().unwrap();
        assert_eq!(
            clean_ast(ast),
            vec![Node::Element(Element {
                tag_name: "h1".to_string(),
                attributes: vec![],
                children: vec![Node::Text("Hello".to_string())],
            })]
        );
    }

    #[test]
    fn test_parse_with_attributes() {
        let mut parser =
            Parser::new(r#"<div id="main" class="container" disabled></div>"#).unwrap();
        let ast = parser.parse().unwrap();
        assert_eq!(
            clean_ast(ast),
            vec![Node::Element(Element {
                tag_name: "div".to_string(),
                attributes: vec![
                    Attribute {
                        name: "id".to_string(),
                        value: Some("main".to_string())
                    },
                    Attribute {
                        name: "class".to_string(),
                        value: Some("container".to_string())
                    },
                    Attribute {
                        name: "disabled".to_string(),
                        value: None
                    },
                ],
                children: vec![],
            })]
        );
    }

    #[test]
    fn test_nested_elements() {
        let mut parser = Parser::new("<div><p>Text</p></div>").unwrap();
        let ast = parser.parse().unwrap();
        assert_eq!(
            clean_ast(ast),
            vec![Node::Element(Element {
                tag_name: "div".to_string(),
                attributes: vec![],
                children: vec![Node::Element(Element {
                    tag_name: "p".to_string(),
                    attributes: vec![],
                    children: vec![Node::Text("Text".to_string())],
                })],
            })]
        );
    }

    #[test]
    fn test_parse_vue_template_and_strip_comments() {
        let vue_template = r#"
            <template>
                <!-- This comment should be stripped -->
                <div class="container" a:test="callexpr()" :self>
                    <!-- Another comment -->
                    <h1 v-model:test="var">{{ title }}</h1>
                    <button @click.prevent="handleClick">Click me</button>
                    <self-closing-tag />
                </div>
            </template>
        "#;
        let mut parser = Parser::new(vue_template).unwrap();
        let ast = parser.parse().unwrap();

        let expected_ast = vec![Node::Element(Element {
            tag_name: "template".to_string(),
            attributes: vec![],
            children: vec![Node::Element(Element {
                tag_name: "div".to_string(),
                attributes: vec![
                    Attribute {
                        name: "class".to_string(),
                        value: Some("container".to_string()),
                    },
                    Attribute {
                        name: "a:test".to_string(),
                        value: Some("callexpr()".to_string()),
                    },
                    Attribute {
                        name: ":self".to_string(),
                        value: None,
                    },
                ],
                children: vec![
                    Node::Element(Element {
                        tag_name: "h1".to_string(),
                        attributes: vec![Attribute {
                            name: "v-model:test".to_string(),
                            value: Some("var".to_string()),
                        }],
                        children: vec![Node::Interpolation("title".to_string())],
                    }),
                    Node::Element(Element {
                        tag_name: "button".to_string(),
                        attributes: vec![Attribute {
                            name: "@click.prevent".to_string(),
                            value: Some("handleClick".to_string()),
                        }],
                        children: vec![Node::Text("Click me".to_string())],
                    }),
                    Node::Element(Element {
                        tag_name: "self-closing-tag".to_string(),
                        attributes: vec![],
                        children: vec![],
                    }),
                ],
            })],
        })];

        assert_eq!(clean_ast(ast), expected_ast);
    }
}
