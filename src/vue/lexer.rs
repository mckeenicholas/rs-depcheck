use std::collections::VecDeque;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum HTMLToken {
    Ident(String),
    StrLit(String),
    OpenBracket,     // <
    CloseBracket,    // >
    Slash,           // /
    Assign,          // =
    Text(String),    // Text content between tags
    Comment(String), // <!-- comment -->
    Eof,
}

#[derive(Debug, Clone)]
pub struct LexerError {
    pub message: String,
    pub position: usize,
}

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Lexer error at position {}: {}",
            self.position, self.message
        )
    }
}

impl std::error::Error for LexerError {}

/// The state of the lexer, indicating whether it's inside a tag or parsing content.
#[derive(Debug, Clone, Copy, PartialEq)]
enum LexerState {
    /// The default state. The lexer is parsing text content or expecting a new tag.
    Data,
    /// The lexer is inside a tag, e.g., after '<' and before '>'.
    Tag,
}

#[derive(Debug)]
pub struct HTMLLexer {
    input: Vec<char>,
    position: usize,
    current_char: Option<char>,
    tokens: VecDeque<HTMLToken>,
    /// The current state of the lexer.
    state: LexerState,
}

impl HTMLLexer {
    pub fn new(input: &str) -> Self {
        let chars: Vec<char> = input.chars().collect();
        let current_char = chars.first().copied();

        Self {
            input: chars,
            position: 0,
            current_char,
            tokens: VecDeque::new(),
            // Start in the Data state, expecting text or tags.
            state: LexerState::Data,
        }
    }

    /// Processes the next character based on the current lexer state.
    fn lex_next(&mut self) -> Result<(), LexerError> {
        match self.state {
            LexerState::Data => self.lex_data_state(),
            LexerState::Tag => self.lex_tag_state(),
        }
    }

    /// Handles tokenization when in the `Data` state (outside of tags).
    fn lex_data_state(&mut self) -> Result<(), LexerError> {
        if self.current_char == Some('{') && self.peek() == Some('{') {
            self.read_interpolation()
        } else if self.current_char == Some('<') {
            // A '<' could be the start of a tag or a comment.
            self.handle_open_bracket()
        } else {
            // Otherwise, consume everything as a single text token until the next '<'.
            self.read_text();
            Ok(())
        }
    }

    /// Checks if a character is valid for use in an identifier (tag names, attributes).
    /// Vue attributes can contain letters, numbers, _, -, :, @, ., and #
    fn is_identifier_char(&self, ch: char) -> bool {
        ch.is_alphanumeric()
            || ch == '_'
            || ch == '-'
            || ch == ':'
            || ch == '@'
            || ch == '.'
            || ch == '#'
    }

    /// Handles tokenization when in the `Tag` state (inside `<...>`).
    fn lex_tag_state(&mut self) -> Result<(), LexerError> {
        let ch = match self.current_char {
            Some(c) => c,
            None => return Ok(()), // End of input
        };

        match ch {
            '>' => {
                self.tokens.push_back(HTMLToken::CloseBracket);
                self.advance();
                // Exiting the tag, switch back to Data state.
                self.state = LexerState::Data;
            }
            '/' => {
                self.tokens.push_back(HTMLToken::Slash);
                self.advance();
            }
            '=' => {
                self.tokens.push_back(HTMLToken::Assign);
                self.advance();
            }
            '"' | '\'' => self.read_string_literal()?,
            c if c.is_whitespace() => self.skip_whitespace(),
            // Identifiers (tag names, attributes) can start with letters, _, :, @, #
            c if c.is_alphabetic() || c == '_' || c == ':' || c == '@' || c == '#' => {
                self.read_identifier();
            }
            _ => {
                println!("{ch}, {:?}", self);

                return Err(LexerError {
                    message: format!("Unexpected character '{ch}' inside a tag"),
                    position: self.position,
                });
            }
        }
        Ok(())
    }

    /// Advances the position in the input buffer by one character.
    fn advance(&mut self) {
        if self.position < self.input.len() {
            self.position += 1;
            self.current_char = self.input.get(self.position).copied();
        } else {
            self.current_char = None;
        }
    }

    /// Peeks at the next character in the input without advancing.
    fn peek(&self) -> Option<char> {
        self.input.get(self.position + 1).copied()
    }

    /// Peeks at a character a given offset ahead in the input.
    fn peek_ahead(&self, offset: usize) -> Option<char> {
        self.input.get(self.position + offset).copied()
    }

    /// Handles a '<' character, determining if it's a tag or comment.
    fn handle_open_bracket(&mut self) -> Result<(), LexerError> {
        // Check for comments <!-- -->
        if self.peek() == Some('!')
            && self.peek_ahead(2) == Some('-')
            && self.peek_ahead(3) == Some('-')
        {
            // It's a comment. Comments are parsed but don't change the state.
            self.read_comment()?;
        } else {
            // It's a tag. Push the token and switch to the Tag state.
            self.tokens.push_back(HTMLToken::OpenBracket);
            self.advance();
            self.state = LexerState::Tag;
        }
        Ok(())
    }

    /// Reads an HTML comment from `<!--` to `-->`.
    fn read_comment(&mut self) -> Result<(), LexerError> {
        let start_position = self.position;

        // Skip "<!--"
        for _ in 0..4 {
            self.advance();
        }

        let mut comment_text = String::new();
        while let Some(ch) = self.current_char {
            if ch == '-' && self.peek() == Some('-') && self.peek_ahead(2) == Some('>') {
                // Found end of comment "-->"
                self.advance(); // Skip first '-'
                self.advance(); // Skip second '-'
                self.advance(); // Skip '>'
                self.tokens
                    .push_back(HTMLToken::Comment(comment_text.trim().to_string()));
                return Ok(());
            }
            comment_text.push(ch);
            self.advance();
        }

        Err(LexerError {
            message: "Unterminated comment".to_string(),
            position: start_position,
        })
    }

    /// Reads an interpolation from `{{` to `}}`.
    fn read_interpolation(&mut self) -> Result<(), LexerError> {
        let start_position = self.position;

        // Skip "{{"
        self.advance();
        self.advance();

        let mut content = String::new();
        while let Some(ch) = self.current_char {
            if ch == '}' && self.peek() == Some('}') {
                // Found end of interpolation "}}"
                self.advance(); // Skip first '}'
                self.advance(); // Skip second '}'
                self.tokens
                    .push_back(HTMLToken::Text(format!("{{{{ {} }}}}", content.trim())));
                return Ok(());
            }
            content.push(ch);
            self.advance();
        }

        Err(LexerError {
            message: "Unterminated interpolation".to_string(),
            position: start_position,
        })
    }

    /// Reads a string literal enclosed in single or double quotes.
    fn read_string_literal(&mut self) -> Result<(), LexerError> {
        let quote_char = self.current_char.unwrap();
        let start_position = self.position;
        self.advance(); // Skip opening quote

        let mut string_value = String::new();
        while let Some(ch) = self.current_char {
            if ch == quote_char {
                self.advance(); // Skip closing quote
                self.tokens.push_back(HTMLToken::StrLit(string_value));
                return Ok(());
            }

            // Handle escape sequences
            if ch == '\\' {
                self.advance();
                if let Some(escaped_char) = self.current_char {
                    match escaped_char {
                        'n' => string_value.push('\n'),
                        't' => string_value.push('\t'),
                        'r' => string_value.push('\r'),
                        '\\' => string_value.push('\\'),
                        '\'' => string_value.push('\''),
                        '"' => string_value.push('"'),
                        _ => {
                            // Invalid escape, treat as literal characters
                            string_value.push('\\');
                            string_value.push(escaped_char);
                        }
                    }
                    self.advance();
                } else {
                    return Err(LexerError {
                        message: "Unterminated escape sequence".to_string(),
                        position: self.position,
                    });
                }
            } else {
                string_value.push(ch);
                self.advance();
            }
        }

        Err(LexerError {
            message: "Unterminated string literal".to_string(),
            position: start_position,
        })
    }

    /// Reads an identifier, such as a tag name or attribute name.
    fn read_identifier(&mut self) {
        let mut identifier = String::new();
        while let Some(ch) = self.current_char {
            if self.is_identifier_char(ch) {
                identifier.push(ch);
                self.advance();
            } else {
                break;
            }
        }
        self.tokens.push_back(HTMLToken::Ident(identifier));
    }

    /// Reads text content between tags.
    fn read_text(&mut self) {
        let mut text = String::new();
        while let Some(ch) = self.current_char {
            if ch == '<' {
                break; // Start of a new tag
            }
            text.push(ch);
            self.advance();
        }

        // Only push a text token if it's not empty after trimming whitespace.
        let trimmed_text = text.trim();
        if !trimmed_text.is_empty() {
            self.tokens
                .push_back(HTMLToken::Text(trimmed_text.to_string()));
        }
    }

    /// Skips over consecutive whitespace characters.
    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.current_char {
            if ch.is_whitespace() {
                self.advance();
            } else {
                break;
            }
        }
    }

    /// Gets the next token for a parser.
    pub fn next_token(&mut self) -> Result<HTMLToken, LexerError> {
        // If the buffer is empty, generate more tokens.
        while self.tokens.is_empty() && self.current_char.is_some() {
            self.lex_next()?;
        }
        Ok(self.tokens.pop_front().unwrap_or(HTMLToken::Eof))
    }

    /// Consumes the entire input and collects all tokens into a vector.
    /// This method is typically used to tokenize the complete input at once.
    pub fn tokenize(&mut self) -> Result<Vec<HTMLToken>, LexerError> {
        let mut tokens = Vec::new();
        loop {
            let token = self.next_token()?;
            if token == HTMLToken::Eof {
                tokens.push(token);
                break;
            }
            tokens.push(token);
        }
        Ok(tokens)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_tag() {
        let mut lexer = HTMLLexer::new("<div></div>");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(
            tokens,
            vec![
                HTMLToken::OpenBracket,
                HTMLToken::Ident("div".to_string()),
                HTMLToken::CloseBracket,
                HTMLToken::OpenBracket,
                HTMLToken::Slash,
                HTMLToken::Ident("div".to_string()),
                HTMLToken::CloseBracket,
                HTMLToken::Eof,
            ]
        );
    }

    #[test]
    fn test_text_content() {
        let mut lexer = HTMLLexer::new("<h1>Hello World</h1>");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens,
            vec![
                HTMLToken::OpenBracket,
                HTMLToken::Ident("h1".to_string()),
                HTMLToken::CloseBracket,
                HTMLToken::Text("Hello World".to_string()),
                HTMLToken::OpenBracket,
                HTMLToken::Slash,
                HTMLToken::Ident("h1".to_string()),
                HTMLToken::CloseBracket,
                HTMLToken::Eof,
            ]
        );
    }

    #[test]
    fn test_vue_template() {
        let vue_template = r#"
            <template>
                <!-- This is a comment -->
                <div class="container" a:test="callexpr()" :self>
                    <h1 v-model:test="var">{{ title }}</h1>
                    <button @click.prevent="handleClick" boolenanItem>Click me</button>
                </div>
            </template>
        "#;

        let mut lexer = HTMLLexer::new(vue_template);
        let tokens = lexer.tokenize().unwrap();

        let expected_tokens = vec![
            HTMLToken::OpenBracket,
            HTMLToken::Ident("template".to_string()),
            HTMLToken::CloseBracket,
            HTMLToken::Comment("This is a comment".to_string()),
            HTMLToken::OpenBracket,
            HTMLToken::Ident("div".to_string()),
            HTMLToken::Ident("class".to_string()),
            HTMLToken::Assign,
            HTMLToken::StrLit("container".to_string()),
            HTMLToken::Ident("a:test".to_string()),
            HTMLToken::Assign,
            HTMLToken::StrLit("callexpr()".to_string()),
            HTMLToken::Ident(":self".to_string()),
            HTMLToken::CloseBracket,
            HTMLToken::OpenBracket,
            HTMLToken::Ident("h1".to_string()),
            HTMLToken::Ident("v-model:test".to_string()),
            HTMLToken::Assign,
            HTMLToken::StrLit("var".to_string()),
            HTMLToken::CloseBracket,
            HTMLToken::Text("{{ title }}".to_string()),
            HTMLToken::OpenBracket,
            HTMLToken::Slash,
            HTMLToken::Ident("h1".to_string()),
            HTMLToken::CloseBracket,
            HTMLToken::OpenBracket,
            HTMLToken::Ident("button".to_string()),
            HTMLToken::Ident("@click.prevent".to_string()),
            HTMLToken::Assign,
            HTMLToken::StrLit("handleClick".to_string()),
            HTMLToken::Ident("boolenanItem".to_string()),
            HTMLToken::CloseBracket,
            HTMLToken::Text("Click me".to_string()),
            HTMLToken::OpenBracket,
            HTMLToken::Slash,
            HTMLToken::Ident("button".to_string()),
            HTMLToken::CloseBracket,
            HTMLToken::OpenBracket,
            HTMLToken::Slash,
            HTMLToken::Ident("div".to_string()),
            HTMLToken::CloseBracket,
            HTMLToken::OpenBracket,
            HTMLToken::Slash,
            HTMLToken::Ident("template".to_string()),
            HTMLToken::CloseBracket,
            HTMLToken::Eof,
        ];

        // For debugging:
        if tokens != expected_tokens {
            for (i, (t, e)) in tokens.iter().zip(expected_tokens.iter()).enumerate() {
                if t != e {
                    println!("Mismatch at index {}: Got {:?}, Expected {:?}", i, t, e);
                }
            }
        }

        assert_eq!(tokens, expected_tokens);
    }
}
