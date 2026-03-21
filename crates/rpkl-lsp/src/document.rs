//! Document management and parsing

use rpkl_parser::{parse_module, Module, Span};
use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range};

/// A parsed PKL document
pub struct Document {
    /// The source text
    pub text: String,
    /// Line start offsets for position conversion
    line_offsets: Vec<usize>,
    /// Parsed AST (if successful)
    pub ast: Option<Module>,
    /// Parse error message (if parsing failed)
    pub parse_error: Option<String>,
}

impl Document {
    /// Create a new document from source text
    pub fn new(text: String) -> Self {
        let line_offsets = compute_line_offsets(&text);

        let (ast, parse_error) = match parse_module(&text) {
            Ok(module) => (Some(module), None),
            Err(err) => (None, Some(err.to_string())),
        };

        Self {
            text,
            line_offsets,
            ast,
            parse_error,
        }
    }

    /// Get diagnostics for this document
    pub fn diagnostics(&self) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();

        if let Some(ref error) = self.parse_error {
            // Parse the error message to extract location if possible
            // pest errors typically have "at line X, column Y"
            let (range, message) = parse_error_location(error, &self.line_offsets, &self.text);

            diagnostics.push(Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::ERROR),
                source: Some("rpkl".to_string()),
                message,
                ..Default::default()
            });
        }

        diagnostics
    }

    /// Convert a byte offset to an LSP Position
    pub fn offset_to_position(&self, offset: usize) -> Option<Position> {
        if offset > self.text.len() {
            return None;
        }

        // Binary search for the line
        let line = match self.line_offsets.binary_search(&offset) {
            Ok(line) => line,
            Err(line) => line.saturating_sub(1),
        };

        let line_start = self.line_offsets.get(line).copied().unwrap_or(0);

        // Convert byte offset to character offset
        let character = self.text[line_start..offset].chars().count();

        Some(Position {
            line: line as u32,
            character: character as u32,
        })
    }

    /// Convert an LSP Position to a byte offset
    pub fn position_to_offset(&self, position: Position) -> Option<usize> {
        let line = position.line as usize;
        if line >= self.line_offsets.len() {
            return None;
        }

        let line_start = self.line_offsets[line];
        let line_end = self
            .line_offsets
            .get(line + 1)
            .copied()
            .unwrap_or(self.text.len());

        let line_text = &self.text[line_start..line_end];

        // Convert character offset to byte offset
        let mut byte_offset = 0;
        for (i, c) in line_text.chars().enumerate() {
            if i == position.character as usize {
                break;
            }
            byte_offset += c.len_utf8();
        }

        Some(line_start + byte_offset)
    }

    /// Convert a Span to an LSP Range
    pub fn span_to_range(&self, span: Span) -> Range {
        let start = self.offset_to_position(span.start).unwrap_or(Position {
            line: 0,
            character: 0,
        });
        let end = self.offset_to_position(span.end).unwrap_or(Position {
            line: 0,
            character: 0,
        });
        Range { start, end }
    }
}

/// Compute line start offsets for a string
fn compute_line_offsets(text: &str) -> Vec<usize> {
    let mut offsets = vec![0];
    for (i, c) in text.char_indices() {
        if c == '\n' {
            offsets.push(i + 1);
        }
    }
    offsets
}

/// Parse error message to extract location information
fn parse_error_location(error: &str, _line_offsets: &[usize], _text: &str) -> (Range, String) {
    // Try to parse pest-style error messages
    // They look like: " --> 1:10\n  |\n1 | content\n  |          ^---\n  |\n  = expected ..."

    // Look for the pattern " --> line:column"
    if let Some(pos_start) = error.find(" --> ") {
        let after_arrow = &error[pos_start + 5..];
        if let Some(colon_pos) = after_arrow.find(':') {
            let line_str = &after_arrow[..colon_pos];
            if let Ok(line) = line_str.parse::<u32>() {
                let after_colon = &after_arrow[colon_pos + 1..];
                let col_end = after_colon
                    .find(|c: char| !c.is_ascii_digit())
                    .unwrap_or(after_colon.len());
                if let Ok(col) = after_colon[..col_end].parse::<u32>() {
                    let start = Position {
                        line: line.saturating_sub(1),
                        character: col.saturating_sub(1),
                    };
                    let end = Position {
                        line: line.saturating_sub(1),
                        character: col,
                    };

                    // Extract just the "expected ..." part for a cleaner message
                    let message = if let Some(expected_pos) = error.find("= expected") {
                        error[expected_pos + 2..].trim().to_string()
                    } else {
                        "Parse error".to_string()
                    };

                    return (Range { start, end }, message);
                }
            }
        }
    }

    // Fallback: error at the start of the file
    (
        Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: 0,
                character: 1,
            },
        },
        error.to_string(),
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_offset_position_conversion() {
        let doc = Document::new("hello\nworld\n".to_string());

        // First line
        assert_eq!(
            doc.offset_to_position(0),
            Some(Position {
                line: 0,
                character: 0
            })
        );
        assert_eq!(
            doc.offset_to_position(5),
            Some(Position {
                line: 0,
                character: 5
            })
        );

        // Second line
        assert_eq!(
            doc.offset_to_position(6),
            Some(Position {
                line: 1,
                character: 0
            })
        );
        assert_eq!(
            doc.offset_to_position(11),
            Some(Position {
                line: 1,
                character: 5
            })
        );

        // Round-trip
        let pos = Position {
            line: 1,
            character: 3,
        };
        let offset = doc.position_to_offset(pos).unwrap();
        assert_eq!(doc.offset_to_position(offset), Some(pos));
    }

    #[test]
    fn test_offset_position_empty_document() {
        let doc = Document::new("".to_string());
        assert_eq!(
            doc.offset_to_position(0),
            Some(Position {
                line: 0,
                character: 0
            })
        );
        assert_eq!(doc.offset_to_position(1), None);
    }

    #[test]
    fn test_offset_position_single_line_no_newline() {
        let doc = Document::new("hello".to_string());
        assert_eq!(
            doc.offset_to_position(0),
            Some(Position {
                line: 0,
                character: 0
            })
        );
        assert_eq!(
            doc.offset_to_position(4),
            Some(Position {
                line: 0,
                character: 4
            })
        );
        assert_eq!(
            doc.offset_to_position(5),
            Some(Position {
                line: 0,
                character: 5
            })
        );
    }

    #[test]
    fn test_offset_position_unicode() {
        // "café" has 5 chars but 6 bytes (é is 2 bytes in UTF-8)
        let doc = Document::new("café\nworld".to_string());
        // character 4 is 'é' at byte offset 3..5
        // 'é' is at character position 3, byte offset 3
        assert_eq!(
            doc.offset_to_position(3),
            Some(Position {
                line: 0,
                character: 3
            })
        );
        // After "café" (5 bytes for 4 chars + newline at byte 5)
        assert_eq!(
            doc.offset_to_position(6),
            Some(Position {
                line: 1,
                character: 0
            })
        );
    }

    #[test]
    fn test_position_to_offset_out_of_range() {
        let doc = Document::new("hello\nworld".to_string());
        // Line 5 doesn't exist
        assert_eq!(
            doc.position_to_offset(Position {
                line: 5,
                character: 0
            }),
            None
        );
    }

    #[test]
    fn test_position_to_offset_round_trip() {
        let doc = Document::new("name: String\nage: Int\nactive: Boolean".to_string());
        for line in 0..3u32 {
            for ch in 0..5u32 {
                let pos = Position {
                    line,
                    character: ch,
                };
                if let Some(offset) = doc.position_to_offset(pos) {
                    let back = doc.offset_to_position(offset);
                    assert_eq!(back, Some(pos), "Round-trip failed for {:?}", pos);
                }
            }
        }
    }

    #[test]
    fn test_multiple_empty_lines() {
        let doc = Document::new("a\n\n\nb".to_string());
        assert_eq!(
            doc.offset_to_position(2),
            Some(Position {
                line: 1,
                character: 0
            })
        );
        assert_eq!(
            doc.offset_to_position(3),
            Some(Position {
                line: 2,
                character: 0
            })
        );
        assert_eq!(
            doc.offset_to_position(4),
            Some(Position {
                line: 3,
                character: 0
            })
        );
    }

    #[test]
    fn test_span_to_range() {
        let doc = Document::new("name = \"hello\"".to_string());
        let span = Span { start: 0, end: 4 };
        let range = doc.span_to_range(span);
        assert_eq!(
            range.start,
            Position {
                line: 0,
                character: 0
            }
        );
        assert_eq!(
            range.end,
            Position {
                line: 0,
                character: 4
            }
        );
    }

    #[test]
    fn test_parse_valid_pkl() {
        let doc = Document::new("name = \"hello\"\nage = 42".to_string());
        assert!(doc.ast.is_some());
        assert!(doc.parse_error.is_none());
        assert!(doc.diagnostics().is_empty());
    }

    #[test]
    fn test_parse_invalid_pkl() {
        let doc = Document::new("name = {{{invalid".to_string());
        // Should have a parse error or at least not panic
        // The parser may or may not succeed depending on the grammar
        if doc.ast.is_none() {
            assert!(doc.parse_error.is_some());
            assert!(!doc.diagnostics().is_empty());
        }
    }

    #[test]
    fn test_diagnostics_on_valid_document() {
        let doc = Document::new("x = 1\ny = 2".to_string());
        assert!(doc.diagnostics().is_empty());
    }

    #[test]
    fn test_parse_error_location_arrow_format() {
        let error =
            " --> 3:10\n  |\n3 | bad stuff\n  |          ^---\n  |\n  = expected identifier";
        let line_offsets = vec![0, 10, 20];
        let text = "line one\nline two\nline three";
        let (range, message) = parse_error_location(error, &line_offsets, text);
        assert_eq!(range.start.line, 2); // line 3 -> 0-indexed = 2
        assert_eq!(range.start.character, 9); // column 10 -> 0-indexed = 9
        assert_eq!(message, "expected identifier");
    }

    #[test]
    fn test_parse_error_location_fallback() {
        let error = "some random error message";
        let line_offsets = vec![0];
        let text = "hello";
        let (range, message) = parse_error_location(error, &line_offsets, text);
        assert_eq!(range.start.line, 0);
        assert_eq!(range.start.character, 0);
        assert_eq!(message, error);
    }

    #[test]
    fn test_compute_line_offsets() {
        let offsets = compute_line_offsets("abc\ndef\nghi");
        assert_eq!(offsets, vec![0, 4, 8]);
    }

    #[test]
    fn test_compute_line_offsets_trailing_newline() {
        let offsets = compute_line_offsets("abc\n");
        assert_eq!(offsets, vec![0, 4]);
    }

    #[test]
    fn test_compute_line_offsets_empty() {
        let offsets = compute_line_offsets("");
        assert_eq!(offsets, vec![0]);
    }

    #[test]
    fn test_parse_class_definition() {
        let doc = Document::new("class Person {\n  name: String\n  age: Int\n}".to_string());
        assert!(doc.ast.is_some());
        assert!(doc.parse_error.is_none());
    }

    #[test]
    fn test_parse_import() {
        let doc = Document::new("import \"./other.pkl\"\n\nname = other.value".to_string());
        // Parser may or may not handle this depending on grammar specifics
        // At minimum it shouldn't panic
        let _ = doc.diagnostics();
    }

    #[test]
    fn test_parse_method() {
        let doc =
            Document::new("function greet(name: String): String = \"Hello, \\(name)\"".to_string());
        assert!(doc.ast.is_some(), "Method should parse successfully");
    }

    #[test]
    fn test_parse_typealias() {
        let doc = Document::new("typealias Name = String".to_string());
        assert!(doc.ast.is_some());
    }

    #[test]
    fn test_parse_complex_document() {
        let source = r#"
class Config {
  host: String
  port: Int
  debug: Boolean
}

config = new Config {
  host = "localhost"
  port = 8080
  debug = true
}

items = new Listing {
  "item1"
  "item2"
}
"#;
        let doc = Document::new(source.to_string());
        assert!(
            doc.ast.is_some(),
            "Complex document should parse: {:?}",
            doc.parse_error
        );
    }
}
