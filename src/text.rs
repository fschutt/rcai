use serde_derive::{Deserialize, Serialize};

#[derive(Debug, PartialEq, PartialOrd, Ord, Eq, Serialize, Deserialize)]
pub struct Paragraph {
    pub heading: Option<String>,
    pub text: Text,
}

#[derive(Debug, PartialEq, PartialOrd, Ord, Eq, Serialize, Deserialize)]
pub struct Text {
    pub sentences: Vec<String>,
}

impl Text {
    /// Split text into sentences
    pub fn parse(text: &str) -> Self {
        let mut sentences = Vec::new();
        let mut current_sentence = String::new();
        let mut within_quote = false;

        for c in text.chars() {
            if c == '"' || c == '\'' || c == '"' {
                within_quote = !within_quote;
                current_sentence.push(c);
            } else if (c == '.' || c == '!' || c == '?') && !within_quote {
                current_sentence.push(c);
                if !current_sentence.trim().is_empty() {
                    sentences.push(current_sentence.trim().to_string());
                }
                current_sentence = String::new();
            } else {
                current_sentence.push(c);
            }
        }

        // Add the last sentence if it's not empty
        if !current_sentence.trim().is_empty() {
            sentences.push(current_sentence.trim().to_string());
        }

        Self {
            sentences: sentences.iter().map(|c| clean_text(c)).collect(),
        }
    }
}

/// Clean text by removing special characters and normalizing whitespace
fn clean_text(text: &str) -> String {
    let mut result = String::with_capacity(text.len());
    let mut last_was_space = true;

    for c in text.chars() {
        if c.is_alphanumeric()
            || c == '\''
            || c == '-'
            || c == '.'
            || c == '!'
            || c == '?'
            || c == ','
            || c == ':'
            || c == ';'
            || c == ' '
        {
            if c == ' ' {
                if !last_was_space {
                    result.push(c);
                    last_was_space = true;
                }
            } else {
                result.push(c);
                last_was_space = false;
            }
        }
    }

    result.trim().to_string()
}

/// Tokenize text into words
pub fn tokenize(text: &str) -> Vec<String> {
    text.split(|c: char| {
        c.is_whitespace() || c == '.' || c == ',' || c == '!' || c == '?' || c == ':' || c == ';'
    })
    .filter(|s| !s.is_empty())
    .map(|s| s.to_lowercase())
    .collect()
}

/// Check if a word is a common stopword
pub fn is_stopword(word: &str) -> bool {
    static STOPWORDS: &[&str] = &[
        "the", "and", "a", "an", "of", "to", "in", "for", "with", "on", "at", "from", "by", "as",
        "is", "are", "was", "were", "be", "been", "being", "have", "has", "had", "do", "does",
        "did", "will", "would", "shall", "should", "may", "might", "must", "can", "could", "that",
        "this", "these", "those", "it", "its", "they", "them", "their", "he", "him", "his", "she",
        "her", "we", "us", "our", "you", "your", "i", "my",
    ];

    STOPWORDS.contains(&word)
}

pub fn tokenize_sentence(sentence: &str) -> Vec<String> {
    let mut tokens = Vec::new();
    let mut current_token = String::new();
    let mut in_word = false;

    for c in sentence.chars() {
        if c.is_alphanumeric() || c == '_' || c == '\'' {
            // Word character
            if !in_word && !current_token.is_empty() {
                // Was in punctuation, now in word - emit punctuation token
                tokens.push(current_token);
                current_token = String::new();
            }
            current_token.push(c);
            in_word = true;
        } else if ".,:;!?".contains(c) {
            // Punctuation to capture as separate token
            if !current_token.is_empty() {
                // Emit the previous token
                tokens.push(current_token);
                current_token = String::new();
            }
            // Emit the punctuation as its own token
            tokens.push(c.to_string());
            in_word = false;
        } else {
            // Whitespace or other character to ignore
            if !current_token.is_empty() {
                // Emit the previous token
                tokens.push(current_token);
                current_token = String::new();
            }
            in_word = false;
        }
    }

    // Don't forget the last token
    if !current_token.is_empty() {
        tokens.push(current_token);
    }

    tokens
}
