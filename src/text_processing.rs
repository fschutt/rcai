// src/text_processing.rs
use std::fs;

/// Load text from a file
pub fn load_text_file(path: &str) -> Result<String, String> {
    fs::read_to_string(path).map_err(|e| e.to_string())
}

/// Split text into sentences
pub fn split_into_sentences(text: &str) -> Vec<String> {
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
    
    sentences
}

/// Clean text by removing special characters and normalizing whitespace
pub fn clean_text(text: &str) -> String {
    let mut result = String::with_capacity(text.len());
    let mut last_was_space = true;
    
    for c in text.chars() {
        if c.is_alphanumeric() || c == '\'' || c == '-' || c == '.' || c == '!' || c == '?' || c == ',' || c == ':' || c == ';' || c == ' ' {
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
    text.split(|c: char| c.is_whitespace() || c == '.' || c == ',' || c == '!' || c == '?' || c == ':' || c == ';')
        .filter(|s| !s.is_empty())
        .map(|s| s.to_lowercase())
        .collect()
}

/// Check if a word is a common stopword
pub fn is_stopword(word: &str) -> bool {
    static STOPWORDS: &[&str] = &[
        "the", "and", "a", "an", "of", "to", "in", "for", "with", "on",
        "at", "from", "by", "as", "is", "are", "was", "were", "be", "been",
        "being", "have", "has", "had", "do", "does", "did", "will", "would",
        "shall", "should", "may", "might", "must", "can", "could", "that",
        "this", "these", "those", "it", "its", "they", "them", "their", "he",
        "him", "his", "she", "her", "we", "us", "our", "you", "your", "i", "my",
    ];
    
    STOPWORDS.contains(&word)
}