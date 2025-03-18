use crate::sentence::ParsedSentence;
use crate::wordnet::SynsetDatabase;
use serde_derive::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Debug, PartialEq, PartialOrd, Ord, Eq, Serialize, Deserialize)]
pub struct Paragraph {
    pub heading: Option<String>,
    pub text: Text,
}

#[derive(Debug, PartialEq, PartialOrd, Ord, Eq, Serialize, Deserialize)]
pub struct Text {
    pub sentences: Vec<String>,
    pub parsed_sentences: Option<Vec<ParsedSentence>>,
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
            parsed_sentences: None,
        }
    }

    /// Parse all sentences with linguistic analysis
    pub fn parse_linguistically(&mut self, db: &SynsetDatabase) -> Result<(), String> {
        let mut parsed = Vec::new();

        for sentence in &self.sentences {
            match ParsedSentence::new(sentence, db) {
                Ok(parsed_sentence) => parsed.push(parsed_sentence),
                Err(e) => return Err(format!("Error parsing sentence '{}': {}", sentence, e)),
            }
        }

        self.parsed_sentences = Some(parsed);
        Ok(())
    }

    /// Find references between sentences (anaphora resolution)
    pub fn resolve_references(&mut self) -> Result<(), String> {
        if self.parsed_sentences.is_none() {
            return Err("Sentences must be parsed first".to_string());
        }
        
        // Track potential referents across sentences
        let mut potential_referents = HashMap::new();
        
        if let Some(ref mut parsed_sentences) = self.parsed_sentences {
            let ps_len = parsed_sentences.len();
            for i in 0..ps_len {
                // Extract subjects from current sentence
                let cur_subjects = parsed_sentences[i].get_subjects().into_iter().cloned().collect::<Vec<_>>();
                for subject in cur_subjects.iter() {
                    if let Some(instance) = subject.token.instance {
                        potential_referents.insert(subject.token.text.to_lowercase(), instance);
                    }
                }
                
                // Extract objects from current sentence
                for object in parsed_sentences[i].get_objects() {
                    if let Some(instance) = object.token.instance {
                        potential_referents.insert(object.token.text.to_lowercase(), instance);
                    }
                }
                
                // Try to resolve pronouns in next sentence
                if i < ps_len - 1 {
                    for clause in &mut parsed_sentences[i + 1].clauses {
                        for token in &clause.tokens {
                            if token.token.token_type == crate::sentence::TokenType::Pronoun {
                                let pronoun = token.token.text.to_lowercase();
                                
                                // Basic pronoun resolution - can be improved
                                if pronoun == "it" || pronoun == "this" || pronoun == "that" {
                                    // For demonstratives, probably referring to previous concept
                                    if let Some(last_subject) = cur_subjects.first() {
                                        if let Some(instance) = last_subject.token.instance {
                                            // Link the pronoun to this referent
                                            if let Some(ref mut references) = clause.reference {
                                                // Update the reference
                                                // TODO: doesn't update the reference
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        
        Ok(())
    }

    /// Perform causal analysis on the text
    pub fn analyze_causality(&self) -> Vec<CausalRelationship> {
        let mut relationships = Vec::new();
        
        // This is a placeholder for more sophisticated causal analysis
        // In a real implementation, we would:
        // 1. Identify causal terms ("because", "since", "therefore", etc.)
        // 2. Track state changes mentioned in sentences
        // 3. Identify temporal ordering
        // 4. Build causal graphs
        
        if let Some(ref parsed_sentences) = self.parsed_sentences {
            for (i, sentence) in parsed_sentences.iter().enumerate() {
                // Look for causality markers in each clause
                for clause in &sentence.clauses {
                    for token in &clause.tokens {
                        // Check for causal verbs
                        if token.token.normalized_text == "cause" || 
                           token.token.normalized_text == "lead" ||
                           token.token.normalized_text == "result" {
                            // Extract cause and effect
                            if let Some(subject) = clause.tokens.iter().find(|t| t.role.is_subject()) {
                                if let Some(object) = clause.tokens.iter().find(|t| t.role.is_object()) {
                                    relationships.push(CausalRelationship {
                                        cause: subject.token.text.clone(),
                                        effect: object.token.text.clone(),
                                        relationship_type: RelationshipType::Direct,
                                    });
                                }
                            }
                        }
                        
                        // Check for words indicating state changes
                        if token.token.normalized_text == "supersede" || 
                           token.token.normalized_text == "replace" ||
                           token.token.normalized_text == "substitute" {
                            // This indicates a replacement relationship
                            if let Some(subject) = clause.tokens.iter().find(|t| t.role.is_subject()) {
                                if let Some(object) = clause.tokens.iter().find(|t| t.role.is_object()) {
                                    relationships.push(CausalRelationship {
                                        cause: subject.token.text.clone(),
                                        effect: format!("{} replaces {}", subject.token.text, object.token.text),
                                        relationship_type: RelationshipType::Replacement,
                                    });
                                }
                            }
                        }
                    }
                }
            }
        }
        
        relationships
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CausalRelationship {
    pub cause: String,
    pub effect: String,
    pub relationship_type: RelationshipType,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum RelationshipType {
    Direct,       // A causes B
    Enablement,   // A enables B
    Prevention,   // A prevents B
    Replacement,  // A replaces B
    Temporal,     // A happens before B
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