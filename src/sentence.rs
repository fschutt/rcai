use crate::uuid::Uuid;
use crate::wordnet::SynsetDatabase;
use serde_derive::{Deserialize, Serialize};
use std::collections::BTreeMap;
use wn_parser::common::SynsetType;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TokenType {
    Noun,
    Verb,
    Adjective,
    Adverb,
    Determiner,
    Preposition,
    Conjunction,
    Punctuation,
    Unknown,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Token {
    pub text: String,
    pub normalized_text: String,
    pub token_type: TokenType,
    pub uuid: Option<Uuid>,
    pub instance: Option<Uuid>,
    pub modifiers: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum SentenceRole {
    Subject,
    Verb { tense: Tense },
    Object,
    Modifier,
}

impl SentenceRole {
    pub fn is_subject(&self) -> bool {
        *self == SentenceRole::Subject
    }
    pub fn is_verb(&self) -> bool {
        match self {
            Self::Verb { .. } => true,
            _ => false,
        }
    }
    pub fn is_object(&self) -> bool {
        *self == SentenceRole::Object
    }
    pub fn is_modifier(&self) -> bool {
        *self == SentenceRole::Modifier
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Tense {
    Infinitive,
    PresentSimple,
    PresentContinuous,
    PresentPerfect,
    PresentPerfectContinuous,
    PastSimple,
    PastContinuous,
    PastPerfect,
    PastPerfectContinuous,
    FutureSimple,
    FutureContinuous,
    FuturePerfect,
    FuturePerfectContinuous,
    ConditionalSimple,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ParsedToken {
    pub token: Token,
    pub role: SentenceRole,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ParsedSentence {
    pub tokens: Vec<ParsedToken>,
}

impl ParsedSentence {
    pub fn new(sentence: &str, db: &SynsetDatabase) -> Result<Self, String> {
        // Tokenize the sentence
        let tokens = tokenize(sentence, db)?;

        // Assign roles to tokens
        let parsed_tokens = assign_roles(tokens, db)?;

        Ok(Self {
            tokens: parsed_tokens,
        })
    }

    pub fn to_string(&self) -> String {
        let subject = self.tokens.iter().find(|t| t.role.is_subject());
        let verb = self.tokens.iter().find(|t| t.role.is_verb());
        let object = self.tokens.iter().find(|t| t.role.is_object());

        let mut result = String::new();

        if let Some(subject) = subject {
            result.push_str(&format!(
                "{}\tNOUN\tUUID {} INSTANCE {} MODIFIERS {} TYPE subject\n",
                subject.token.text,
                subject
                    .token
                    .uuid
                    .map_or("None".to_string(), |u| u.to_string()),
                subject
                    .token
                    .instance
                    .map_or("None".to_string(), |u| u.to_string()),
                subject.token.modifiers.join(" ")
            ));
        }

        if let Some(verb) = verb {
            // Determine if we have a past tense verb
            let tense = if verb.token.text != verb.token.normalized_text {
                "past"
            } else {
                "present"
            };

            result.push_str(&format!(
                "{}\tVERB\tUUID {} INSTANCE {} MODIFIERS {} TENSE {}\n",
                verb.token.normalized_text,
                verb.token
                    .uuid
                    .map_or("None".to_string(), |u| u.to_string()),
                verb.token
                    .instance
                    .map_or("None".to_string(), |u| u.to_string()),
                verb.token.modifiers.join(" "),
                tense,
            ));
        }

        if let Some(object) = object {
            result.push_str(&format!(
                "{}\tNOUN\tUUID {} INSTANCE {} MODIFIERS {} TYPE object\n",
                object.token.text,
                object
                    .token
                    .uuid
                    .map_or("None".to_string(), |u| u.to_string()),
                object
                    .token
                    .instance
                    .map_or("None".to_string(), |u| u.to_string()),
                object.token.modifiers.join(" ")
            ));
        }

        result
    }
}

// Lists of common words by type
const DETERMINERS: &[&str] = &[
    "a", "an", "the", "this", "that", "these", "those", "my", "your", "his", "her", "its", "our",
    "their",
];
const PREPOSITIONS: &[&str] = &[
    "in", "on", "at", "by", "with", "from", "to", "for", "of", "about",
];
const CONJUNCTIONS: &[&str] = &["and", "but", "or", "nor", "for", "yet", "so"];

fn tokenize(sentence: &str, db: &SynsetDatabase) -> Result<Vec<Token>, String> {
    let token_strings = crate::text::tokenize_sentence(sentence);

    let mut tokens = Vec::new();
    let mut adj_or_adv_positions = BTreeMap::new();

    // First pass: identify token types and record positions of adjectives/adverbs
    for (i, token_str) in token_strings.iter().enumerate() {
        let token_lower = token_str.to_lowercase();

        let token_type = if token_str.chars().all(|c| c.is_ascii_punctuation()) {
            TokenType::Punctuation
        } else if DETERMINERS.contains(&token_lower.as_str()) {
            TokenType::Determiner
        } else if PREPOSITIONS.contains(&token_lower.as_str()) {
            TokenType::Preposition
        } else if CONJUNCTIONS.contains(&token_lower.as_str()) {
            TokenType::Conjunction
        } else {
            // Try to find in WordNet
            let base_form = db.get_base_form(&token_lower).0;

            let mut synsets = db.find_synsets_by_word(&base_form);

            // If nothing found, check for irregular verbs
            if synsets.is_empty() && token_lower != base_form {
                synsets = db.find_synsets_by_word(&db.get_base_form(&token_lower).0);
            }

            if synsets.is_empty() {
                TokenType::Unknown
            } else {
                // Count occurrences of each type
                let mut type_counts: BTreeMap<SynsetType, usize> = BTreeMap::new();
                for (_, _, syn_type) in &synsets {
                    *type_counts.entry(syn_type.clone()).or_insert(0) += 1;
                }

                // Find the most common type
                let most_common = type_counts
                    .iter()
                    .max_by_key(|(_, &count)| count)
                    .map(|(syn_type, _)| syn_type.clone())
                    .unwrap_or_else(|| SynsetType::Unknown(String::new()));

                match most_common {
                    SynsetType::Noun => TokenType::Noun,
                    SynsetType::Verb => TokenType::Verb,
                    SynsetType::Adjective | SynsetType::AdjectiveSatellite => {
                        adj_or_adv_positions.insert(i, TokenType::Adjective);
                        TokenType::Adjective
                    }
                    SynsetType::Adverb => {
                        adj_or_adv_positions.insert(i, TokenType::Adverb);
                        TokenType::Adverb
                    }
                    _ => TokenType::Unknown,
                }
            }
        };

        tokens.push(Token {
            text: token_str.clone(),
            normalized_text: db.get_base_form(token_str).0,
            token_type,
            uuid: None,
            instance: None,
            modifiers: Vec::new(),
        });
    }

    // Second pass: assign UUIDs and find modifiers
    for i in 0..tokens.len() {
        let mut token = tokens[i].clone();

        // Assign UUID for nouns and verbs
        if token.token_type == TokenType::Noun || token.token_type == TokenType::Verb {
            let word_to_lookup = if token.token_type == TokenType::Verb {
                token.normalized_text.clone()
            } else {
                token.text.to_lowercase()
            };

            let synsets = db.find_synsets_by_word(&word_to_lookup);
            if let Some((uuid, _, _)) = synsets.first() {
                token.uuid = Some(*uuid);
                token.instance = Some(Uuid::new());
            }
        }

        // Find modifiers (adjectives before nouns, adverbs before verbs)
        if token.token_type == TokenType::Noun {
            // Look backwards for adjectives
            for j in (0..i).rev() {
                if let Some(TokenType::Adjective) = adj_or_adv_positions.get(&j) {
                    token.modifiers.push(tokens[j].text.clone());
                } else if tokens[j].token_type != TokenType::Determiner {
                    // Stop at non-adjectives and non-determiners
                    break;
                }
            }
        } else if token.token_type == TokenType::Verb {
            // Look backwards for adverbs
            for j in (0..i).rev() {
                if let Some(TokenType::Adverb) = adj_or_adv_positions.get(&j) {
                    token.modifiers.push(tokens[j].text.clone());
                } else {
                    // Stop at non-adverbs
                    break;
                }
            }
        }

        tokens[i] = token;
    }

    Ok(tokens)
}

fn assign_roles(tokens: Vec<Token>, db: &SynsetDatabase) -> Result<Vec<ParsedToken>, String> {
    let mut parsed_tokens = Vec::new();
    let mut found_verb = false;
    let mut found_subject = false;
    let mut found_object = false;

    // Simple SVO (Subject-Verb-Object) parsing
    for token in tokens {
        let role = match token.token_type {
            TokenType::Noun if !found_verb && !found_subject => {
                found_subject = true;
                SentenceRole::Subject
            }
            TokenType::Verb if !found_verb => {
                found_verb = true;
                SentenceRole::Verb {
                    tense: db.get_base_form(&token.normalized_text).1,
                }
            }
            TokenType::Noun if found_verb && !found_object => {
                found_object = true;
                SentenceRole::Object
            }
            _ => SentenceRole::Modifier,
        };

        parsed_tokens.push(ParsedToken { token, role });
    }

    Ok(parsed_tokens)
}
