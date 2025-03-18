use crate::uuid::Uuid;
use crate::wordnet::SynsetDatabase;
use serde_derive::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashSet};
use wn_parser::common::SynsetType;

#[derive(Debug, Clone, PartialOrd, PartialEq, Eq, Ord, Serialize, Deserialize)]
pub enum TokenType {
    Noun,
    Verb,
    Adjective,
    Adverb,
    Determiner,
    Preposition,
    Conjunction,
    CoordinatingConjunction,
    SubordinatingConjunction,
    Pronoun,
    Punctuation,
    Unknown,
}

#[derive(Debug, Clone, PartialOrd, PartialEq, Eq, Ord, Serialize, Deserialize)]
pub struct Token {
    pub text: String,
    pub normalized_text: String,
    pub token_type: TokenType,
    pub uuid: Option<Uuid>,
    pub instance: Option<Uuid>,
    pub modifiers: Vec<String>,
}

#[derive(Debug, Clone, PartialOrd, PartialEq, Eq, Ord, Serialize, Deserialize)]
pub enum SentenceRole {
    Subject,
    Verb { tense: Tense },
    Object,
    IndirectObject,
    Modifier,
    PrepositionalPhrase { preposition: String },
}

#[derive(Debug, Clone, PartialOrd, PartialEq, Eq, Ord, Serialize, Deserialize)]
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

#[derive(Debug, Clone, PartialOrd, PartialEq, Eq, Ord, Serialize, Deserialize)]
pub struct ParsedToken {
    pub token: Token,
    pub role: SentenceRole,
}

#[derive(Debug, Clone, PartialOrd, PartialEq, Eq, Ord, Serialize, Deserialize)]
pub struct Clause {
    pub tokens: Vec<ParsedToken>,
    pub clause_type: ClauseType,
    pub reference: Option<ReferenceType>,
}

#[derive(Debug, Clone, PartialOrd, PartialEq, Eq, Ord, Serialize, Deserialize)]
pub enum AdverbialClauseType {
    Time,        // when, after, before, as, while, until, since
    Condition,   // if, unless, provided that
    Purpose,     // so that, in order that
    Reason,      // because, since, as
    Concession,  // although, though, even though
    Place,       // where, wherever
    Comparison,  // as, than, as if, as though
    Manner,      // as, the way, how
    Result,      // so that, such that
    Frequency,   // whenever, every time
}

#[derive(Debug, Clone, PartialOrd, PartialEq, Eq, Ord, Serialize, Deserialize)]
pub enum ClauseType {
    Main,
    Subordinate { conjunction: Option<String> },
    Coordinate { conjunction: String },
    Adverbial { 
        clause_type: AdverbialClauseType,
        conjunction: String,
    },
}

#[derive(Debug, Clone, PartialOrd, PartialEq, Eq, Ord, Serialize, Deserialize)]
pub enum ReferenceType {
    Pronoun { referent: Uuid }, // Points to instance UUID of the referent
    Demonstrative { referent: Uuid }, // For "this", "that", etc.
}

#[derive(Debug, Clone, PartialOrd, PartialEq, Eq, Ord, Serialize, Deserialize)]
pub struct ParsedSentence {
    pub clauses: Vec<Clause>,
    pub references: BTreeMap<Uuid, Uuid>, // Maps pronouns to their referents
}

impl ParsedSentence {
    pub fn new(sentence: &str, db: &SynsetDatabase) -> Result<Self, String> {
        // Tokenize the sentence
        let tokens = tokenize(sentence, db)?;

        // Split into clauses
        let clauses = split_into_clauses(tokens, db)?;
        
        // Resolve references between clauses
        let references = resolve_references(&clauses);

        Ok(Self {
            clauses,
            references,
        })
    }

    pub fn to_string(&self) -> String {
        let mut result = String::new();

        for (i, clause) in self.clauses.iter().enumerate() {
            result.push_str(&format!("Clause {}: {}\n", i + 1, match &clause.clause_type {
                ClauseType::Main => "Main".to_string(),
                ClauseType::Subordinate { conjunction: Some(ref conj) } => format!("Subordinate ({})", conj),
                ClauseType::Subordinate { conjunction: None } => "Subordinate".to_string(),
                ClauseType::Coordinate { conjunction } => format!("Coordinate ({})", conjunction),
                ClauseType::Adverbial { conjunction, clause_type } => format!("Adverbial - {:?} ({})", clause_type, conjunction),
            }));

            // Find subjects, verbs, objects in the clause
            let subject = clause.tokens.iter().find(|t| matches!(t.role, SentenceRole::Subject));
            let verb = clause.tokens.iter().find(|t| matches!(t.role, SentenceRole::Verb { .. }));
            let object = clause.tokens.iter().find(|t| matches!(t.role, SentenceRole::Object));

            if let Some(subject) = subject {
                let subject_text = match subject.token.token_type {
                    TokenType::Pronoun => {
                        if let Some(referent) = clause.reference.as_ref() {
                            match referent {
                                ReferenceType::Pronoun { referent } | ReferenceType::Demonstrative { referent } => {
                                    format!("{}*", subject.token.text)
                                }
                            }
                        } else {
                            subject.token.text.clone()
                        }
                    },
                    _ => subject.token.text.clone()
                };

                result.push_str(&format!(
                    "  {}\tNOUN\tUUID {} INSTANCE {} MODIFIERS {} TYPE subject\n",
                    subject_text,
                    subject.token.uuid.map_or("None".to_string(), |u| u.to_string()),
                    subject.token.instance.map_or("None".to_string(), |u| u.to_string()),
                    subject.token.modifiers.join(" ")
                ));
            }

            if let Some(verb) = verb {
                // Determine tense
                let tense = if let SentenceRole::Verb { tense } = &verb.role {
                    format!("{:?}", tense)
                } else {
                    "unknown".to_string()
                };

                result.push_str(&format!(
                    "  {}\tVERB\tUUID {} INSTANCE {} MODIFIERS {} TENSE {}\n",
                    verb.token.normalized_text,
                    verb.token.uuid.map_or("None".to_string(), |u| u.to_string()),
                    verb.token.instance.map_or("None".to_string(), |u| u.to_string()),
                    verb.token.modifiers.join(" "),
                    tense.to_lowercase(),
                ));
            }

            if let Some(object) = object {
                result.push_str(&format!(
                    "  {}\tNOUN\tUUID {} INSTANCE {} MODIFIERS {} TYPE object\n",
                    object.token.text,
                    object.token.uuid.map_or("None".to_string(), |u| u.to_string()),
                    object.token.instance.map_or("None".to_string(), |u| u.to_string()),
                    object.token.modifiers.join(" ")
                ));
            }

            // Add prepositional phrases
            for token in &clause.tokens {
                if let SentenceRole::PrepositionalPhrase { preposition } = &token.role {
                    if token.token.token_type == TokenType::Noun {
                        result.push_str(&format!(
                            "  {}\tNOUN\tUUID {} INSTANCE {} MODIFIERS {} PREPOSITION {}\n",
                            token.token.text,
                            token.token.uuid.map_or("None".to_string(), |u| u.to_string()),
                            token.token.instance.map_or("None".to_string(), |u| u.to_string()),
                            token.token.modifiers.join(" "),
                            preposition
                        ));
                    }
                }
            }

            // Add a separator between clauses
            if i < self.clauses.len() - 1 {
                result.push_str("---\n");
            }
        }

        // Add reference resolutions if present
        if !self.references.is_empty() {
            result.push_str("\nReferences:\n");
            for (pronoun, referent) in &self.references {
                result.push_str(&format!("  {} -> {}\n", pronoun, referent));
            }
        }

        result
    }

    // Get all subjects across all clauses
    pub fn get_subjects(&self) -> Vec<&ParsedToken> {
        self.clauses
            .iter()
            .flat_map(|clause| clause.tokens.iter().filter(|t| matches!(t.role, SentenceRole::Subject)))
            .collect()
    }

    // Get all objects across all clauses
    pub fn get_objects(&self) -> Vec<&ParsedToken> {
        self.clauses
            .iter()
            .flat_map(|clause| clause.tokens.iter().filter(|t| matches!(t.role, SentenceRole::Object)))
            .collect()
    }
}

// Lists of common words by type
const DETERMINERS: &[&str] = &[
    "a", "an", "the", "this", "that", "these", "those", "my", "your", "his", "her", "its", "our",
    "their", "some", "any", "each", "every", "no", "many", "much", "few", "little", "other", "another",
];

const PREPOSITIONS: &[&str] = &[
    "in", "on", "at", "by", "with", "from", "to", "for", "of", "about", "above", "below", "between", 
    "among", "through", "during", "before", "after", "since", "until", "against", "into", "onto", 
    "upon", "across", "along", "around", "behind", "beside", "beyond", "near", "over", "under",
];

const COORDINATING_CONJUNCTIONS: &[&str] = &["and", "but", "or", "nor", "for", "yet", "so"];

// Create a struct to hold subordinating conjunctions with their types
#[derive(Debug, Clone, PartialOrd, PartialEq, Eq, Ord, Serialize, Deserialize)]
struct SubordinatingConjunction {
    conjunction: &'static str,
    clause_type: AdverbialClauseType,
}

// Subordinating conjunctions with their associated adverbial clause types
const ADVERBIAL_CONJUNCTIONS: &[SubordinatingConjunction;33] = &[
    // Time
    SubordinatingConjunction { conjunction: "when", clause_type: AdverbialClauseType::Time },
    SubordinatingConjunction { conjunction: "after", clause_type: AdverbialClauseType::Time },
    SubordinatingConjunction { conjunction: "before", clause_type: AdverbialClauseType::Time },
    SubordinatingConjunction { conjunction: "as", clause_type: AdverbialClauseType::Time },
    SubordinatingConjunction { conjunction: "while", clause_type: AdverbialClauseType::Time },
    SubordinatingConjunction { conjunction: "until", clause_type: AdverbialClauseType::Time },
    SubordinatingConjunction { conjunction: "since", clause_type: AdverbialClauseType::Time },
    
    // Condition
    SubordinatingConjunction { conjunction: "if", clause_type: AdverbialClauseType::Condition },
    SubordinatingConjunction { conjunction: "unless", clause_type: AdverbialClauseType::Condition },
    SubordinatingConjunction { conjunction: "provided", clause_type: AdverbialClauseType::Condition },
    SubordinatingConjunction { conjunction: "provided that", clause_type: AdverbialClauseType::Condition },
    
    // Purpose
    SubordinatingConjunction { conjunction: "so that", clause_type: AdverbialClauseType::Purpose },
    SubordinatingConjunction { conjunction: "in order that", clause_type: AdverbialClauseType::Purpose },
    SubordinatingConjunction { conjunction: "so", clause_type: AdverbialClauseType::Purpose },
    
    // Reason
    SubordinatingConjunction { conjunction: "because", clause_type: AdverbialClauseType::Reason },
    SubordinatingConjunction { conjunction: "since", clause_type: AdverbialClauseType::Reason },
    SubordinatingConjunction { conjunction: "as", clause_type: AdverbialClauseType::Reason },
    
    // Concession
    SubordinatingConjunction { conjunction: "although", clause_type: AdverbialClauseType::Concession },
    SubordinatingConjunction { conjunction: "though", clause_type: AdverbialClauseType::Concession },
    SubordinatingConjunction { conjunction: "even though", clause_type: AdverbialClauseType::Concession },
    
    // Place
    SubordinatingConjunction { conjunction: "where", clause_type: AdverbialClauseType::Place },
    SubordinatingConjunction { conjunction: "wherever", clause_type: AdverbialClauseType::Place },
    
    // Comparison
    SubordinatingConjunction { conjunction: "as", clause_type: AdverbialClauseType::Comparison },
    SubordinatingConjunction { conjunction: "than", clause_type: AdverbialClauseType::Comparison },
    SubordinatingConjunction { conjunction: "as if", clause_type: AdverbialClauseType::Comparison },
    SubordinatingConjunction { conjunction: "as though", clause_type: AdverbialClauseType::Comparison },
    
    // Manner
    SubordinatingConjunction { conjunction: "as", clause_type: AdverbialClauseType::Manner },
    SubordinatingConjunction { conjunction: "how", clause_type: AdverbialClauseType::Manner },
    
    // Result
    SubordinatingConjunction { conjunction: "so that", clause_type: AdverbialClauseType::Result },
    SubordinatingConjunction { conjunction: "such that", clause_type: AdverbialClauseType::Result },
    SubordinatingConjunction { conjunction: "that", clause_type: AdverbialClauseType::Result },
    
    // Frequency
    SubordinatingConjunction { conjunction: "whenever", clause_type: AdverbialClauseType::Frequency },
    SubordinatingConjunction { conjunction: "every time", clause_type: AdverbialClauseType::Frequency },
];

// For compatibility, keep a list of all subordinating conjunctions
const SUBORDINATING_CONJUNCTIONS: &[&str] = &[
    "after", "although", "as", "because", "before", "even if", "even though", "if", "since", 
    "so that", "than", "that", "though", "unless", "until", "when", "whenever", "where", 
    "whereas", "wherever", "whether", "while", "provided", "provided that", "in order that",
    "as if", "as though", "such that", "every time", "how",
];

const PRONOUNS: &[&str] = &[
    "i", "me", "my", "mine", "myself", 
    "you", "your", "yours", "yourself", "yourselves", 
    "he", "him", "his", "himself", 
    "she", "her", "hers", "herself", 
    "it", "its", "itself",
    "we", "us", "our", "ours", "ourselves", 
    "they", "them", "their", "theirs", "themselves", 
    "this", "that", "these", "those",
];

fn tokenize(sentence: &str, db: &SynsetDatabase) -> Result<Vec<Token>, String> {
    let token_strings = crate::text::tokenize_sentence(sentence);

    let mut tokens = Vec::new();
    let mut adj_or_adv_positions = BTreeMap::new();

    // First pass: identify token types
    for (i, token_str) in token_strings.iter().enumerate() {
        let token_lower = token_str.to_lowercase();

        let token_type = if token_str.chars().all(|c| c.is_ascii_punctuation()) {
            TokenType::Punctuation
        } else if DETERMINERS.contains(&token_lower.as_str()) {
            TokenType::Determiner
        } else if PREPOSITIONS.contains(&token_lower.as_str()) {
            TokenType::Preposition
        } else if COORDINATING_CONJUNCTIONS.contains(&token_lower.as_str()) {
            TokenType::CoordinatingConjunction
        } else if SUBORDINATING_CONJUNCTIONS.contains(&token_lower.as_str()) {
            TokenType::SubordinatingConjunction
        } else if PRONOUNS.contains(&token_lower.as_str()) {
            TokenType::Pronoun
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

        // Assign UUID for nouns, verbs, and pronouns
        if token.token_type == TokenType::Noun || token.token_type == TokenType::Verb || token.token_type == TokenType::Pronoun {
            let word_to_lookup = if token.token_type == TokenType::Verb {
                token.normalized_text.clone()
            } else {
                token.text.to_lowercase()
            };

            // For pronouns, we don't look up in WordNet, just create an instance
            if token.token_type == TokenType::Pronoun {
                token.instance = Some(Uuid::new());
            } else {
                let synsets = db.find_synsets_by_word(&word_to_lookup);
                if let Some((uuid, _, _)) = synsets.first() {
                    token.uuid = Some(*uuid);
                    token.instance = Some(Uuid::new());
                }
            }
        }

        // Find modifiers (adjectives before nouns, adverbs before verbs)
        if token.token_type == TokenType::Noun {
            // Look backwards for adjectives
            for j in (0..i).rev() {
                if tokens[j].token_type == TokenType::Adjective {
                    token.modifiers.push(tokens[j].text.clone());
                } else if tokens[j].token_type != TokenType::Determiner {
                    // Stop at non-adjectives and non-determiners
                    break;
                }
            }
        } else if token.token_type == TokenType::Verb {
            // Look backwards for adverbs
            for j in (0..i).rev() {
                if tokens[j].token_type == TokenType::Adverb {
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

fn identify_adverbial_clause_type(conjunction: &str) -> Option<(AdverbialClauseType, String)> {
    // Check for complex conjunctions first (multi-word)
    let complex_conjunctions = [
        ("so that", AdverbialClauseType::Purpose),
        ("so that", AdverbialClauseType::Result),
        ("provided that", AdverbialClauseType::Condition),
        ("in order that", AdverbialClauseType::Purpose),
        ("as if", AdverbialClauseType::Comparison),
        ("as though", AdverbialClauseType::Comparison),
        ("such that", AdverbialClauseType::Result),
        ("every time", AdverbialClauseType::Frequency),
        ("even though", AdverbialClauseType::Concession),
    ];
    
    for (conj, clause_type) in complex_conjunctions.iter() {
        if conjunction.starts_with(conj) {
            return Some((clause_type.clone(), conj.to_string()));
        }
    }
    
    // Check for simple conjunctions
    for conj in ADVERBIAL_CONJUNCTIONS.iter() {
        if conjunction == conj.conjunction {
            return Some((conj.clause_type.clone(), conj.conjunction.to_string()));
        }
    }
    
    None
}

fn detect_adverbial_clause(tokens: &[Token], start_idx: usize) -> Option<(AdverbialClauseType, String, usize)> {
    // Look for adverbial markers like "so...that" or complex patterns
    
    // Check for "so...that" pattern (Result clause)
    if start_idx + 2 < tokens.len() {
        if tokens[start_idx].text.to_lowercase() == "so" {
            // Look ahead for "that"
            for i in start_idx+1..tokens.len() {
                if tokens[i].text.to_lowercase() == "that" {
                    return Some((AdverbialClauseType::Result, "so...that".to_string(), i));
                }
            }
        }
    }
    
    // Check for time adverbs that aren't at clause boundaries
    let time_adverbs = ["barely", "hardly", "scarcely", "no sooner"];
    if start_idx < tokens.len() {
        let lower_text = tokens[start_idx].text.to_lowercase();
        for adv in time_adverbs.iter() {
            if lower_text == *adv {
                // This could be part of a time adverbial construction
                // Look ahead for "when", "than", etc.
                for i in start_idx+1..tokens.len() {
                    if tokens[i].text.to_lowercase() == "when" || 
                       tokens[i].text.to_lowercase() == "than" ||
                       tokens[i].text.to_lowercase() == "before" {
                        return Some((AdverbialClauseType::Time, format!("{}...{}", adv, tokens[i].text), i));
                    }
                }
            }
        }
    }
    
    // Check for comparative constructions 
    if start_idx + 2 < tokens.len() {
        let lower_text = tokens[start_idx].text.to_lowercase();
        if lower_text == "as" || lower_text == "more" || lower_text == "less" {
            // Look for the second "as" or "than"
            for i in start_idx+1..tokens.len() {
                if tokens[i].text.to_lowercase() == "as" && lower_text == "as" {
                    return Some((AdverbialClauseType::Comparison, "as...as".to_string(), i));
                } else if tokens[i].text.to_lowercase() == "than" && 
                          (lower_text == "more" || lower_text == "less") {
                    return Some((AdverbialClauseType::Comparison, format!("{}...than", lower_text), i));
                }
            }
        }
    }
    
    None
}

fn split_into_clauses(tokens: Vec<Token>, db: &SynsetDatabase) -> Result<Vec<Clause>, String> {
    let mut clauses = Vec::new();
    let mut current_clause_tokens = Vec::new();
    let mut clause_type = ClauseType::Main;
    let mut i = 0;

    // Find clause boundaries based on punctuation and conjunctions
    while i < tokens.len() {
        let token = &tokens[i];
        
        match token.token_type {
            TokenType::Punctuation if token.text == "." || token.text == ";" || token.text == "!" || token.text == "?" => {
                // End of clause
                if !current_clause_tokens.is_empty() {
                    let parsed_tokens = assign_roles_to_tokens(&current_clause_tokens, db)?;
                    clauses.push(Clause {
                        tokens: parsed_tokens,
                        clause_type,
                        reference: None, // References resolved in a separate step
                    });
                    current_clause_tokens = Vec::new();
                    clause_type = ClauseType::Main;
                }
            }
            TokenType::CoordinatingConjunction => {
                // Coordinating conjunction starts a new clause
                if !current_clause_tokens.is_empty() {
                    let parsed_tokens = assign_roles_to_tokens(&current_clause_tokens, db)?;
                    clauses.push(Clause {
                        tokens: parsed_tokens,
                        clause_type,
                        reference: None,
                    });
                    current_clause_tokens = Vec::new();
                    clause_type = ClauseType::Coordinate { conjunction: token.text.clone() };
                }
            }
            TokenType::SubordinatingConjunction => {
                // Check if this is an adverbial clause marker
                if let Some((adv_type, conj)) = identify_adverbial_clause_type(&token.text.to_lowercase()) {
                    // Adverbial clause 
                    if i > 0 && !current_clause_tokens.is_empty() {
                        let parsed_tokens = assign_roles_to_tokens(&current_clause_tokens, db)?;
                        clauses.push(Clause {
                            tokens: parsed_tokens,
                            clause_type,
                            reference: None,
                        });
                        current_clause_tokens = Vec::new();
                        clause_type = ClauseType::Adverbial { 
                            clause_type: adv_type,
                            conjunction: conj,
                        };
                    }
                } else {
                    // Regular subordinate clause
                    if i > 0 && !current_clause_tokens.is_empty() {
                        let parsed_tokens = assign_roles_to_tokens(&current_clause_tokens, db)?;
                        clauses.push(Clause {
                            tokens: parsed_tokens,
                            clause_type,
                            reference: None,
                        });
                        current_clause_tokens = Vec::new();
                        clause_type = ClauseType::Subordinate { conjunction: Some(token.text.clone()) };
                    }
                }
                current_clause_tokens.push(token.clone());
            }
            _ => {
                // Check for adverbial clause patterns that span multiple tokens
                if let Some((adv_type, pattern, end_idx)) = detect_adverbial_clause(&tokens, i) {
                    // We found a multi-token adverbial pattern
                    if !current_clause_tokens.is_empty() {
                        let parsed_tokens = assign_roles_to_tokens(&current_clause_tokens, db)?;
                        clauses.push(Clause {
                            tokens: parsed_tokens,
                            clause_type,
                            reference: None,
                        });
                        current_clause_tokens = Vec::new();
                        clause_type = ClauseType::Adverbial { 
                            clause_type: adv_type, 
                            conjunction: pattern,
                        };
                    }
                    // Skip to the end of the pattern
                    i = end_idx;
                } else {
                    current_clause_tokens.push(token.clone());
                }
            }
        }
        
        i += 1;
    }

    // Don't forget to process the last clause
    if !current_clause_tokens.is_empty() {
        let parsed_tokens = assign_roles_to_tokens(&current_clause_tokens, db)?;
        clauses.push(Clause {
            tokens: parsed_tokens,
            clause_type,
            reference: None,
        });
    }

    // If we didn't find any clauses, create a single main clause
    if clauses.is_empty() {
        let parsed_tokens = assign_roles_to_tokens(&tokens, db)?;
        clauses.push(Clause {
            tokens: parsed_tokens,
            clause_type: ClauseType::Main,
            reference: None,
        });
    }

    // Handle adverbial frequency clauses
    // These are special because they often don't have subordinating conjunctions
    // They have frequency adverbs like "often", "always", "never", etc.
    let frequency_adverbs = ["often", "always", "never", "frequently", "rarely", "occasionally", "seldom"];
    
    for i in 0..clauses.len() {
        if let ClauseType::Main = clauses[i].clause_type {
            for token in &clauses[i].tokens {
                if token.token.token_type == TokenType::Adverb {
                    let lower_text = token.token.text.to_lowercase();
                    if frequency_adverbs.contains(&lower_text.as_str()) {
                        // Change the clause type to adverbial frequency
                        clauses[i].clause_type = ClauseType::Adverbial {
                            clause_type: AdverbialClauseType::Frequency,
                            conjunction: lower_text.clone(),
                        };
                        break;
                    }
                }
            }
        }
    }

    // Resolve references between clauses
    resolve_clause_references(&mut clauses);

    Ok(clauses)
}

fn assign_roles_to_tokens(tokens: &[Token], db: &SynsetDatabase) -> Result<Vec<ParsedToken>, String> {
    let mut parsed_tokens = Vec::new();
    let mut found_verb = false;
    let mut found_subject = false;
    let mut found_object = false;
    let mut found_indirect_object = false;
    let mut current_preposition: Option<String> = None;
    let mut skip_stopwords = false;

    // Extended SVO parsing with support for prepositional phrases and indirect objects
    for token in tokens.iter() {
        // Skip stopwords if requested
        if skip_stopwords && is_stopword(&token.text.to_lowercase()) {
            continue;
        }
        
        let role = match token.token_type {
            TokenType::Noun | TokenType::Pronoun if !found_verb && !found_subject => {
                found_subject = true;
                SentenceRole::Subject
            }
            TokenType::Verb if !found_verb => {
                found_verb = true;
                SentenceRole::Verb {
                    tense: db.get_base_form(&token.normalized_text).1,
                }
            }
            TokenType::Noun if found_verb && !found_object && !found_indirect_object && current_preposition.is_none() => {
                // Check if this might be an indirect object
                // Indirect objects typically come before direct objects in English
                // "She gave him a book." - "him" is indirect object, "book" is direct object
                if let Some(next_idx) = tokens.iter().position(|t| &t.text == &token.text).map(|i| i + 1) {
                    if next_idx < tokens.len() && tokens[next_idx].token_type == TokenType::Noun {
                        found_indirect_object = true;
                        SentenceRole::IndirectObject
                    } else {
                        found_object = true;
                        SentenceRole::Object
                    }
                } else {
                    found_object = true;
                    SentenceRole::Object
                }
            }
            TokenType::Noun if found_verb && found_indirect_object && !found_object && current_preposition.is_none() => {
                found_object = true;
                SentenceRole::Object
            }
            TokenType::Noun if current_preposition.is_some() => {
                SentenceRole::PrepositionalPhrase { 
                    preposition: current_preposition.take().unwrap() 
                }
            }
            TokenType::Preposition => {
                current_preposition = Some(token.text.clone());
                SentenceRole::Modifier
            }
            TokenType::Adverb => {
                // Check if this is a sentence adverb or a verb modifier
                if !found_verb {
                    // Sentence adverb (modifies the entire sentence)
                    SentenceRole::Modifier
                } else {
                    // Verb modifier
                    SentenceRole::Modifier
                }
            }
            _ => SentenceRole::Modifier,
        };

        parsed_tokens.push(ParsedToken { token: token.clone(), role });
    }

    Ok(parsed_tokens)
}

// Helper function to check if a word is a stopword
fn is_stopword(word: &str) -> bool {
    static STOPWORDS: &[&str] = &[
        "the", "a", "an", "and", "but", "or", "nor", "for", "yet", "so",
        "in", "on", "at", "by", "to", "for", "with", "about", "against", "between",
        "during", "of", "before", "after", "above", "below", "from", "up", "down",
        "is", "am", "are", "was", "were", "be", "been", "being",
        "have", "has", "had", "do", "does", "did", "can", "could", "shall", "should",
        "will", "would", "may", "might", "must",
    ];
    
    STOPWORDS.contains(&word)
}

fn resolve_clause_references(clauses: &mut Vec<Clause>) {
    if clauses.len() <= 1 {
        return; // No need for resolution with only one clause
    }

    // Find all potential referents (subjects from previous clauses)
    let mut potential_referents = Vec::new();
    for clause in clauses.iter() {
        for token in &clause.tokens {
            if matches!(token.role, SentenceRole::Subject) 
               && token.token.token_type != TokenType::Pronoun {
                if let Some(instance) = token.token.instance {
                    potential_referents.push((instance, token.token.text.clone()));
                }
            }
        }
    }

    // Try to resolve pronouns
    for i in 1..clauses.len() {
        for j in 0..clauses[i].tokens.len() {
            let token = &clauses[i].tokens[j];
            if token.token.token_type == TokenType::Pronoun 
               && matches!(token.role, SentenceRole::Subject) {
                let pronoun_text = token.token.text.to_lowercase();
                
                // Simple heuristic: "it" refers to the last mentioned non-human subject
                // "this" refers to the concept in the previous clause
                if pronoun_text == "it" || pronoun_text == "this" {
                    if let Some((referent_uuid, _)) = potential_referents.last() {
                        let reference_type = if pronoun_text == "it" {
                            ReferenceType::Pronoun { referent: *referent_uuid }
                        } else {
                            ReferenceType::Demonstrative { referent: *referent_uuid }
                        };
                        
                        clauses[i].reference = Some(reference_type);
                    }
                }
            }
        }
    }
}

fn resolve_references(clauses: &[Clause]) -> BTreeMap<Uuid, Uuid> {
    let mut references = BTreeMap::new();
    
    // Extract all resolved references from clauses
    for clause in clauses {
        if let Some(ref_type) = &clause.reference {
            for token in &clause.tokens {
                if token.token.token_type == TokenType::Pronoun && token.token.instance.is_some() {
                    match ref_type {
                        ReferenceType::Pronoun { referent } | ReferenceType::Demonstrative { referent } => {
                            references.insert(token.token.instance.unwrap(), *referent);
                        }
                    }
                }
            }
        }
    }
    
    references
}

// Extends the existing Tense implementation
impl std::fmt::Display for Tense {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Tense::Infinitive => write!(f, "infinitive"),
            Tense::PresentSimple => write!(f, "present simple"),
            Tense::PresentContinuous => write!(f, "present continuous"),
            Tense::PresentPerfect => write!(f, "present perfect"),
            Tense::PresentPerfectContinuous => write!(f, "present perfect continuous"),
            Tense::PastSimple => write!(f, "past simple"),
            Tense::PastContinuous => write!(f, "past continuous"),
            Tense::PastPerfect => write!(f, "past perfect"),
            Tense::PastPerfectContinuous => write!(f, "past perfect continuous"),
            Tense::FutureSimple => write!(f, "future simple"),
            Tense::FutureContinuous => write!(f, "future continuous"),
            Tense::FuturePerfect => write!(f, "future perfect"),
            Tense::FuturePerfectContinuous => write!(f, "future perfect continuous"),
            Tense::ConditionalSimple => write!(f, "conditional simple"),
        }
    }
}

// Keep the original implementation but delegate to the new one
impl SentenceRole {
    pub fn is_subject(&self) -> bool {
        matches!(self, SentenceRole::Subject)
    }
    
    pub fn is_verb(&self) -> bool {
        matches!(self, SentenceRole::Verb { .. })
    }
    
    pub fn is_object(&self) -> bool {
        matches!(self, SentenceRole::Object)
    }
    
    pub fn is_modifier(&self) -> bool {
        matches!(self, SentenceRole::Modifier)
    }
}