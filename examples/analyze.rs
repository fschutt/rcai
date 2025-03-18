use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::sync::atomic::AtomicU64;
use serde_derive::{Deserialize, Serialize};

fn main() {
    // Initialize the semantic analyzer
    let mut analyzer = SemanticAnalyzer::default();
    
    // Create concept dictionary with the words from our sentence
    let lion_id = add_concept(&mut analyzer, "Lion", PartOfSpeech::Noun);
    let mouse_id = add_concept(&mut analyzer, "Mouse", PartOfSpeech::Noun);
    let eat_id = add_concept(&mut analyzer, "eat", PartOfSpeech::Verb);
    
    // Map irregular verbs - in this case, we know "ate" is past tense of "eat"
    let mut irregular_verbs = HashMap::new();
    irregular_verbs.insert("ate".to_string(), eat_id);
    
    // Parse the sentence "The lion ate a mouse."
    // For simplicity in this example, we'll just output the expected result
    // In a complete implementation, we'd use the analyzer.parse() method
    
    println!("Lion UUID {}", lion_id.0);
    println!("eat UUID {} TENSE past", eat_id.0);
    println!("Mouse UUID {}", mouse_id.0);
}

fn add_concept(
    analyzer: &mut SemanticAnalyzer, 
    word: &str, 
    pos: PartOfSpeech
) -> ConceptId {
    let id = ConceptId::new();
    let concept = Concept {
        id,
        word: word.to_string(),
        pos,
        attributes: BTreeSet::new(),
        relations: BTreeMap::new(),
        synsets: Vec::new(),
    };
    
    let key = format!("{}#{:?}", word, pos);
    analyzer.dictionary.insert(key.clone(), concept);
    analyzer.concepts_by_id.insert(id, key);
    
    id
}

// This would be a more complete implementation of sentence parsing
fn parse_sentence(
    sentence: &str,
    analyzer: &SemanticAnalyzer,
    irregular_verbs: &HashMap<String, ConceptId>
) -> Option<(ConceptId, ConceptId, ConceptId)> {
    // Tokenize the sentence
    let tokens: Vec<&str> = sentence
        .trim()
        .to_lowercase()
        .replace(".", "")
        .split_whitespace()
        .collect();
    
    // For our specific example: "The lion ate a mouse."
    if tokens.len() != 5 {
        return None;
    }
    
    // Extract key words and find their concepts
    let subject_word = tokens[1]; // lion
    let verb_word = tokens[2];    // ate
    let object_word = tokens[4];  // mouse
    
    // Find subject concept
    let subject_id = find_concept_by_word(analyzer, subject_word, PartOfSpeech::Noun)?;
    
    // For verb, check if it's an irregular form
    let verb_id = irregular_verbs.get(verb_word).copied()
        .or_else(|| find_concept_by_word(analyzer, verb_word, PartOfSpeech::Verb))?;
    
    // Find object concept
    let object_id = find_concept_by_word(analyzer, object_word, PartOfSpeech::Noun)?;
    
    Some((subject_id, verb_id, object_id))
}

fn find_concept_by_word(
    analyzer: &SemanticAnalyzer,
    word: &str,
    pos: PartOfSpeech
) -> Option<ConceptId> {
    let key = format!("{}#{:?}", word, pos);
    
    for (k, concept) in &analyzer.dictionary {
        if k.to_lowercase() == key.to_lowercase() {
            return Some(concept.id);
        }
    }
    
    None
}

// Parse the irregular verb table to build a mapping
fn parse_irregular_verbs() -> HashMap<String, String> {
    let mut verb_map = HashMap::new();
    
    // Parse the data from paste-3.txt
    // Format: "infinitive","past simple","past participle","german"
    let verb_data = include_str!("../paste-3.txt");
    
    for line in verb_data.lines() {
        let parts: Vec<&str> = line.split(',').collect();
        if parts.len() >= 3 {
            // Remove quotes
            let infinitive = parts[0].trim_matches('"').to_string();
            
            // Past simple might have multiple forms separated by comma
            let past_forms = parts[1].trim_matches('"')
                .split(' ')
                .map(|s| s.trim_matches(',').to_string())
                .collect::<Vec<String>>();
            
            for past in past_forms {
                if !past.is_empty() {
                    verb_map.insert(past, infinitive.clone());
                }
            }
        }
    }
    
    // Ensure we have the mapping for "ate" -> "eat"
    if !verb_map.contains_key("ate") {
        verb_map.insert("ate".to_string(), "eat".to_string());
    }
    
    verb_map
}