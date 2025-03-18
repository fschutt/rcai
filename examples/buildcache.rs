use core::sync::atomic::AtomicU64;
use serde_derive::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};
use std::path::Path;

include!("../src/core.rs");

mod text_processing {
    include!("../src/text_processing.rs");
}

impl SemanticAnalyzer {
    // Pre-populate words from their glossary definitions
    pub fn populate_from_glossary(&mut self, wordnet: &WordNetIntegration, words: &[&str]) {
        for word in words {
            println!("word: {word}");
            let lower_word = word.to_lowercase();
            let senses = wordnet.db.senses(&lower_word);

            if senses.is_empty() {
                continue;
            }

            for sense in senses {
                // Add the base word with correct POS
                let pos = sense.part_of_speech;

                println!("word: {word}: pos: {pos:?}");

                let word_id = self.add_word_with_pos(&lower_word, pos);

                println!("ok1");

                // Parse the glossary to extract related words
                let sentences = text_processing::split_into_sentences(&sense.gloss);

                for sentence in sentences {
                    let clean_text = text_processing::clean_text(&sentence);
                    let tokens = text_processing::tokenize(&clean_text);

                    // Filter out stopwords and very short words
                    let content_words: Vec<_> = tokens
                        .iter()
                        .filter(|&t| t.len() > 2 && !text_processing::is_stopword(t))
                        .collect();

                    // Link content words from glossary to the main word
                    for token in content_words {
                        let related_id = self.add_word(token, wordnet);

                        // Add "RelatedTo" relationship based on glossary
                        if let Some(concept) = self.get_concept_mut(word_id) {
                            println!(
                                "adding relationship of word {word} to glossary entry: {concept:?}"
                            );
                            let relation_set = concept
                                .relations
                                .entry(SemanticRelation::RelatedTo)
                                .or_insert_with(BTreeSet::new);
                            relation_set.insert(related_id);
                        }
                    }
                }

                // Also add synonyms
                for syn in &sense.synonyms {
                    let syn_word = syn.word.to_lowercase();
                    if syn_word != lower_word {
                        let syn_id = self.add_word(&syn_word, wordnet);
                        if let Some(concept) = self.get_concept_mut(word_id) {
                            let syn_set = concept
                                .relations
                                .entry(SemanticRelation::SimilarTo)
                                .or_insert_with(BTreeSet::new);
                            syn_set.insert(syn_id);
                        }
                    }
                }
            }
        }
    }

    pub fn to_json(&self) -> String {
        serde_json::to_string_pretty(self).unwrap_or_default()
    }

    // Save analyzer to a file for build.rs use
    pub fn save_to_file(&self, path: &str) -> Result<(), String> {
        let json = self.to_json();
        std::fs::write(path, json).map_err(|e| e.to_string())
    }

    // Add a word with a specific part of speech
    pub fn add_word_with_pos(&mut self, word: &str, pos: PartOfSpeech) -> ConceptId {
        let lower_word = word.to_lowercase();

        // Create a key that includes the POS for disambiguation
        let key = format!("{}#{:?}", lower_word, pos);

        println!("1.5");

        // If already in dictionary, return existing ID
        if let Some(concept) = self.dictionary.get(&key) {
            return concept.id;
        }

        println!("2.5");
        // Create a new concept with the specified part of speech
        let id = ConceptId::new();
        println!("3.5");

        let concept = Concept {
            id,
            word: word.to_string(),
            pos,
            attributes: BTreeSet::new(),
            relations: BTreeMap::new(),
            synsets: Vec::new(),
        };

        self.dictionary.insert(key.clone(), concept);
        self.concepts_by_id.insert(id, key);

        id
    }

    // Add a word to the dictionary with wordnet enrichment if available
    pub fn add_word(&mut self, word: &str, wordnet: &WordNetIntegration) -> ConceptId {
        let lower_word = word.to_lowercase();

        // Check if any entry exists for this word (with any POS)
        let existing_ids: Vec<_> = self
            .dictionary
            .iter()
            .filter(|(k, _)| k.starts_with(&format!("{}#", lower_word)))
            .map(|(_, v)| v.id)
            .collect();

        println!("1");

        if !existing_ids.is_empty() {
            return existing_ids[0];
        }

        println!("2");

        // Get possible parts of speech from WordNet
        return self.add_word_with_pos(&lower_word, PartOfSpeech::Unknown);
    }

    // Get a mutable concept by ID
    pub fn get_concept_mut(&mut self, id: ConceptId) -> Option<&mut Concept> {
        if let Some(key) = self.concepts_by_id.get(&id) {
            self.dictionary.get_mut(key)
        } else {
            None
        }
    }
}

// This is a build script that runs at compile time to pre-generate the semantic analyzer

fn main() {
    println!("cargo:rerun-if-changed=src/");
    println!("cargo:rerun-if-changed=dict/");
    println!("cargo:rerun-if-changed=common_words.txt");

    // Import our own crate for the build script
    // In a real build.rs, you'd need to ensure the paths are correctly set up
    // Typically this would use a build-dependency in Cargo.toml
    let analyzer_lib = Path::new("src/lib.rs");
    if !analyzer_lib.exists() {
        println!("cargo:warning=Cannot find src/lib.rs - skipping analyzer generation");
        return;
    }

    // For the example, we'll just indicate where the analyzer should be created
    let dest_path = Path::new("./analyzed.json");

    // The actual implementation would do:
    // Create a SemanticAnalyzer
    let mut analyzer = SemanticAnalyzer::default();
    let wordnet = WordNetIntegration::new_english();

    // Load common English words to pre-process
    let common_words = include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/common_words.txt"))
        .lines()
        .map(|s| s.trim())
        .collect::<Vec<_>>();

    // Populate from wordnet glossaries
    analyzer.populate_from_glossary(&wordnet, &common_words);

    // Save the analyzer to JSON
    analyzer
        .save_to_file(dest_path.to_str().unwrap())
        .expect("Failed to save analyzer to JSON");

    // Create a symbolic link or copy the analyzed.json to the project root
    // for easier inclusion at runtime
    println!("cargo:rustc-env=ANALYZER_PATH={}", dest_path.display());
}
