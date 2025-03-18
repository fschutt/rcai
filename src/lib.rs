use core::sync::atomic::AtomicU64;
use std::collections::{BTreeMap, BTreeSet};
use serde_derive::{Deserialize, Serialize};

include!("./core.rs");

pub mod text_processing;

#[derive(Debug, Serialize, Deserialize)]
pub struct ParsedSentence {
    // Core SVO structure
    pub subject: ConceptId,
    pub verb: ConceptId,
    pub object: Option<ConceptId>,
    // Additional semantic components
    pub subject_modifiers: Vec<(ConceptId, SemanticRelation)>,
    pub verb_modifiers: Vec<(ConceptId, SemanticRelation)>,
    pub object_modifiers: Vec<(ConceptId, SemanticRelation)>,
    // Raw sentence
    pub text: String,
}

impl ParsedSentence {
    // Calculate semantic similarity to another sentence
    pub fn similarity(&self, other: &ParsedSentence, analyzer: &SemanticAnalyzer) -> f32 {
        let mut score = 0.0;

        // Direct concept matches (weighted)
        if self.subject == other.subject {
            score += 0.3;
        } else if let Some(sim) = analyzer.concept_similarity(self.subject, other.subject) {
            score += 0.3 * sim;
        }

        if self.verb == other.verb {
            score += 0.4;
        } else if let Some(sim) = analyzer.concept_similarity(self.verb, other.verb) {
            score += 0.4 * sim;
        }

        if let (Some(obj1), Some(obj2)) = (&self.object, &other.object) {
            if obj1 == obj2 {
                score += 0.3;
            } else if let Some(sim) = analyzer.concept_similarity(*obj1, *obj2) {
                score += 0.3 * sim;
            }
        }

        // Check modifiers similarity
        let mod_score = self.modifier_similarity(other, analyzer);
        score += mod_score * 0.2; // Modifiers contribute up to 20% of score

        score
    }

    // Helper to calculate similarity between modifiers
    fn modifier_similarity(&self, other: &ParsedSentence, analyzer: &SemanticAnalyzer) -> f32 {
        // This is a simplified implementation
        // A more sophisticated approach would align modifiers and compare them
        let mut score = 0.0;

        // Compare subject modifiers
        let subj_mod_sim = Self::compare_modifier_sets(
            &self.subject_modifiers,
            &other.subject_modifiers,
            analyzer,
        );

        // Compare verb modifiers
        let verb_mod_sim =
            Self::compare_modifier_sets(&self.verb_modifiers, &other.verb_modifiers, analyzer);

        // Compare object modifiers
        let obj_mod_sim =
            Self::compare_modifier_sets(&self.object_modifiers, &other.object_modifiers, analyzer);

        // Average modifier similarity
        let mut divisor = 3.0;
        if self.subject_modifiers.is_empty() || other.subject_modifiers.is_empty() {
            divisor -= 1.0;
        }
        if self.verb_modifiers.is_empty() || other.verb_modifiers.is_empty() {
            divisor -= 1.0;
        }
        if self.object_modifiers.is_empty() || other.object_modifiers.is_empty() {
            divisor -= 1.0;
        }

        if divisor > 0.0 {
            score = (subj_mod_sim + verb_mod_sim + obj_mod_sim) / divisor;
        }

        score
    }

    // Helper to compare two sets of modifiers
    fn compare_modifier_sets(
        mods1: &[(ConceptId, SemanticRelation)],
        mods2: &[(ConceptId, SemanticRelation)],
        analyzer: &SemanticAnalyzer,
    ) -> f32 {
        if mods1.is_empty() || mods2.is_empty() {
            return 0.0;
        }

        let mut total_sim = 0.0;
        let mut matches = 0;

        for (id1, rel1) in mods1 {
            for (id2, rel2) in mods2 {
                if rel1 == rel2 {
                    if id1 == id2 {
                        total_sim += 1.0;
                    } else if let Some(sim) = analyzer.concept_similarity(*id1, *id2) {
                        total_sim += sim;
                    }
                    matches += 1;
                }
            }
        }

        if matches > 0 {
            total_sim / matches as f32
        } else {
            0.0
        }
    }
}

impl SemanticAnalyzer {
    pub fn new_english() -> Self {
        Self {
            dictionary: BTreeMap::new(),
            concepts_by_id: BTreeMap::new(),
        }
    }

    pub fn from_json(s: &str) -> Result<Self, String> {
        serde_json::from_str(s).map_err(|e| e.to_string())
    }

     
    // Load analyzer from bytes for runtime use
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, String> {
        let json = std::str::from_utf8(bytes).map_err(|e| e.to_string())?;
        Self::from_json(json)
    }
    
    // Load pre-built analyzer from compiled-in JSON
    pub fn load_prebuilt() -> Result<Self, String> {
        let analyzer_json = include_bytes!(concat!(env!("CARGO_MANIFEST_DIR"), "/analyzed.json"));
        Self::from_bytes(analyzer_json)
    }

    // Get a concept by ID
    pub fn get_concept(&self, id: ConceptId) -> Option<&Concept> {
        self.concepts_by_id.get(&id).and_then(|key| self.dictionary.get(key))
    }

    // Calculate similarity between two concepts (0.0 to 1.0)
    pub fn concept_similarity(&self, id1: ConceptId, id2: ConceptId) -> Option<f32> {
        if id1 == id2 {
            return Some(1.0);
        }

        let c1 = self.get_concept(id1)?;
        let c2 = self.get_concept(id2)?;

        // Exact word match but different POS
        if c1.word.to_lowercase() == c2.word.to_lowercase() {
            return Some(0.9);
        }

        // Check direct semantic relationships
        for (rel, targets) in &c1.relations {
            if targets.contains(&id2) {
                // Different relationships have different similarity weights
                match rel {
                    SemanticRelation::SimilarTo => return Some(0.9),
                    SemanticRelation::IsA | SemanticRelation::TypeOf => return Some(0.8),
                    SemanticRelation::HasSubtype => return Some(0.7),
                    SemanticRelation::PartOf | SemanticRelation::HasPart => return Some(0.6),
                    SemanticRelation::DerivedFrom => return Some(0.5),
                    SemanticRelation::RelatedTo => return Some(0.4),
                    _ => return Some(0.3),
                }
            }
        }

        // Check shared relationships (concepts that relate to both)
        let mut shared_count = 0;
        let mut total_count = 0;

        for (rel1, targets1) in &c1.relations {
            if let Some(targets2) = c2.relations.get(rel1) {
                let intersection = targets1.intersection(targets2).count();
                shared_count += intersection;
                total_count += targets1.len() + targets2.len() - intersection;
            } else {
                total_count += targets1.len();
            }
        }

        // Add any relations in c2 that weren't in c1
        for (rel2, targets2) in &c2.relations {
            if !c1.relations.contains_key(rel2) {
                total_count += targets2.len();
            }
        }

        if total_count > 0 {
            Some((shared_count as f32) / (total_count as f32))
        } else {
            // No relationships to compare
            Some(0.0)
        }
    }

    // Parse a sentence into semantic components
    pub fn parse(&self, sentence: &str) -> Option<ParsedSentence> {

        let clean_sentence = text_processing::clean_text(sentence);
        let words = text_processing::tokenize(&clean_sentence);

        if words.len() < 2 {
            return None; // Need at least subject and verb
        }

        let mut i = 0;
        let mut subject = None;
        let mut subject_modifiers = Vec::new();

        // Find subject with modifiers
        while i < words.len() {
            let word = &words[i];
            
            // Check for all POS variants of the word
            let possible_concepts: Vec<_> = self.dictionary.iter()
                .filter(|(k, _)| k.starts_with(&format!("{}#", word)))
                .collect();
                
            for (_, concept) in &possible_concepts {
                match concept.pos {
                    PartOfSpeech::Article => {
                        break;
                    }
                    PartOfSpeech::Adjective => {
                        if subject.is_some() {
                            subject_modifiers.push((concept.id, SemanticRelation::Modifies));
                        }
                    }
                    PartOfSpeech::Noun => {
                        subject = Some(concept.id);
                        i += 1;
                        break;
                    }
                    _ => {}
                }
            }
            
            if subject.is_some() {
                break;
            }
            
            i += 1;
        }

        // Find verb with modifiers
        let mut verb = None;
        let mut verb_modifiers = Vec::new();

        while i < words.len() {
            let word = &words[i];
            
            // Check for all POS variants of the word
            let possible_concepts: Vec<_> = self.dictionary.iter()
                .filter(|(k, _)| k.starts_with(&format!("{}#", word)))
                .collect();
                
            for (_, concept) in &possible_concepts {
                match concept.pos {
                    PartOfSpeech::Verb => {
                        verb = Some(concept.id);
                        i += 1;
                        break;
                    }
                    PartOfSpeech::Adverb => {
                        if verb.is_some() {
                            verb_modifiers.push((concept.id, SemanticRelation::Modifies));
                        }
                    }
                    _ => {}
                }
            }
            
            if verb.is_some() {
                break;
            }
            
            i += 1;
        }

        // Find object with modifiers
        let mut object = None;
        let mut object_modifiers = Vec::new();

        while i < words.len() {
            let word = &words[i];
            
            // Check for all POS variants of the word
            let possible_concepts: Vec<_> = self.dictionary.iter()
                .filter(|(k, _)| k.starts_with(&format!("{}#", word)))
                .collect();
                
            for (_, concept) in &possible_concepts {
                match concept.pos {
                    PartOfSpeech::Article => {
                        break;
                    }
                    PartOfSpeech::Adjective => {
                        if object.is_some() {
                            object_modifiers.push((concept.id, SemanticRelation::Modifies));
                        }
                    }
                    PartOfSpeech::Noun => {
                        object = Some(concept.id);
                        break;
                    }
                    _ => {}
                }
            }
            
            if object.is_some() {
                break;
            }
            
            i += 1;
        }

        if let (Some(subject_id), Some(verb_id)) = (subject, verb) {
            Some(ParsedSentence {
                subject: subject_id,
                verb: verb_id,
                object,
                subject_modifiers,
                verb_modifiers,
                object_modifiers,
                text: sentence.to_string(),
            })
        } else {
            None
        }
    }

    // Calculate connectivity between a collection of sentences
    pub fn sentence_connectivity(
        &self,
        sentences: &[ParsedSentence],
    ) -> BTreeMap<(usize, usize), f32> {
        let mut connectivity = BTreeMap::new();

        for i in 0..sentences.len() {
            for j in i + 1..sentences.len() {
                let similarity = sentences[i].similarity(&sentences[j], self);
                connectivity.insert((i, j), similarity);
                connectivity.insert((j, i), similarity); // Symmetric
            }
        }

        connectivity
    }

    // Find the most semantically central sentence
    pub fn find_central_sentence(&self, sentences: &[ParsedSentence]) -> Option<usize> {
        if sentences.is_empty() {
            return None;
        }

        let connectivity = self.sentence_connectivity(sentences);
        let mut total_similarity = vec![0.0; sentences.len()];

        for ((i, _), score) in connectivity {
            total_similarity[i] += score;
        }

        // Find max
        let mut max_idx = 0;
        let mut max_sim = total_similarity[0];

        for (i, sim) in total_similarity.iter().enumerate().skip(1) {
            if *sim > max_sim {
                max_idx = i;
                max_sim = *sim;
            }
        }

        Some(max_idx)
    }
}