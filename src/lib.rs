use core::sync::atomic::AtomicU64;
use std::collections::{BTreeMap, BTreeSet};

use serde_derive::{Deserialize, Serialize};

static COUNTER1: AtomicU64 = AtomicU64::new(0);
static COUNTER2: AtomicU64 = AtomicU64::new(0);

/// UUID to identify something
#[derive(Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Uuid(u128);

impl core::fmt::Debug for Uuid {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        core::fmt::Display::fmt(self, f)
    }
}

impl core::fmt::Display for Uuid {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let id = self.0;
        write!(
            f,
            "{:08x}-{:04x}-{:04x}-{:04x}-{:012x}",
            (id >> 96) & 0xFFFFFFFF,
            (id >> 80) & 0xFFFF,
            (id >> 64) & 0xFFFF,
            (id >> 48) & 0xFFFF,
            id & 0xFFFFFFFFFFFF
        )
    }
}

impl Uuid {
    /// Generate a new pseudo-UUID without external dependencies
    pub fn new() -> Self {
        let id1 = COUNTER1.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        Uuid(id1 as u128)
    }
}

/// Internal ID for Concepts
#[derive(Debug, PartialEq, Hash, Copy, Clone, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct ConceptId(Uuid);

impl core::fmt::Display for ConceptId {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_tuple("ConceptId").field(&self.0).finish()
    }
}

impl ConceptId {
    pub fn new() -> Self {
        Self(Uuid::new())
    }
}

#[derive(Debug, PartialEq, Hash, Clone, Eq, PartialOrd, Ord)]
enum Capability {
    // Core capabilities of the AI
    GenerateCode,  // Create code
    AnalyzeCode,   // Examine code
    OptimizeCode,  // Improve performance
    RefactorCode,  // Restructure code
    DebugCode,     // Find errors
    TestCode,      // Verify functionality
    DocumentCode,  // Add comments/docs
    FormatCode,    // Style code
    TranslateCode, // Convert between languages
    ExplainCode,   // Clarify code
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub enum PartOfSpeech {
    Noun,
    Verb,
    Article,
    Adjective,
    Adverb,
    Preposition,
    Pronoun,
    Conjunction,
    Interjection,
    Unknown,
}

impl PartOfSpeech {
    fn from_wordnet(pos: &wordnet::PartOfSpeech) -> Option<Self> {
        match pos {
            wordnet::PartOfSpeech::Noun => Some(PartOfSpeech::Noun),
            wordnet::PartOfSpeech::Verb => Some(PartOfSpeech::Verb),
            wordnet::PartOfSpeech::Adjective => Some(PartOfSpeech::Adjective),
            wordnet::PartOfSpeech::Adverb => Some(PartOfSpeech::Adverb),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub enum SemanticRelation {
    // Core semantic relationships
    IsA,         // Hypernym
    TypeOf,      // Instance hypernym
    HasSubtype,  // Hyponym
    PartOf,      // Holonym
    HasPart,     // Meronym
    OppositeOf,  // Antonym
    SimilarTo,   // Similar
    DerivedFrom, // Derivationally related
    Causes,      // Cause
    Entails,     // Entailment
    AttributeOf, // Attribute
    Modifies,    // Modifies (for adjectives and adverbs)
    ModifiedBy,  // Modified by
}

impl SemanticRelation {
    fn from_wordnet(rel: &wordnet::Relationship) -> Option<Self> {
        match rel {
            wordnet::Relationship::Hypernym => Some(SemanticRelation::IsA),
            wordnet::Relationship::InstanceHypernym => Some(SemanticRelation::TypeOf),
            wordnet::Relationship::Hyponym => Some(SemanticRelation::HasSubtype),
            wordnet::Relationship::MemberHolonym
            | wordnet::Relationship::SubstanceHolonym
            | wordnet::Relationship::PartHolonym => Some(SemanticRelation::PartOf),
            wordnet::Relationship::MemberMeronym
            | wordnet::Relationship::SubstanceMeronym
            | wordnet::Relationship::PartMeronym => Some(SemanticRelation::HasPart),
            wordnet::Relationship::Antonym => Some(SemanticRelation::OppositeOf),
            wordnet::Relationship::SimilarTo => Some(SemanticRelation::SimilarTo),
            wordnet::Relationship::DerivationallyRelated => Some(SemanticRelation::DerivedFrom),
            wordnet::Relationship::Cause => Some(SemanticRelation::Causes),
            wordnet::Relationship::Entailment => Some(SemanticRelation::Entails),
            wordnet::Relationship::Attribute => Some(SemanticRelation::AttributeOf),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct Concept {
    pub id: ConceptId,
    pub word: String,
    pub pos: PartOfSpeech,
    pub attributes: BTreeSet<ConceptId>,
    // Semantic relationships to other concepts
    pub relations: BTreeMap<SemanticRelation, BTreeSet<ConceptId>>,
    // WordNet synset IDs if available
    pub synsets: Vec<String>,
    // Definition from WordNet if available
    // pub definition: Option<String>,
}

#[derive(Debug)]
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

// WordNet integration to enhance semantic analysis
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct WordNetIntegration {
    db: wordnet::Database,
}

impl Default for WordNetIntegration {
    fn default() -> Self {
        Self::new()
    }
}

impl WordNetIntegration {
    pub fn new() -> Self {
        let files = &[
            &include_bytes!("../dict/data.adj")[..],
            &include_bytes!("../dict/data.adv")[..],
            &include_bytes!("../dict/data.noun")[..],
            &include_bytes!("../dict/data.verb")[..],
            &include_bytes!("../dict/index.adj")[..],
            &include_bytes!("../dict/index.adv")[..],
            &include_bytes!("../dict/index.noun")[..],
            &include_bytes!("../dict/index.sense")[..],
            &include_bytes!("../dict/index.verb")[..],
        ];

        let db = wordnet::initialize_database(files);

        Self { db }
    }

    // Get all possible parts of speech for a word
    pub fn get_pos(&self, word: &str) -> Vec<PartOfSpeech> {
        let senses = self.db.senses(word);
        let mut pos_set = BTreeSet::new();

        for sense in senses {
            if let Some(p) = PartOfSpeech::from_wordnet(&sense.part_of_speech) {
                pos_set.insert(p);
            }
        }

        pos_set.into_iter().collect()
    }

    // Get definitions for a word
    pub fn get_definitions(&self, word: &str) -> Vec<String> {
        let senses = self.db.senses(word);
        senses.iter().map(|s| s.gloss.clone()).collect()
    }

    // Get semantic relations for a word
    pub fn get_relations(&self, word: &str) -> BTreeMap<SemanticRelation, Vec<String>> {
        let senses = self.db.senses(word);
        let mut relations = BTreeMap::new();

        for sense in senses {
            for pointer in &sense.pointers {
                if let Some(rel) = SemanticRelation::from_wordnet(&pointer.relationship) {
                    let entry = relations.entry(rel).or_insert_with(Vec::new);

                    // Read the related concept
                    if let Some(s) = pointer.read() {
                        for syn in &s.synonyms {
                            entry.push(syn.word.clone());
                        }
                    }
                }
            }
        }

        relations
    }
}

// Main analyzer that combines basic SVO parsing with semantic analysis
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct SemanticAnalyzer {
    pub dictionary: BTreeMap<String, Concept>,
    #[serde(skip, default)]
    pub wordnet: WordNetIntegration,
    pub concepts_by_id: BTreeMap<ConceptId, String>, // Reverse lookup
}

impl SemanticAnalyzer {
    pub fn new_english() -> Self {
        Self {
            dictionary: BTreeMap::new(),
            wordnet: WordNetIntegration::new(),
            concepts_by_id: BTreeMap::new(),
        }
    }

    pub fn from_json(s: &str) -> Result<Self, String> {
        serde_json::from_str(s).map_err(|e| e.to_string())
    }

    pub fn to_json(&self) -> String {
        serde_json::to_string_pretty(self).unwrap_or_default()
    }

    // Add a word to the dictionary with wordnet enrichment if available
    pub fn add_word(&mut self, word: &str) -> ConceptId {
        let lower_word = word.to_lowercase();

        // If already in dictionary, return existing ID
        if let Some(concept) = self.dictionary.get(&lower_word) {
            return concept.id;
        }

        // Create the main concept
        let id = self.create_concept_without_relations(&lower_word);

        // Get the relations for just the first level
        let mut word_relations = BTreeMap::new();
        for (rel, related_words) in self.wordnet.get_relations(&lower_word) {
            let related_words_vec = related_words;
            word_relations.insert(rel, related_words_vec);
        }

        // Process relations without recursion
        for (rel, related_words) in word_relations {
            let mut related_ids = BTreeSet::new();

            for related in related_words {
                // If the related word is already in the dictionary, use its ID
                if let Some(concept) = self.dictionary.get(&related) {
                    related_ids.insert(concept.id);
                    continue;
                }

                // Otherwise, create it without processing its relations
                let related_id = self.create_concept_without_relations(&related);
                related_ids.insert(related_id);
            }

            // Update the original concept's relations
            if let Some(concept) = self.dictionary.get_mut(&lower_word) {
                concept.relations.insert(rel, related_ids);
            }
        }

        id
    }

    // Helper function to create a concept without processing its relations
    fn create_concept_without_relations(&mut self, word: &str) -> ConceptId {
        let lower_word = word.to_string();

        // If already in dictionary, return existing ID
        if let Some(concept) = self.dictionary.get(&lower_word) {
            return concept.id;
        }

        let id = ConceptId::new();
        let mut concept = Concept {
            id,
            word: word.to_string(),
            pos: PartOfSpeech::Unknown,
            attributes: BTreeSet::new(),
            relations: BTreeMap::new(),
            synsets: Vec::new(),
            // definition: None,
        };

        // Enrich with WordNet data
        // Get part of speech if not provided
        let pos_options = self.wordnet.get_pos(&lower_word);
        if !pos_options.is_empty() {
            concept.pos = pos_options[0].clone();
        }

        /*
        // Get definitions
        let defs = self.wordnet.get_definitions(&lower_word);
        if !defs.is_empty() {
            concept.definition = Some(defs[0].clone());
        }*/

        self.dictionary.insert(lower_word.clone(), concept);
        self.concepts_by_id.insert(id, lower_word);

        id
    }

    // Get a concept by ID
    pub fn get_concept(&self, id: ConceptId) -> Option<&Concept> {
        if let Some(word) = self.concepts_by_id.get(&id) {
            self.dictionary.get(word)
        } else {
            None
        }
    }

    // Calculate similarity between two concepts (0.0 to 1.0)
    pub fn concept_similarity(&self, id1: ConceptId, id2: ConceptId) -> Option<f32> {
        if id1 == id2 {
            return Some(1.0);
        }

        let c1 = self.get_concept(id1)?;
        let c2 = self.get_concept(id2)?;

        // Exact word match
        if c1.word.to_lowercase() == c2.word.to_lowercase() {
            return Some(1.0);
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
        let words: Vec<&str> = sentence.split_whitespace().collect();

        if words.len() < 2 {
            return None; // Need at least subject and verb
        }

        let mut i = 0;
        let mut subject = None;
        let mut subject_modifiers = Vec::new();

        // Find subject with modifiers
        while i < words.len() {
            let word = words[i].to_lowercase();
            if let Some(concept) = self.dictionary.get(&word) {
                match concept.pos {
                    PartOfSpeech::Article => {
                        i += 1;
                        continue;
                    }
                    PartOfSpeech::Adjective => {
                        if let Some(subj_id) = subject {
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
            i += 1;
        }

        // Find verb with modifiers
        let mut verb = None;
        let mut verb_modifiers = Vec::new();

        while i < words.len() {
            let word = words[i].to_lowercase();
            if let Some(concept) = self.dictionary.get(&word) {
                match concept.pos {
                    PartOfSpeech::Verb => {
                        verb = Some(concept.id);
                        i += 1;
                        break;
                    }
                    PartOfSpeech::Adverb => {
                        if let Some(v_id) = verb {
                            verb_modifiers.push((concept.id, SemanticRelation::Modifies));
                        }
                    }
                    _ => {}
                }
            }
            i += 1;
        }

        // Find object with modifiers
        let mut object = None;
        let mut object_modifiers = Vec::new();

        while i < words.len() {
            let word = words[i].to_lowercase();
            if let Some(concept) = self.dictionary.get(&word) {
                match concept.pos {
                    PartOfSpeech::Article => {
                        i += 1;
                        continue;
                    }
                    PartOfSpeech::Adjective => {
                        if let Some(obj_id) = object {
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

        for ((i, j), score) in connectivity {
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

mod wordnet {
    use std::cmp::Ordering;
    use std::str::from_utf8;

    use std::io;

    // New slice-based reader with identical interface
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
    pub struct ReadAtBytes {
        data: &'static [u8],
    }

    impl ReadAtBytes {
        pub fn new(data: &'static [u8]) -> Self {
            ReadAtBytes { data }
        }

        pub fn read_at(&self, buf: &mut [u8], pos: u64) -> io::Result<usize> {
            let pos = pos as usize;
            if pos >= self.data.len() {
                return Ok(0);
            }

            let remaining = self.data.len() - pos;
            let to_read = std::cmp::min(buf.len(), remaining);

            buf[..to_read].copy_from_slice(&self.data[pos..pos + to_read]);

            Ok(to_read)
        }
    }

    // Generic read_line function that works with any type implementing the read_at method
    fn read_line_at<R>(f: &R, mut pos: u64) -> Vec<u8>
    where
        R: ReadAt,
    {
        let mut block = vec![];
        let mut writing_to = 0usize;
        block.resize(512, 0u8);

        loop {
            let len = f.read_at(&mut block[writing_to..], pos).unwrap();
            let truncate = block
                .iter()
                .skip(writing_to)
                .enumerate()
                .find(|x| *x.1 == b'\n')
                .map(|x| x.0);
            if let Some(t) = truncate {
                block.truncate(writing_to + t);
                return block;
            }
            writing_to += len;
            pos += len as u64;
            let newlen = block.len() * 2;
            block.resize(newlen, 0u8);
        }
    }

    // Define trait for read_at functionality
    pub trait ReadAt {
        fn read_at(&self, buf: &mut [u8], pos: u64) -> std::io::Result<usize>;
    }

    // Implement for slice-based reader
    impl ReadAt for ReadAtBytes {
        fn read_at(&self, buf: &mut [u8], pos: u64) -> std::io::Result<usize> {
            self.read_at(buf, pos)
        }
    }

    // The rest of the original code with modifications to use the trait
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
    pub enum PartOfSpeech {
        Noun,
        Adjective,
        AdjectiveSatellite,
        Verb,
        Adverb,
    }

    impl PartOfSpeech {
        pub fn short(&self) -> &'static str {
            match *self {
                PartOfSpeech::Noun => "n",
                PartOfSpeech::Adjective => "adj",
                PartOfSpeech::AdjectiveSatellite => "adj",
                PartOfSpeech::Verb => "v",
                PartOfSpeech::Adverb => "adv",
            }
        }
    }

    fn part_of_speech_code_to_part_of_speech(code: &[u8]) -> PartOfSpeech {
        match code {
            b"n" => PartOfSpeech::Noun,
            b"v" => PartOfSpeech::Verb,
            b"a" => PartOfSpeech::Adjective,
            b"s" => PartOfSpeech::AdjectiveSatellite,
            b"r" => PartOfSpeech::Adverb,
            _ => panic!("impossible part of speech '{}'", from_utf8(code).unwrap()),
        }
    }

    #[derive(Debug, PartialEq)]
    pub enum Relationship {
        Antonym,
        Hypernym,
        InstanceHypernym,
        Hyponym,
        MemberHolonym,
        SubstanceHolonym,
        PartHolonym,
        MemberMeronym,
        SubstanceMeronym,
        PartMeronym,
        Attribute,
        DerivationallyRelated,
        DomainOfTopic,
        MemberOfTopic,
        DomainOfRegion,
        MemberOfRegion,
        DomainOfUsage,
        MemberOfUsage,
        Entailment,
        Cause,
        AlsoSee,
        VerbGroup,
        SimilarTo,
        VerbParticiple,
        PertainymOrDerivedFromAdjective,
    }

    fn relationship_code_to_relationship(code: &[u8]) -> Option<Relationship> {
        match code {
            b"!" => Some(Relationship::Antonym),
            b"@" => Some(Relationship::Hypernym),
            b"@i" => Some(Relationship::InstanceHypernym),
            b"~" => Some(Relationship::Hyponym),
            b"~i" => Some(Relationship::InstanceHypernym),
            b"#m" => Some(Relationship::MemberHolonym),
            b"#s" => Some(Relationship::SubstanceHolonym),
            b"#p" => Some(Relationship::PartHolonym),
            b"%m" => Some(Relationship::MemberMeronym),
            b"%s" => Some(Relationship::SubstanceMeronym),
            b"%p" => Some(Relationship::PartMeronym),
            b"=" => Some(Relationship::Attribute),
            b"+" => Some(Relationship::DerivationallyRelated),
            b";c" => Some(Relationship::DomainOfTopic),
            b"-c" => Some(Relationship::MemberOfTopic),
            b";r" => Some(Relationship::DomainOfRegion),
            b"-r" => Some(Relationship::MemberOfRegion),
            b";u" => Some(Relationship::DomainOfUsage),
            b"-u" => Some(Relationship::MemberOfUsage),
            b"*" => Some(Relationship::Entailment),
            b">" => Some(Relationship::Cause),
            b"^" => Some(Relationship::AlsoSee),
            b"$" => Some(Relationship::VerbGroup),
            b"&" => Some(Relationship::SimilarTo),
            b"<" => Some(Relationship::VerbParticiple),
            b"\\" => Some(Relationship::PertainymOrDerivedFromAdjective),
            _ => None,
        }
    }

    #[derive(Debug)]
    pub struct Sense<'db> {
        pub part_of_speech: PartOfSpeech,
        pub gloss: String,
        pub synonyms: Vec<SenseRef>,
        pub pointers: Vec<PointerRef<'db>>,
    }

    #[derive(Debug)]
    pub struct PointerRef<'db> {
        pub db: &'db Database,
        pub relationship: Relationship,
        pub part_of_speech: PartOfSpeech,
        pub offset: u64,
    }

    impl<'db> PointerRef<'db> {
        pub fn read(&self) -> Option<Sense<'db>> {
            self.db
                .dbfile_for_part_of_speech(&self.part_of_speech)
                .map(|r| r.read_sense(self.db, self.offset))
        }
    }

    #[derive(Debug)]
    pub struct SenseRef {
        pub word: String,
        pub lex_id: u32,
    }

    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
    struct DBFile {
        pub name: String,
        pub index: ReadAtBytes,
        pub index_size: u64,
        pub data: ReadAtBytes,
        pub part_of_speech: PartOfSpeech,
    }

    impl DBFile {
        fn is_found_here(&self, pos: u64, data: &[u8], remaining_word: &[u8]) -> Ordering {
            for x in 0..data.len() {
                if x == remaining_word.len() && data[x] == b' ' {
                    return Ordering::Equal;
                } else if x >= remaining_word.len() || data[x] > remaining_word[x] {
                    return Ordering::Less;
                } else if data[x] < remaining_word[x] {
                    return Ordering::Greater;
                }
            }
            let mut block = [0u8; 32];
            let bytes = self
                .index
                .read_at(&mut block, pos + data.len() as u64)
                .unwrap();
            return self.is_found_here(
                pos + data.len() as u64,
                &block[0..bytes],
                &remaining_word[data.len()..],
            );
        }

        fn find_position(&self, word: &[u8]) -> Option<u64> {
            let mut block = [0u8; 32];

            let mut end = self.index_size;
            let mut begin = 0u64;
            let mut pos = end / 2;

            while end - begin > (word.len() + 10) as u64 {
                if end - pos < 32 {
                    pos = begin;
                }

                let bytes = self.index.read_at(&mut block, pos).unwrap();
                let block = &block[0..bytes];

                if pos == begin {
                    begin += bytes as u64;
                }

                if let Some(newline_offset) = block
                    .iter()
                    .enumerate()
                    .find(|a| *a.1 == b'\n')
                    .map(|x| x.0)
                {
                    let newline = &block[newline_offset + 1..];
                    let current_line_starts_at = pos + newline_offset as u64 + 1;
                    let rel = self.is_found_here(current_line_starts_at, newline, word);
                    match rel {
                        Ordering::Equal => return Some(current_line_starts_at),
                        Ordering::Less => {
                            end = current_line_starts_at;
                        }
                        Ordering::Greater => {
                            begin = current_line_starts_at + word.len() as u64;
                        }
                    }

                    if begin >= end {
                        break;
                    }

                    let newpos = (end - begin) / 2 + begin;
                    if newpos == pos {
                        break;
                    } else {
                        pos = newpos;
                    }
                } else if (pos + bytes as u64) < end {
                    pos += bytes as u64;
                } else {
                    pos -= std::cmp::min(64, pos);
                }
            }

            None
        }

        fn read_sense<'db>(&self, database: &'db Database, offset: u64) -> Sense<'db> {
            let line = read_line_at(&self.data, offset);
            let sections: Vec<_> = line.split(|x| *x == b' ').collect();
            let part_of_speech = part_of_speech_code_to_part_of_speech(sections[2]);

            let mut index = 3;
            let synonyms_cnt =
                usize::from_str_radix(from_utf8(sections[index]).unwrap(), 16).unwrap();
            index += 1;

            let mut synonyms = vec![];
            synonyms.reserve(synonyms_cnt);

            for _sn in 0..synonyms_cnt {
                synonyms.push(SenseRef {
                    word: from_utf8(sections[index])
                        .unwrap()
                        .chars()
                        .map(|x| if x == '_' { ' ' } else { x })
                        .collect(),
                    lex_id: u32::from_str_radix(from_utf8(sections[index + 1]).unwrap(), 16)
                        .unwrap(),
                });
                index += 2;
            }

            let pointer_count =
                u32::from_str_radix(from_utf8(sections[index]).unwrap(), 10).unwrap();
            index += 1;

            let mut pointers = vec![];
            pointers.reserve(pointer_count as usize);

            for _pointern in 0..pointer_count {
                let rel = match relationship_code_to_relationship(sections[index]) {
                    Some(s) => s,
                    None => continue,
                };
                let offset =
                    u64::from_str_radix(from_utf8(sections[index + 1]).unwrap(), 10).unwrap();
                let part_of_speech = part_of_speech_code_to_part_of_speech(sections[index + 2]);
                let _offset =
                    u64::from_str_radix(from_utf8(sections[index + 3]).unwrap(), 16).unwrap();

                index += 4;
                pointers.push(PointerRef {
                    db: database,
                    relationship: rel,
                    part_of_speech: part_of_speech,
                    offset: offset,
                });
            }

            if sections[2] == b"v" {
                let frame_count =
                    usize::from_str_radix(from_utf8(sections[index]).unwrap(), 10).unwrap();
                index += frame_count + 1;
            }

            let gloss = {
                let line_utf = from_utf8(&line).unwrap();
                let gloss = &line_utf[line_utf.find('|').unwrap() + 2..];
                gloss
            };

            Sense {
                part_of_speech: part_of_speech,
                gloss: gloss.to_string(),
                synonyms: synonyms,
                pointers: pointers,
            }
        }

        fn senses<'db>(&self, database: &'db Database, word: &[u8]) -> Option<Vec<Sense<'db>>> {
            let offset = self.find_position(word)?;
            let line = read_line_at(&self.index, offset);
            let line = String::from_utf8(line).unwrap();
            let sections: Vec<&str> = line.split(' ').collect();

            let mut index = 2;
            let synset_cnt: u32 = sections[index].parse().unwrap();
            index += 1;

            let ptr_symbols_cnt: usize = sections[index].parse().unwrap();
            index += 1;
            index += ptr_symbols_cnt;
            index += 1; // skip sense_cnt
            index += 1; // skip tagsense_cnt

            let mut senses = vec![];
            senses.reserve(synset_cnt as usize);

            for synset in 0..synset_cnt {
                let synset_offset =
                    u64::from_str_radix(sections[index + synset as usize], 10).unwrap();
                senses.push(self.read_sense(database, synset_offset));
            }

            Some(senses)
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
    pub struct Database {
        pub db_files: Vec<DBFile>,
    }

    // Trait to abstract over different DB file implementations
    pub trait DBFileT: std::fmt::Debug {
        fn get_part_of_speech(&self) -> &PartOfSpeech;
        fn read_sense<'db>(&self, database: &'db Database, offset: u64) -> Sense<'db>;
        fn senses<'db>(&self, database: &'db Database, word: &[u8]) -> Option<Vec<Sense<'db>>>;
    }

    impl DBFileT for DBFile {
        fn get_part_of_speech(&self) -> &PartOfSpeech {
            &self.part_of_speech
        }

        fn read_sense<'db>(&self, database: &'db Database, offset: u64) -> Sense<'db> {
            self.read_sense(database, offset)
        }

        fn senses<'db>(&self, database: &'db Database, word: &[u8]) -> Option<Vec<Sense<'db>>> {
            self.senses(database, word)
        }
    }

    impl Database {
        // Create a database directly from byte slices
        pub fn from_bytes(files: &[&'static [u8]]) -> Database {
            // files are expected in this order:
            // data.adj, data.adv, data.noun, data.verb,
            // index.adj, index.adv, index.noun, index.verb

            let mut db_files = Vec::new();

            // Adjective
            db_files.push(DBFile {
                name: "index.adj".to_string(),
                index: ReadAtBytes::new(files[4]),
                index_size: files[4].len() as u64,
                data: ReadAtBytes::new(files[0]),
                part_of_speech: PartOfSpeech::Adjective,
            });

            // Adverb
            db_files.push(DBFile {
                name: "index.adv".to_string(),
                index: ReadAtBytes::new(files[5]),
                index_size: files[5].len() as u64,
                data: ReadAtBytes::new(files[1]),
                part_of_speech: PartOfSpeech::Adverb,
            });

            // Noun
            db_files.push(DBFile {
                name: "index.noun".to_string(),
                index: ReadAtBytes::new(files[6]),
                index_size: files[6].len() as u64,
                data: ReadAtBytes::new(files[2]),
                part_of_speech: PartOfSpeech::Noun,
            });

            // Verb
            db_files.push(DBFile {
                name: "index.verb".to_string(),
                index: ReadAtBytes::new(files[8]),
                index_size: files[8].len() as u64,
                data: ReadAtBytes::new(files[3]),
                part_of_speech: PartOfSpeech::Verb,
            });

            Database { db_files }
        }

        fn dbfile_for_part_of_speech(&self, part_of_speech: &PartOfSpeech) -> Option<&DBFile> {
            for db in &self.db_files {
                if db.get_part_of_speech() == part_of_speech {
                    return Some(db);
                }
            }
            None
        }

        pub fn senses(&self, word: &str) -> Vec<Sense> {
            let mut all = vec![];
            let word_bytes = word
                .to_lowercase()
                .chars()
                .map(|x| if x == ' ' { '_' } else { x })
                .collect::<String>()
                .into_bytes();

            for db in &self.db_files {
                if let Some(senses) = db.senses(self, &word_bytes) {
                    all.extend(senses);
                }
            }
            all
        }
    }

    // Example of usage:
    pub fn create_database_from_bytes(files: &[&'static [u8]]) -> Database {
        Database::from_bytes(files)
    }

    // This function directly initializes the database with the provided files
    pub fn initialize_database(files: &[&'static [u8]]) -> Database {
        Database {
            db_files: vec![
                DBFile {
                    name: "index.adj".to_string(),
                    index: ReadAtBytes::new(files[4]),
                    index_size: files[4].len() as u64,
                    data: ReadAtBytes::new(files[0]),
                    part_of_speech: PartOfSpeech::Adjective,
                },
                DBFile {
                    name: "index.adv".to_string(),
                    index: ReadAtBytes::new(files[5]),
                    index_size: files[5].len() as u64,
                    data: ReadAtBytes::new(files[1]),
                    part_of_speech: PartOfSpeech::Adverb,
                },
                DBFile {
                    name: "index.noun".to_string(),
                    index: ReadAtBytes::new(files[6]),
                    index_size: files[6].len() as u64,
                    data: ReadAtBytes::new(files[2]),
                    part_of_speech: PartOfSpeech::Noun,
                },
                DBFile {
                    name: "index.verb".to_string(),
                    index: ReadAtBytes::new(files[8]),
                    index_size: files[8].len() as u64,
                    data: ReadAtBytes::new(files[3]),
                    part_of_speech: PartOfSpeech::Verb,
                },
            ],
        }
    }
}
