use crate::sentence::Tense;
use crate::uuid::Uuid;
use std::collections::BTreeMap;
use wn_parser::common::{Synset, SynsetType};

#[derive(Debug, Clone)]
pub struct SynsetEntry {
    pub uuid: Uuid,
    pub synset: Synset,
}

#[derive(Debug, Clone)]
pub struct SynsetDatabase {
    pub noun_synsets: BTreeMap<Uuid, Synset>,
    pub verb_synsets: BTreeMap<Uuid, Synset>,
    pub adj_synsets: BTreeMap<Uuid, Synset>,
    pub adv_synsets: BTreeMap<Uuid, Synset>,
    pub word_to_synsets: BTreeMap<String, Vec<(Uuid, SynsetType)>>,
    pub irregular_verbs: BTreeMap<String, (String, Tense)>,
}

fn get_irregular_verbs() -> BTreeMap<String, (String, Tense)> {
    let mut map = BTreeMap::new();
    map.insert("ate".to_string(), ("eat".to_string(), Tense::PastSimple));
    map.insert(
        "eaten".to_string(),
        ("eat".to_string(), Tense::PresentPerfect),
    );
    map
}

impl SynsetDatabase {
    /// Initializes and loads the database
    pub fn new() -> Result<Self, String> {
        let mut db = SynsetDatabase {
            noun_synsets: BTreeMap::new(),
            verb_synsets: BTreeMap::new(),
            adj_synsets: BTreeMap::new(),
            adv_synsets: BTreeMap::new(),
            word_to_synsets: BTreeMap::new(),
            irregular_verbs: get_irregular_verbs(),
        };
        db.load_data_file(include_bytes!("../dict/data.noun"), SynsetType::Noun)?;
        db.load_data_file(include_bytes!("../dict/data.verb"), SynsetType::Verb)?;
        db.load_data_file(include_bytes!("../dict/data.adj"), SynsetType::Adjective)?;
        db.load_data_file(include_bytes!("../dict/data.adv"), SynsetType::Adverb)?;
        db.build_word_index();
        Ok(db)
    }

    fn load_data_file(&mut self, data: &[u8], synset_type: SynsetType) -> Result<(), String> {
        let content = String::from_utf8_lossy(data);

        for line in content.lines() {
            if line.trim().is_empty() || line.starts_with(' ') {
                continue;
            }

            match wn_parser::data::parse_data_line(line) {
                Ok(synset) => {
                    let uuid = Uuid::new();
                    match synset_type {
                        SynsetType::Noun => {
                            self.noun_synsets.insert(uuid, synset);
                        }
                        SynsetType::Verb => {
                            self.verb_synsets.insert(uuid, synset);
                        }
                        SynsetType::Adjective | SynsetType::AdjectiveSatellite => {
                            self.adj_synsets.insert(uuid, synset);
                        }
                        SynsetType::Adverb => {
                            self.adv_synsets.insert(uuid, synset);
                        }
                        _ => {}
                    }
                }
                Err(e) => eprintln!("Error parsing data line: {}", e),
            }
        }

        Ok(())
    }

    fn build_word_index(&mut self) {
        for (uuid, synset) in &self.noun_synsets {
            for word in &synset.words {
                let entry = self
                    .word_to_synsets
                    .entry(word.word.to_lowercase())
                    .or_insert_with(Vec::new);
                entry.push((*uuid, SynsetType::Noun));
            }
        }

        for (uuid, synset) in &self.verb_synsets {
            for word in &synset.words {
                let entry = self
                    .word_to_synsets
                    .entry(word.word.to_lowercase())
                    .or_insert_with(Vec::new);
                entry.push((*uuid, SynsetType::Verb));
            }
        }

        for (uuid, synset) in &self.adj_synsets {
            for word in &synset.words {
                let entry = self
                    .word_to_synsets
                    .entry(word.word.to_lowercase())
                    .or_insert_with(Vec::new);
                entry.push((*uuid, SynsetType::Adjective));
            }
        }

        for (uuid, synset) in &self.adv_synsets {
            for word in &synset.words {
                let entry = self
                    .word_to_synsets
                    .entry(word.word.to_lowercase())
                    .or_insert_with(Vec::new);
                entry.push((*uuid, SynsetType::Adverb));
            }
        }
    }

    pub fn find_synsets_by_word(&self, word: &str) -> Vec<(Uuid, &Synset, SynsetType)> {
        let word = word.to_lowercase();
        let mut results = Vec::new();

        if let Some(synset_refs) = self.word_to_synsets.get(&word) {
            for (uuid, synset_type) in synset_refs {
                match synset_type {
                    SynsetType::Noun => {
                        if let Some(synset) = self.noun_synsets.get(uuid) {
                            results.push((*uuid, synset, SynsetType::Noun));
                        }
                    }
                    SynsetType::Verb => {
                        if let Some(synset) = self.verb_synsets.get(uuid) {
                            results.push((*uuid, synset, SynsetType::Verb));
                        }
                    }
                    SynsetType::Adjective | SynsetType::AdjectiveSatellite => {
                        if let Some(synset) = self.adj_synsets.get(uuid) {
                            results.push((*uuid, synset, SynsetType::Adjective));
                        }
                    }
                    SynsetType::Adverb => {
                        if let Some(synset) = self.adv_synsets.get(uuid) {
                            results.push((*uuid, synset, SynsetType::Adverb));
                        }
                    }
                    _ => {}
                }
            }
        }

        results
    }

    pub fn get_base_form(&self, word: &str) -> (String, Tense) {
        self.irregular_verbs
            .get(&word.to_lowercase())
            .cloned()
            .unwrap_or_else(|| (word.to_lowercase(), Tense::Infinitive))
    }
}
