static COUNTER1: AtomicU64 = AtomicU64::new(0);

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

// Main analyzer that combines basic SVO parsing with semantic analysis
#[derive(Debug, Default, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct SemanticAnalyzer {
    pub dictionary: BTreeMap<String, Concept>,
    pub concepts_by_id: BTreeMap<ConceptId, String>, // Reverse lookup
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
    RelatedTo,   // Generic relation from glossary
}

impl SemanticRelation {
    pub fn from_relation(rel: &Relationship) -> Option<Self> {
        match rel {
            Relationship::Hypernym => Some(SemanticRelation::IsA),
            Relationship::InstanceHypernym => Some(SemanticRelation::TypeOf),
            Relationship::Hyponym => Some(SemanticRelation::HasSubtype),
            Relationship::MemberHolonym
            | Relationship::SubstanceHolonym
            | Relationship::PartHolonym => Some(SemanticRelation::PartOf),
            Relationship::MemberMeronym
            | Relationship::SubstanceMeronym
            | Relationship::PartMeronym => Some(SemanticRelation::HasPart),
            Relationship::Antonym => Some(SemanticRelation::OppositeOf),
            Relationship::SimilarTo => Some(SemanticRelation::SimilarTo),
            Relationship::DerivationallyRelated => Some(SemanticRelation::DerivedFrom),
            Relationship::Cause => Some(SemanticRelation::Causes),
            Relationship::Entailment => Some(SemanticRelation::Entails),
            Relationship::Attribute => Some(SemanticRelation::AttributeOf),
            _ => None,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub enum PartOfSpeech {
    Noun,
    Verb,
    Article,
    Adjective,
    Adverb,
    AdjectiveSatellite,
    Preposition,
    Pronoun,
    Conjunction,
    Interjection,
    Unknown,
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
}

// WordNet integration to enhance semantic analysis
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct WordNetIntegration {
    db: Database,
}

impl WordNetIntegration {
    pub fn new_english() -> Self {
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

        let db = initialize_database(files);

        Self { db }
    }

    // Get all possible parts of speech for a word
    pub fn get_pos(&self, word: &str) -> Vec<PartOfSpeech> {
        println!("get_pos for word {word}");
        let senses = self.db.senses(word);
        println!("got {} senses", senses.len());

        let mut pos_set = BTreeSet::new();

        for sense in senses {
            pos_set.insert(sense.part_of_speech);
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
        println!("get relations for word {word}...");
        let senses = self.db.senses(word);
        println!("ok got {} senses", senses.len());

        let mut relations = BTreeMap::new();

        for sense in senses {
            for pointer in &sense.pointers {

                let rel = match SemanticRelation::from_relation(&pointer.relationship) {
                    Some(s) => s,
                    None => continue,
                };

                let entry = relations.entry(rel).or_insert_with(Vec::new);

                // Read the related concept
                if let Some(s) = pointer.read() {
                    for syn in &s.synonyms {
                        entry.push(syn.word.clone());
                    }
                }
            }
        }

        println!("done");
        relations
    }
}

use core::cmp::Ordering;
use core::str::from_utf8;

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

#[derive(Debug, PartialEq, Ord, PartialOrd, Eq, Serialize, Deserialize)]
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
            .and_then(|r| r.read_sense(self.db, self.offset))
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

    fn get_part_of_speech(&self) -> &PartOfSpeech {
        &self.part_of_speech
    }

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

    fn read_sense<'db>(&self, database: &'db Database, offset: u64) -> Option<Sense<'db>> {
        
        let line = read_line_at(&self.data, offset);
        let sections: Vec<_> = line.split(|x| *x == b' ').collect();
        let part_of_speech = match part_of_speech_code_to_part_of_speech(sections[2]) {
            Some(s) => s,
            None => return None,
        };

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
            
            let part_of_speech = match part_of_speech_code_to_part_of_speech(sections[index + 2]) {
                Some(s) => s,
                None => continue,
            };

            let _offset =
                u64::from_str_radix(from_utf8(sections[index + 3]).unwrap(), 16).unwrap();

            index += 4;
            pointers.push(PointerRef {
                db: database,
                relationship: rel,
                part_of_speech,
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

        Some(Sense {
            part_of_speech,
            gloss: gloss.to_string(),
            synonyms: synonyms,
            pointers: pointers,
        })
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
            if let Some(s) = self.read_sense(database, synset_offset) {
                senses.push(s);
            }
        }

        Some(senses)
    }
}

fn part_of_speech_code_to_part_of_speech(code: &[u8]) -> Option<PartOfSpeech> {
    match code {
        b"n" => Some(PartOfSpeech::Noun),
        b"v" => Some(PartOfSpeech::Verb),
        b"a" => Some(PartOfSpeech::Adjective),
        b"r" => Some(PartOfSpeech::Adverb),
        b"s" => Some(PartOfSpeech::AdjectiveSatellite),
        _ => { 
            println!("unknown part of speech '{}'", from_utf8(code).unwrap()); 
            None 
        },
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Database {
    pub db_files: Vec<DBFile>,
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