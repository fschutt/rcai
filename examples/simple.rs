use rcai::{PartOfSpeech, SemanticAnalyzer};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Create analyzer with WordNet integration
    let mut analyzer = SemanticAnalyzer::new_english();

    // Populate dictionary for basic analysis
    // Even without WordNet, this will allow basic parsing
    let words = [
        ("the", PartOfSpeech::Article),
        ("a", PartOfSpeech::Article),
        ("an", PartOfSpeech::Article),
        ("lion", PartOfSpeech::Noun),
        ("tiger", PartOfSpeech::Noun),
        ("cat", PartOfSpeech::Noun),
        ("dog", PartOfSpeech::Noun),
        ("animal", PartOfSpeech::Noun),
        ("mouse", PartOfSpeech::Noun),
        ("rodent", PartOfSpeech::Noun),
        ("prey", PartOfSpeech::Noun),
        ("food", PartOfSpeech::Noun),
        ("ate", PartOfSpeech::Verb),
        ("devoured", PartOfSpeech::Verb),
        ("consumed", PartOfSpeech::Verb),
        ("chased", PartOfSpeech::Verb),
        ("quickly", PartOfSpeech::Adverb),
        ("slowly", PartOfSpeech::Adverb),
        ("hungry", PartOfSpeech::Adjective),
        ("large", PartOfSpeech::Adjective),
        ("small", PartOfSpeech::Adjective),
        ("fierce", PartOfSpeech::Adjective),
    ];

    for (word, _) in words {
        analyzer.add_word(word);
    }

    // Define a set of example sentences
    let sentences = [
        "The lion ate a mouse",
        "A hungry tiger devoured a small rodent",
        "The cat quickly chased the mouse",
        "A large dog ate the food slowly",
        "The fierce tiger consumed its prey",
    ];

    // Parse sentences
    let parsed_sentences: Vec<_> = sentences
        .iter()
        .filter_map(|s| {
            let parsed = analyzer.parse(s);
            if parsed.is_none() {
                println!("Failed to parse: {}", s);
            }
            parsed
        })
        .collect();

    println!("Successfully parsed {} sentences", parsed_sentences.len());

    // Calculate connectivity between sentences
    let connectivity = analyzer.sentence_connectivity(&parsed_sentences);

    println!("\nSentence Connectivity Matrix:");
    println!("----------------------------");

    for i in 0..sentences.len() {
        for j in 0..sentences.len() {
            if i == j {
                print!("1.00 ");
            } else if let Some(score) = connectivity.get(&(i, j)) {
                print!("{:.2} ", score);
            } else {
                print!("---- ");
            }
        }
        println!();
    }

    // Find the most central sentence
    if let Some(central_idx) = analyzer.find_central_sentence(&parsed_sentences) {
        println!("\nMost central sentence: \"{}\"", sentences[central_idx]);
    }

    // Show detailed semantic analysis of each sentence
    println!("\nDetailed Semantic Analysis:");
    println!("---------------------------");

    for (i, sentence) in parsed_sentences.iter().enumerate() {
        println!("\n{}. \"{}\"", i + 1, sentences[i]);

        let subject = analyzer.get_concept(sentence.subject).unwrap();
        println!("  Subject: {} ({})", subject.word, sentence.subject);

        for (mod_id, rel) in &sentence.subject_modifiers {
            if let Some(modifier) = analyzer.get_concept(*mod_id) {
                println!("    Modified by: {} ({:?})", modifier.word, rel);
            }
        }

        let verb = analyzer.get_concept(sentence.verb).unwrap();
        println!("  Verb: {} ({})", verb.word, sentence.verb);

        for (mod_id, rel) in &sentence.verb_modifiers {
            if let Some(modifier) = analyzer.get_concept(*mod_id) {
                println!("    Modified by: {} ({:?})", modifier.word, rel);
            }
        }

        if let Some(obj_id) = sentence.object {
            if let Some(object) = analyzer.get_concept(obj_id) {
                println!("  Object: {} ({})", object.word, obj_id);

                for (mod_id, rel) in &sentence.object_modifiers {
                    if let Some(modifier) = analyzer.get_concept(*mod_id) {
                        println!("    Modified by: {} ({:?})", modifier.word, rel);
                    }
                }
            }
        }
    }

    Ok(())
}
