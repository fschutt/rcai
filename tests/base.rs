use rcai::*;

#[test]
fn base_test() {
    // Create analyzer and add words from our test sentence
    let mut analyzer = SemanticAnalyzer::new_english();
    analyzer.add_word("lion");
    println!("{:#?}", analyzer.concepts_by_id);
}

#[test]
fn basic_parsing() {
    // Create analyzer and add words from our test sentence
    let mut analyzer = SemanticAnalyzer::new_english();
    analyzer.add_word("the");
    analyzer.add_word("a");
    analyzer.add_word("lion");
    analyzer.add_word("mouse");
    analyzer.add_word("ate");

    // Parse test sentence
    let test_sentence = "The lion ate a mouse";
    if let Some(parsed) = analyzer.parse(test_sentence) {
        println!("Parsed sentence:");
        println!("Subject: {}", parsed.subject);
        println!("Verb: {}", parsed.verb);
        println!("Object: {:?}", parsed.object);
    } else {
        println!("Failed to parse the sentence");
    }
}

#[test]
fn wordnet_integration() {
    let mut analyzer = SemanticAnalyzer::new_english();

    // Add some words
    let lion_id = analyzer.add_word("lion");
    let tiger_id = analyzer.add_word("tiger");
    let cat_id = analyzer.add_word("cat");
    let dog_id = analyzer.add_word("dog");

    // Check similarities
    if let Some(sim) = analyzer.concept_similarity(lion_id, tiger_id) {
        println!("Lion-Tiger similarity: {:.2}", sim);
        assert!(sim > 0.5, "Lions and tigers should be similar");
    }

    if let Some(sim) = analyzer.concept_similarity(lion_id, cat_id) {
        println!("Lion-Cat similarity: {:.2}", sim);
        assert!(sim > 0.3, "Lions and cats should be somewhat similar");
    }

    if let Some(sim) = analyzer.concept_similarity(lion_id, dog_id) {
        println!("Lion-Dog similarity: {:.2}", sim);
        assert!(
            sim < sim,
            "Lions should be less similar to dogs than to cats"
        );
    }
}

#[test]
fn sentence_similarity() {
    let mut analyzer = SemanticAnalyzer::new_english();

    // Add some words
    analyzer.add_word("the");
    analyzer.add_word("a");
    analyzer.add_word("lion");
    analyzer.add_word("tiger");
    analyzer.add_word("cat");
    analyzer.add_word("ate");
    analyzer.add_word("devoured");
    analyzer.add_word("mouse");
    analyzer.add_word("rodent");

    // Parse sentences
    let sentences = [
        "The lion ate a mouse",
        "A tiger devoured a rodent",
        "The cat ate a mouse",
    ];

    let parsed_sentences: Vec<_> = sentences.iter().filter_map(|s| analyzer.parse(s)).collect();

    if parsed_sentences.len() == 3 {
        let sim_0_1 = parsed_sentences[0].similarity(&parsed_sentences[1], &analyzer);
        let sim_0_2 = parsed_sentences[0].similarity(&parsed_sentences[2], &analyzer);
        let sim_1_2 = parsed_sentences[1].similarity(&parsed_sentences[2], &analyzer);

        println!("Similarity 0-1: {:.2}", sim_0_1);
        println!("Similarity 0-2: {:.2}", sim_0_2);
        println!("Similarity 1-2: {:.2}", sim_1_2);

        // Sentence 0 and 2 should be more similar (same verb, same object)
        assert!(
            sim_0_2 > sim_0_1,
            "Expected sentence 0 to be more similar to 2 than to 1"
        );
    }
}
