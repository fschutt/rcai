use rcai::sentence::ParsedSentence;
use rcai::text::Text;
use rcai::wordnet::SynsetDatabase;

fn test_adverbial_clauses(db: &SynsetDatabase) {
    let examples = [
        // Time clauses
        "Her goldfish died when she was young.",
        "He came after night had fallen.",
        "We barely had gotten there when mighty Casey struck out.",
        "He told us his adventures in Arctic as we went along.",
        
        // Condition clauses
        "If they lose weight during an illness, they soon regain it afterwards.",
        
        // Purpose clauses
        "They had to take some of his land so that they could extend the churchyard.",
        
        // Reason clauses
        "I couldn't feel anger against him because I liked him too much.",
        
        // Concession clauses
        "I used to read a lot, though I don't have much time for books now.",
        
        // Place clauses
        "He said he was happy where he was.",
        
        // Comparison clauses
        "Johan can speak English as fluently as his teacher can.",
        "She is a better cook than I am.",
        
        // Manner clauses
        "I was never allowed to do things as I wanted to do.",
        "He spent a lot of money as if he was very rich.",
        
        // Result clauses
        "My suitcase had become so damaged that the lid would not stay closed.",
        
        // With adverbs of frequency
        "He is often late for work."
    ];
    
    for (i, example) in examples.iter().enumerate() {
        println!("Example {}: {}", i + 1, example);
        
        match ParsedSentence::new(example, db) {
            Ok(parsed) => {
                println!("{}", parsed.to_string());
                println!("-----------------------------------");
            },
            Err(e) => println!("Error parsing: {}", e),
        }
    }
}

fn main() {
    // Use the mock database for this example
    let db = SynsetDatabase::new().unwrap();

    // Get sentence from command line or use default
    let sentence = "A hungry tiger devour a small rodent";

    match ParsedSentence::new(&sentence, &db) {
        Ok(parsed) => println!("{}", parsed.to_string()),
        Err(e) => println!("Error parsing sentence: {}", e),
    }

    // OK:
    //
    // tiger   NOUN    UUID 002b5c INSTANCE 01cb9b MODIFIERS big TYPE subject
    // eat     VERB    UUID 0145e2 INSTANCE 01cb9c MODIFIERS unkindly TENSE past
    // mouse   NOUN    UUID 002fda INSTANCE 01cb9d MODIFIERS thirsty TYPE object

    test_adverbial_clauses(&db);
    
    // Now, how do we express this text:
    let text = Text::parse(
        "
        Objective-C was the standard programming language on Apple platforms like macOS, 
        iOS, iPadOS, tvOS and watchOS. It is an object-oriented language centered around 
        “sending messages” to its instances - this can for the most part be viewed as a 
        function call.
    ",
    );

    // The text contains 3 sentences:

    // objective-c SUBJECT
    // be VERB
    // language OBJECT

    // it (SUBJECT, refers to objective-c)
    // be VERB
    // language OBJECT,
    // -> modifiers: centered around sending messages to its instances

    // this SUBJECT (refers to "centered around sending messages to its instances")
    // view as VERB
    // function call NOUN

    let text = Text::parse(
        "
        It has since been superseded by Swift, but most of the core libraries 
        and frameworks that are in use on Apple systems are still written in Objective-C, 
        and hence we would like the ability to interact with these using Rust. This 
        crate enables bi-directional interop with Objective-C, in as safe a 
        manner as possible.
    ",
    );

    // NOTE: for causal analysis, we need to sort the sentences from past to future tense
    // Past: Objective-C exists
    // Present: has been superseded (supersede being a keyword that triggers a "cause", such that:)

    // Initial state: Objective-C exists
    // Story: Superseded by Swift
    // Ask the program: "What is the present case?" - "superseded = replace", "replace what? replace Objective-C."
    // Output should be "Swift exists, Objective-C does not exist" (false, but good enough for our case)
}
