use rcai::sentence::ParsedSentence;
use rcai::text::Text;
use rcai::wordnet::SynsetDatabase;

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
