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

fn analyze(causal_text: &mut Text, db: &SynsetDatabase) {

    if let Err(e) = causal_text.parse_linguistically(&db) {
        println!("Error parsing causal text: {}", e);
    } else {
        // Print parsed sentences
        if let Some(parsed_sentences) = &causal_text.parsed_sentences {
            println!("\nCausal text parsing results:");
            for (i, parsed) in parsed_sentences.iter().enumerate() {
                println!("Sentence {}:", i + 1);
                println!("{}", parsed.to_string());
            }
        }

        // Perform causal analysis
        println!("\nCausal relationships:");
        for relationship in causal_text.analyze_causality() {
            println!("  {} -> {} ({})", 
                relationship.cause, 
                relationship.effect, 
                match relationship.relationship_type {
                    rcai::text::RelationshipType::Direct => "direct cause",
                    rcai::text::RelationshipType::Enablement => "enablement",
                    rcai::text::RelationshipType::Prevention => "prevention",
                    rcai::text::RelationshipType::Replacement => "replacement",
                    rcai::text::RelationshipType::Temporal => "temporal sequence",
                }
            );
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

    // test_adverbial_clauses(&db);
    
    // Now, how do we express this text:
    let mut text = Text::parse(
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

    let mut text = Text::parse(
        "
        It has since been superseded by Swift, but most of the core libraries 
        and frameworks that are in use on Apple systems are still written in Objective-C, 
        and hence we would like the ability to interact with these using Rust. This 
        crate enables bi-directional interop with Objective-C, in as safe a 
        manner as possible.
    ",
    );

    analyze(&mut text, &db);

    // NOTE: for causal analysis, we need to sort the sentences from past to future tense
    // Past: Objective-C exists
    // Present: has been superseded (supersede being a keyword that triggers a "cause", such that:)

    // Initial state: Objective-C exists
    // Story: Superseded by Swift
    // Ask the program: "What is the present case?" - "superseded = replace", "replace what? replace Objective-C."
    // Output should be "Swift exists, Objective-C does not exist" (false, but good enough for our case)

    // Wikipedia: Bootloader, text is available under the Creative Commons Attribution-ShareAlike 4.0 License;
    let text3 = Text::parse("
        A bootloader, also spelled as boot loader or called bootstrap loader, 
        is a computer program that is responsible for booting a computer and 
        booting an operating system. If it also provides an interactive menu with multiple 
        boot choices then it's often called a boot manager.

        When a computer is turned off, its software‍—‌including operating systems, application 
        code, and data‍—‌remains stored on non-volatile memory. When the computer is powered on, 
        it typically does not have an operating system or its loader in random-access memory (RAM). 
        The computer first executes a relatively small program stored in the boot ROM, which is 
        read-only memory (ROM, and later EEPROM, NOR flash) along with some needed data, to 
        initialize hardware devices such as CPU, motherboard, memory, storage and other I/O
        devices, to access the nonvolatile device (usually block device, e.g., NAND flash) or 
        devices from which the operating system programs and data can be loaded into RAM.

        Some earlier computer systems, upon receiving a boot signal from a human operator or a peripheral device, 
        may load a very small number of fixed instructions into memory at a specific location, initialize at least 
        one CPU, and then point the CPU to the instructions and start their execution. These instructions typically 
        start an input operation from some peripheral device (which may be switch-selectable by the operator). Other 
        systems may send hardware commands directly to peripheral devices or I/O controllers that cause an extremely 
        simple input operation (such as read sector zero of the system device into memory starting at location 1000) 
        to be carried out, effectively loading a small number of boot loader instructions into memory; a completion 
        signal from the I/O device may then be used to start execution of the instructions by the CPU.

        Smaller computers often use less flexible but more automatic boot loader mechanisms to ensure that the computer 
        starts quickly and with a predetermined software configuration. In many desktop computers, for example, the 
        bootstrapping process begins with the CPU executing software contained in ROM (for example, the BIOS of an 
        IBM PC or an IBM PC compatible) at a predefined address (some CPUs, including the Intel x86 series, are designed 
        to execute this software after reset without outside help). This software contains rudimentary functionality to 
        search for devices eligible to participate in booting, and load a small program from a special section (most 
        commonly the boot sector) of the most promising device, typically starting at a fixed entry point such as the 
        start of the sector.
    ");
}
