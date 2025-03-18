use rcai::SemanticAnalyzer;

fn main() {
    let mut analyzer = SemanticAnalyzer::new_english();
    for w in ["the", "lion", "ate", "a", "mouse"] {
        println!("adding word {w}");
        analyzer.add_word(w);
        println!("added!");
    }
    std::fs::write("./output.json", analyzer.to_json());
}
