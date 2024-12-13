use ll1_checker::CFG;

use std::io::{self, Write};

fn main() {
    let mut input = String::new();

    println!("Specify CFG = (Σ, V, P, S)");

    println!("Enter a set of terminals Σ (space-separated):");
    io::stdout().flush().unwrap();
    input.clear();
    io::stdin().read_line(&mut input).unwrap();
    let terminals: Vec<String> = input.trim().split_whitespace().map(String::from).collect();

    println!("Enter a set of non terminals V (space-separated):");
    io::stdout().flush().unwrap();
    input.clear();
    io::stdin().read_line(&mut input).unwrap();
    let non_terminals: Vec<String> = input.trim().split_whitespace().map(String::from).collect();

    println!(
        "Enter a set of productions in the format symbol1:[symbol2 symbol3], symbol4:[symbol5 symbol6]:"
    );
    io::stdout().flush().unwrap();
    input.clear();
    io::stdin().read_line(&mut input).unwrap();
    let productions: Vec<(String, Vec<String>)> = input
        .trim()
        .split(", ")
        .map(|pair| {
            let mut parts = pair.split(':');
            let key = parts.next().unwrap().to_string();
            let values = parts
                .next()
                .unwrap()
                .trim_matches(&['[', ']'] as &[_])
                .split_whitespace()
                .map(String::from)
                .collect();
            (key, values)
        })
        .collect();

    println!("Enter a start symbol S:");
    io::stdout().flush().unwrap();
    input.clear();
    io::stdin().read_line(&mut input).unwrap();
    let start_symbol: String = input.trim().to_string();

    // Print the parsed inputs
    println!("\nParsed inputs:");
    println!("Σ: {:?}", terminals);
    println!("V: {:?}", non_terminals);
    println!("P: {:?}", productions);
    println!("S: {}", start_symbol);

    let g = CFG::new(terminals, non_terminals, productions, start_symbol);

    println!("Your CFG is{} ll(1).", if g.is_ll1() { "" } else { " not" });

    g.show_director_sets();
}
