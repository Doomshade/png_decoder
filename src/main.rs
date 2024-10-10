use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();
    let file = args.get(1).expect("Please provide a path to the PNG file");
    dbg!(file);
    let contents =
        fs::read_to_string(file).expect(format!("Failed to open {}", file.as_str()).as_str());
    dbg!(args);
}
