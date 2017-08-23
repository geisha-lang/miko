extern crate peg;

fn main() {
    peg::cargo_build("src/syntax/parser/grammar.rustpeg");
}
