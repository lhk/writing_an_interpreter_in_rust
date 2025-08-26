This is a Rust implementation of **Writing an Interpreter in Go** by Thorsten Ball.
The book is amazing, and to really understand how everything fits together, I thought it would be a good exercise to translate the codebase from Go to Rust.

I aimed to use idiomatic Rust and refactored some of the Go patterns, especially the AST data structures and some of the dispatch mechanisms in the Pratt parser.

Apart from that, I stuck to the original, and the code in `src/` should closely mimic Thorsten Ball’s code.
For the tests, I used AI to quickly and roughly translate them from the original Go tests to Rust. I was a bit surprised at how much the LLM struggled with typing, and how often it was tripped up by the changes I made in the AST/parsing and some of the evaluation logic. No guarantees for the tests! They pass, but may still contain some messy tripwires.

Next up "Crafting Interpreters" in Zig? :)