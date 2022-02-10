# Monkey Programming Language

A Monkey Programming Language Interpreter written in Rust

*Note: This is still in development!*

I've been following the **[Writing an Interpreter in Go](https://interpreterbook.com/)** book and \
this is my attempt to write the Monkey interpreter in Rust. 

Please note that this is a personal project, done for fun, and may contain bugs. 

At the time of this commit, the interpreter is fairly basic, it is at the level at the end of the \
third chapter of the book. 

## Current Features: 
- let statements: `let x = 32;`
- if statements: `if (!!true) { 2 + 3 };`
- functions: `fn (x) { 2 * x }(10);`
- return statements: `return 3;`
- Prefix operators: `-3; !false;`
- Inflix operators: `+`, `-`, `*`, `/`, `>`, `>=`, `<`, `<=`, `==`, `!=`

## REPL
To run the REPL: 
`cargo run --release --bin repl`

## To interpreter files: 
`cargo run --release --bin monkey <path-to-file>`
