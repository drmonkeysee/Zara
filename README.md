# Zara

A [Scheme](https://www.scheme.org) interpreter written in [Rust](https://www.rust-lang.org). The current plan is to implement the [R7RS-small](https://small.r7rs.org) standard which describes a minimal, academic-oriented variant as opposed to the more expansive and practical R5RS and R6RS standards.

The focus of this project is exploring the Rust programming language and building a Scheme interpreter from the ground-up, so the usage of 3rd-party crates will be minimal.

## Build

Rust's toolchain is so intuitive this section is probably redundant, but here goes. The Zara package is organized as two crates:

- **binary**: the REPL and other CLI functionality
- **lib**: the Scheme interpreter

### All (Rust-supported) Platforms

1. Install [Rust](https://www.rust-lang.org/tools/install)
2. `cargo run`

## Dependencies

- [RustyLine](https://crates.io/crates/rustyline)

## Resources

- [Scheme](https://www.scheme.org)
	- [Standards](https://standards.scheme.org)
- [R7RS Working Group](https://small.r7rs.org)
- [The Scheme Programming Language (4th Edition)](https://scheme.com/tspl4/)
