# Skitter

An experimental bytecode interpreter for the Rust programming language. Not fit for any kind of serious use.

Currently uses rustc as a front-end, and supports a decent subset of the language.

The `std` crate is not yet supported, but a decent amount of code from `core` and `alloc` runs.

Major roadblocks remaining:
- Destructors
- FFI

Here is a very bad diagram of the project's high-level architecture:

![flowchart](http://tmp.bz/42d8WDmYISw.png)

## Usage

Requires a specific rust compiler and components specified in `rust-toolchain.md` which cargo should install automatically.
