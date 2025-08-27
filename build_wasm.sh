#!/bin/sh
set -e
cargo build --target wasm32-unknown-unknown --release
wasm-bindgen target/wasm32-unknown-unknown/release/writing_an_interpreter_in_rust.wasm --out-dir ./wasm-out --target web
mkdir -p dist
cp wasm-out/* dist/