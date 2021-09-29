https://rust-fuzz.github.io/book

cargo install cargo-fuzz
cargo fuzz run fuzz_target_1
cargo fuzz run inverse_equal
cargo fuzz coverage fuzz_target_1
cargo cov -- show fuzz/target/x86_64-unknown-linux-gnu/release/fuzz_target_1 --format=html -instr-profile=fuzz/coverage/fuzz_target_1/coverage.profdata --output-dir=target/debug/fuzz/coverage
firefox target/debug/fuzz/coverage/index.html

#cargo install afl # didn't manage to get this to work on NixOS