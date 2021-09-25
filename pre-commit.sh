#!/usr/bin/env bash
set -ex

ln -sf ../../pre-commit.sh .git/hooks/pre-commit

rustup component add llvm-tools-preview
cargo install cargo-binutils rustfilt cargo-audit grcov

export RUSTFLAGS="-D warnings -Z instrument-coverage"
cargo fmt -- --check
# https://doc.rust-lang.org/nightly/unstable-book/compiler-flags/instrument-coverage.html
#cargo clean # needed to find out the object later
cargo test
cargo clippy
cargo profdata -- merge -sparse default.profraw -o default.profdata
cargo cov -- show --use-color --ignore-filename-regex='(/.cargo/registry)|(/rustc/)' --instr-profile=default.profdata --object $(cargo test --no-run --message-format=json | jq -r "select(.profile.test == true) | .filenames[]") --show-instantiations --show-line-counts-or-regions --Xdemangler=rustfilt
cargo cov -- show --format=html --use-color --ignore-filename-regex='(/.cargo/registry)|(/rustc/)' --instr-profile=default.profdata --object $(cargo test --no-run --message-format=json | jq -r "select(.profile.test == true) | .filenames[]") --output-dir=target/debug/coverage --show-instantiations --show-line-counts-or-regions --Xdemangler=rustfilt
firefox target/debug/coverage/index.html
cargo cov -- report --use-color --ignore-filename-regex='(/.cargo/registry)|(/rustc/)' --instr-profile=default.profdata --object $(cargo test --no-run --message-format=json | jq -r "select(.profile.test == true) | .filenames[]")

# bad coverage for unused instantiations
#~/.cargo/bin/grcov . -s . --binary-path ./target/debug/ -t html --branch --ignore-not-existing -o ./target/debug/grcov/
#firefox target/debug/grcov/index.html

cargo audit