#!/usr/bin/env bash
set -ex

ln -sf ../../pre-commit.sh .git/hooks/pre-commit

rustup component add llvm-tools-preview
cargo install cargo-binutils rustfilt cargo-audit

export RUSTFLAGS="-D warnings"
cargo fmt -- --check
cargo build
cargo clippy
cargo test
# https://doc.rust-lang.org/nightly/unstable-book/compiler-flags/instrument-coverage.html
RUSTFLAGS="-Z instrument-coverage" cargo test
cargo profdata -- merge -sparse default.profraw -o default.profdata
cargo cov -- report --use-color --ignore-filename-regex='/.cargo/registry' --instr-profile=default.profdata --object target/debug/deps/ical_rrule-b2a70cd2ca5a8a21
cargo cov -- show --use-color --ignore-filename-regex='/.cargo/registry' --instr-profile=default.profdata --object target/debug/deps/ical_rrule-b2a70cd2ca5a8a21 --show-instantiations --show-line-counts-or-regions --Xdemangler=rustfilt

cargo audit