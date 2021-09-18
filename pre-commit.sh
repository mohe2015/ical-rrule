#!/usr/bin/env bash
set -ex

ln -sf ../../pre-commit.sh .git/hooks/pre-commit
export RUSTFLAGS="-D warnings"
cargo fmt -- --check
cargo build
cargo clippy
cargo test
