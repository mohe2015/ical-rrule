#!/usr/bin/env bash
ln -s ../../pre-commit.sh .git/hooks/pre-commit

set -e

cargo fmt -- --check
cargo check
cargo clippy
