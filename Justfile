watch-docs:
    cargo watch -s 'cargo +nightly doc --features unstable-doc && browser-sync start --ss target/doc -s target/doc --directory --no-open'   