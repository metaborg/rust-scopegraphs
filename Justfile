watch-docs:
    cargo +nightly watch -s 'cargo +nightly docs-rs -p scopegraphs && browser-sync start --ss target/doc -s target/doc --directory --no-open'
