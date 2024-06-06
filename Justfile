watch-docs:
    cargo +nightly watch -s 'cargo +nightly docs-rs -p scopegraphs && browser-sync start --ss target/x86_64-unknown-linux-gnu/doc -s target/x86_64-unknown-linux-gnu/doc --directory --no-open'

publish:
    cargo publish -p scopegraphs-render-docs
    cargo publish -p scopegraphs-regular-expressions
    cargo publish -p scopegraphs-macros
    cargo publish -p scopegraphs
