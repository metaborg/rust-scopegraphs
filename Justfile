watch-docs:
    cargo +nightly watch -s 'cargo +nightly docs-rs -p scopegraphs && browser-sync start --ss target/x86_64-unknown-linux-gnu/doc -s target/x86_64-unknown-linux-gnu/doc --directory --no-open'

build-docs:
    rm -rf /tmp/SG_TARGET*
    cargo +nightly docs-rs -p scopegraphs
    rm -rf /tmp/SG_TARGET*

