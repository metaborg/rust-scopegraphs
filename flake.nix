{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };
  };

  outputs = { self, nixpkgs, rust-overlay, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ (import rust-overlay) ];
        };
      in {
        devShells.default = pkgs.mkShell rec {
          buildInputs = with pkgs; [
            # necessary for building wgpu in 3rd party packages (in most cases)
            libxkbcommon
            wayland
            xorg.libX11
            xorg.libXcursor
            xorg.libXrandr
            xorg.libXi
            alsa-lib
            fontconfig
            freetype
            shaderc
            directx-shader-compiler
            pkg-config
            cmake
            mold # could use any linker, needed for rustix (but mold is fast)

            libGL
            vulkan-headers
            vulkan-loader
            vulkan-tools
            vulkan-tools-lunarg
            vulkan-extension-layer
            vulkan-validation-layers # don't need them *strictly* but immensely helpful

            cargo-nextest
            cargo-fuzz
            cargo-watch

            # nice for developing wgpu itself
            typos

            # if you don't already have rust installed through other means,
            # this shell.nix can do that for you with this below
            yq # for tomlq below
            rustup

            # nice tools
            gdb
            rr
            evcxr
            valgrind
            renderdoc
            just

            # for this project
            nodePackages.browser-sync
          ];

          shellHook = ''
            export RUSTC_VERSION="$(tomlq -r .toolchain.channel rust-toolchain.toml)"
            export PATH="$PATH:''${CARGO_HOME:-~/.cargo}/bin"
            export PATH="$PATH:''${RUSTUP_HOME:-~/.rustup/toolchains/$RUSTC_VERSION-x86_64-unknown-linux/bin}"
            export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:${
              builtins.toString (pkgs.lib.makeLibraryPath buildInputs)
            }";

            # for this project
            cargo install cargo-docs-rs

            rustup default $RUSTC_VERSION
            rustup component add rust-src rust-analyzer

          '';
        };
      });
}
