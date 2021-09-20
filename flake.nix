{
  description = "ical-rrule's flake";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let pkgs = nixpkgs.legacyPackages.${system}; in
        {
          devShell = pkgs.mkShell.override { stdenv = pkgs.llvmPackages_12.stdenv; } {
            nativeBuildInputs = [
              pkgs.bashInteractive # fix nested shells
              pkgs.pkg-config
              pkgs.gnumake
              pkgs.llvmPackages_12.clang
              pkgs.llvmPackages_12.llvm
              pkgs.llvmPackages_12.llvm.dev
              pkgs.llvmPackages_12.lld
            ];

            buildInputs = [
              pkgs.openssl
            ];
          };

          packages = rec {
            ical-rrule = pkgs.rustPlatform.buildRustPackage rec {
              pname = "ical-rrule";
              version = "0.1.0";
              src = pkgs.nix-gitignore.gitignoreSource [ ./.gitignore "flake.nix" "result" ] ./.;

              nativeBuildInputs = [ pkgs.pkg-config ];
              buildInputs = [
                pkgs.openssl
              ];

              cargoLock = {
                lockFile = ./Cargo.lock;
              };
            };
          };
        }
      );
}
