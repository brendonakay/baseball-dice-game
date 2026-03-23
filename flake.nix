{
  description = "haskell configuration.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

  };
  outputs =
    { self, nixpkgs, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
      };
    in
    {
      # Build the Lean DiffTest oracle binary using nixpkgs' lean4.
      # The resulting binary is NixOS-compatible with no manual patching needed.
      # Usage: nix build .#difftest  →  result/bin/DiffTest
      packages."${system}".difftest = pkgs.stdenv.mkDerivation {
        name = "baseball-difftest";
        src = pkgs.lib.cleanSourceWith {
          src = ./lean;
          filter = path: type:
            # Exclude lake build artifacts and elan cache
            !(pkgs.lib.hasInfix "/.lake/" path);
        };

        nativeBuildInputs = [ pkgs.lean4 ];

        postPatch = ''
          # Remove the elan toolchain pin so lake uses the nixpkgs lean4
          # instead of trying to invoke elan to switch versions.
          # Remove the manifest so lake regenerates it cleanly (no external deps
          # means this is safe — lake just builds without resolving any packages).
          rm -f lean-toolchain lake-manifest.json
        '';

        buildPhase = ''
          export HOME=$TMPDIR
          lake build DiffTest
        '';

        installPhase = ''
          install -Dm755 .lake/build/bin/DiffTest $out/bin/DiffTest
        '';
      };

      devShells."${system}".default =
        pkgs.mkShell {
          inputsFrom = [ pkgs ];
          buildInputs = [
            # Override SQLite to enable readline
            (pkgs.sqlite.overrideAttrs (old: {
              configureFlags = old.configureFlags or [ ] ++ [ "--enable-readline" ];
              buildInputs = (old.buildInputs or [ ]) ++ [ pkgs.readline ];
            }))
          ];
          packages = with pkgs; [
            ghc
            cabal-install
            haskell-language-server
            haskellPackages.hlint
            haskellPackages.ghcid
            haskellPackages.ormolu
            libz
            jq
            elan
            lean4
          ];
          LD_LIBRARY_PATH = with pkgs; "${freeglut}/lib";
          SQLITE_FLAGS = "--enable-readline";
        };
    };
}
