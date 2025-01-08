{
  description = "haskell configuration.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    
  }; 
  outputs = { self, nixpkgs, ... }: let
    system = "x86_64-linux";
  in {
    devShells."${system}".default = let
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
      };
    in pkgs.mkShell {
      inputsFrom = [pkgs];
      buildInputs = [
        # Override SQLite to enable readline
        (pkgs.sqlite.overrideAttrs (old: {
          configureFlags = old.configureFlags or [] ++ [ "--enable-readline" ];
          buildInputs = (old.buildInputs or []) ++ [ pkgs.readline ];
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
      ];
      LD_LIBRARY_PATH=with pkgs; "${freeglut}/lib";
      SQLITE_FLAGS="--enable-readline";
    };
  };
}
