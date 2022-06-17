{
  description = "panel-for-pen: xmonad-friendly desktop panel for pen tablet";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.11";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem flake-utils.lib.allSystems (system:
      let
        pkgs = import nixpkgs { inherit system; };
        hsenv = pkgs.haskellPackages.ghcWithPackages (p: [ p.cabal-install ]);
      in { devShell = pkgs.mkShell { buildInputs = [ hsenv ]; }; });
}
