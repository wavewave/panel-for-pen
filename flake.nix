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
        newHs = pkgs.haskellPackages.override (old: {
          overrides = self: super: {
            "panel-for-pen" = self.callCabal2nix "panel-for-pen" ./. { };
          };
        });
        hsenv = newHs.ghcWithPackages
          (p: [ p.cabal-install p.gi-gtk p.errors p.X11 ]);
      in {
        defaultPackage = newHs.panel-for-pen;
        devShell = pkgs.mkShell { buildInputs = [ hsenv ]; };
      });
}
