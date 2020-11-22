{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;

  nixPackages = [
    (haskellPackages.ghcWithPackages (pkgs: with pkgs; [
      http-streams optparse-generic terminal-progress-bar
    ]))
  ];
in
pkgs.stdenv.mkDerivation {
  name = "env";
  buildInputs = nixPackages;
}
