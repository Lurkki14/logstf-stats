{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;

  nixPackages = [
    (haskellPackages.ghcWithPackages (pkgs: with pkgs; [
      http-streams
    ]))
  ];
in
pkgs.stdenv.mkDerivation {
  name = "env";
  buildInputs = nixPackages;
}
