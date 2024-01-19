{ pkgs ? import <nixpkgs> {} }:
let
  c3c = import ./c3c.nix { inherit pkgs; };
  #pl-compiler = import ./default.nix { inherit pkgs; };
in
pkgs.mkShell {

  buildInputs = [
    c3c
    pkgs.qbe
    #pl-compiler
  ];

}
