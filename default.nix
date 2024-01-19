{ pkgs ? import <nixpkgs> {} }:
pkgs.stdenv.mkDerivation {
  pname = "pl-compiler";
  version = "0.0.0";
  system = "x86_64-linux";

  src = ./src;

  nativeBuildInputs = [
    (import ./c3c.nix { inherit pkgs; })
  ];

  buildInputs = [
    pkgs.qbe
  ];

  buildPhase = ''
    echo "building the compiler"
    mkdir -p $out/bin
    c3c compile $src/* -o $out/bin/pl-compiler
  '';

  installPhase = ''
  '';

}
