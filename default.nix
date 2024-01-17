{ pkgs ? import <nixpkgs> {} }:
pkgs.stdenv.mkDerivation {
  pname = "pl-compiler";
  version = "0.0.0";
  system = "x86_64-linux";

  src = ./src;

  nativeBuildInputs = [
    (import ./c3c.nix { inherit pkgs; })
  ];

  buildInputs = with pkgs; [
    lld
    llvm
  ];

  buildPhase = ''
    echo "building the compiler"
    mkdir -p $out/bin
    c3c compile $src/main.c3 -o $out/bin/pl-compiler
  '';

  installPhase = ''
  '';

}
