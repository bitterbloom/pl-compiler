{ pkgs ? import <nixpkgs> {} }:
let
  c3c = import ./c3c.nix { inherit pkgs; };
in
pkgs.stdenv.mkDerivation {
  pname = "pl-compiler";
  version = "0.0.0";
  system = "x86_64-linux";

  src = ./.;

  # Build phase
  nativeBuildInputs = [
    c3c
  ];

  buildInputs = [
    pkgs.qbe
  ];

  # TODO: Change compile to build
  buildPhase = ''
    echo "checking libraries linked to c3c"
    ldd ${c3c}/bin/c3c
    echo "building the compiler"
    mkdir -p $out/bin
    c3c compile $src/src/* -o $out/bin/pl-compiler
    echo "checking libraries linked to pl-compiler"
    ldd $out/bin/pl-compiler
    echo "testing the compiler"
    c3c test --print-linking --debug-stats
  '';

  # Check phase
  # doCheck = true;

  # navtiveCheckInputs = [
  #   pkgs.gcc
  # ];

  # checkPhase = ''
  #   echo "testing the compiler"
  #   c3c test
  # '';

  # Install phase
  installPhase = ''
    echo "doing nothing"
  '';

}
