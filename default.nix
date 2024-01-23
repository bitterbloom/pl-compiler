{ pkgs ? import <nixpkgs> {} }:
pkgs.stdenv.mkDerivation {
  pname = "pl-compiler";
  version = "0.0.0";
  system = "x86_64-linux";

  src = ./.;

  # Build phase
  nativeBuildInputs = [
    (import ./c3c.nix { inherit pkgs; })
  ];

  buildInputs = [
    pkgs.qbe
  ];

  # TODO: Change compile to build
  buildPhase = ''
    echo "building the compiler"
    mkdir -p $out/bin
    c3c compile $src/src/* -o $out/bin/pl-compiler
  '';

  # Check phase
  doCheck = true;

  nativeCheckInputs = with pkgs; [
    gcc
  ];

  checkPhase = ''
    echo "testing the compiler"
    c3c test
  '';

  # Install phase
  dontInstall = true;

  installPhase = ''
  '';

}
