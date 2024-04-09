{ pkgs ? import <nixpkgs> {} }:
pkgs.stdenv.mkDerivation {
  pname = "pl-compiler";
  version = "0.0.0";
  system = "x86_64-linux";

  src = fetchGit {
    url = ./.;
    shallow = true;
  };

  # Build phase
  nativeBuildInputs = [
    pkgs.zig
  ];

  buildInputs = [
    pkgs.qbe
  ];

  # TODO: Change compile to build
  buildPhase = ''
    echo "building the compiler"
    export XDG_CACHE_HOME=$TMP/.cache
    zig build --prefix-exe-dir $out
  '';

  # Check phase
  doCheck = true;

  nativeCheckInputs = [
    pkgs.gcc
  ];

  checkPhase = ''
    echo "testing the compiler"
    zig build test
  '';

  # Install phase
  installPhase = ''
    echo "doing nothing"
  '';

  # Nix shell
  shellHook = ''
  '';

}
