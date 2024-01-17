{ pkgs ? import <nixpkgs> {} }:
pkgs.stdenv.mkDerivation {
  pname = "c3c";
  version = "0.5.3";
  system = "x86_64-linux";

  src = pkgs.fetchurl {
    url = "https://github.com/c3lang/c3c/releases/download/latest/c3-linux.tar.gz";
    hash = "sha256-qYX9MAE1Xbih46Qw3XpWlkpTYfj6Z74wzKUTSaT8F00=";
  };

  nativeBuildInputs = with pkgs; [
    libxml2
    libffi
    zlib
    zstd
    ncurses
    stdenv.cc.cc.lib
    autoPatchelfHook
  ];

  buildInputs = with pkgs; [
    lld
    llvm
  ];

  installPhase = ''
    echo "installing the c3 compiler v0.5.3"
    mkdir -p $out/bin
    tar -xzf $src -C $out
    mv $out/linux/c3c $out/bin/c3c
    mv $out/linux/lib/ $out/bin/lib/
    rm -r $out/linux/
    echo "auto-patching the c3 compiler elf"
    #autoPatchelf $out/bin/c3c
  '';
}
