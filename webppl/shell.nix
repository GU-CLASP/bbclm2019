{pkgs ? import <nixpkgs> {
    inherit system;
  }, system ? builtins.currentSystem, nodejs ? pkgs."nodejs-4_x"}:

let
  nodeEnv = import ./node-env.nix {
    inherit (pkgs) stdenv python2 utillinux runCommand writeTextFile;
    inherit nodejs;
  };
  nodePkgs = import ./node-packages.nix {
    inherit (pkgs) fetchurl fetchgit;
    inherit nodeEnv;
  };
  canvas = nodePkgs.canvas.override (oldAttrs: {
    buildInputs = oldAttrs.buildInputs ++ [ nodePkgs.node-gyp nodePkgs.gyp nodePkgs.node-gyp-build pkgs.pkgconfig pkgs.cairo pkgs.libjpeg pkgs.giflib pkgs.freetype pkgs.pango ];
  });
  # webppl-viz = nodePkgs."webppl-viz-git://github.com/probmods/webppl-viz.git".override (oldAttrs: {
  #   buildInputs = oldAttrs.buildInputs ++ [ nodePkgs.node-gyp nodePkgs.gyp nodePkgs.node-gyp-build pkgs.pkgconfig pkgs.cairo pkgs.libjpeg pkgs.giflib  ];
  # });
in pkgs.stdenv.mkDerivation {
  name = "my-env-0";
  buildInputs = [ nodePkgs.webppl nodePkgs.npm nodejs nodePkgs.node-gyp 
  pkgs.fontconfig pkgs.cairo pkgs.freetype pkgs.pkgconfig pkgs.pango pkgs.giflib pkgs.freetype pkgs.libjpeg
  ];
}

