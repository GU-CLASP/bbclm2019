# { nixpkgs ? import <nixpkgs> {} }:
let distro = fetchTarball "https://github.com/NixOS/nixpkgs/archive/4a2340ff6bd0474d9a3e933f28b8568c59019b82.tar.gz";
             # fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-18.09.tar.gz  # CGI broken here (and GF strangely depends on it)
in with import distro {};
let myGHC = haskellPackages.ghcWithPackages
                     (haskellPackages: with haskellPackages; [
                       # libraries
                       parsek
                       # tools
                       cabal-install
                       mtl
                       split
                       logict
                       monadplus
                     ]);

in stdenv.mkDerivation {
  name = "parser-env-0";
  buildInputs = [ haskellPackages.gf myGHC ];
  shellHook = ''
    export LANG=en_US.UTF-8
    export PYTHONIOENCODING=UTF-8
    eval $(egrep ^export ${ghc}/bin/ghc)
  '';
}


