let 
  nixpkgsRev = "24c765c744b";

  githubNix = fq: rev: 
    builtins.fetchTarball "https://github.com/${fq}/archive/${rev}.tar.gz";
in
{ pkgs ? import (githubNix "nixos/nixpkgs" nixpkgsRev) {}
, compiler ? "default"
}:
let 
  hpkgs = if compiler == "default" then pkgs.haskellPackages else pkgs.haskell.packages."${compiler}";
in 
hpkgs.developPackage { 
  root = ./.;
  name = "arachne";
  modifier = drv: 
  with pkgs.haskell.lib; 
    addBuildTools drv (with hpkgs; [ cabal-install ghcid ])
  ;
}
