{ pkgs ? fix.nixpkgs
, compiler ? "default"
, fix ? import nix/fix fix
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
