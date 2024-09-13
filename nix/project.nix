{ repoRoot, inputs, pkgs, lib, system }:

let
  sha256map = {
    "https://github.com/j-mueller/sc-tools"."e1f4359b6fb6a2b939bac0840e229193dd4f6b54" = "sha256-+dVtHGOQHvAjwOZZ0ZyHsQLF2roREUVIoMU31AD3YZs=";
    "https://github.com/Plutonomicon/plutarch-plutus"."e50661e24670974b398be19426617bc6389fdac6" = "f7dd88e3664a72dacc91f488b122955a874a6615d88961c9e87f2c93bf833f86";

  };


  modules = [
  ];


  cabalProject = pkgs.haskell-nix.cabalProject' {
    inherit modules sha256map;
    src = ../.;
    name = "proposal-loans";
    compiler-nix-name = "ghc966";
    inputMap = { "https://chap.intersectmbo.org/" = inputs.CHaP; };
    shell.withHoogle = false;
  };


  project = lib.iogx.mkHaskellProject {
    inherit cabalProject;
    shellArgs = repoRoot.nix.shell;
  };

in

project
