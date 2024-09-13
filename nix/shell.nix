{ repoRoot, inputs, pkgs, lib, system }:

cabalProject:

let
  cardano-cli = inputs.cardano-cli.legacyPackages.cardano-cli;
  cardano-node = inputs.cardano-node.packages.cardano-node;
  cardano-submit-api = inputs.cardano-node.packages.cardano-submit-api;
  lustre-v4 = repoRoot.nix.lustre-v4;
  kind2 = repoRoot.nix.kind2;
  tex = (pkgs.texlive.combine {
    inherit (pkgs.texlive) scheme-basic standalone xcolor metafont;
  });
in
{
  name = "stablecoin-plutus";
  packages = [
    pkgs.ghcid
  ];

  env = {
    CARDANO_CLI = "${cardano-cli}/bin/cardano-cli";
    CARDANO_NODE = "${cardano-node}/bin/cardano-node";
    CARDANO_SUBMIT_API = "${cardano-submit-api}/bin/cardano-submit-api";
    LUSTRE_INSTALL = "${lustre-v4}";
  };


  preCommit = {
    # NOTE: when this attribute set changes, `.pre-commit-config.yaml` (which is a sym link to the nix store) changes.
    #       To maintain a the same hooks for both nix and non-nix environment you should update the `.pre-commit-config.yaml.nonix`
    #       (`cp .pre-commit-config.yaml .pre-commit-config.yaml.nonix`).
    #       This step is necessary because `.pre-commit-config.yaml` is ignored by git.
    cabal-fmt.enable = true;
    stylish-haskell.enable = true;
    nixpkgs-fmt.enable = true;
  };
}
