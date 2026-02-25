# an ad-hoc nix setup, pending proper nixification of the build

# nixpkgs master January 2023
with import (builtins.fetchTarball https://github.com/NixOS/nixpkgs/archive/6ff46bd3309820a57817ff82ef0fc05526b5266f.tar.gz) { };
with haskell.lib;
mkShellNoCC rec {
  name = "incredible-proof-machine-env";
  buildInputs = [
    cabal-install

    (haskell.packages.ghcHEAD.ghc.withPackages(p: with p; [
       aeson
       parsec
       tagged
       unbound-generics
       either
       yaml
       tasty
       tasty-expected-failure
       tasty-hunit
       tasty-quickcheck
       criterion
       void
       extra
      ]))

    (( pkgsCross.ghcjs.haskell.packages.ghcHEAD
      .override({ overrides = self: super: {
      #   # depends on doctest
      #   foldl = dontCheck super.foldl;
      #   # fails
      #   unbound-generics = dontCheck super.unbound-generics;
      #   unliftio = dontCheck super.unliftio;
      #   conduit = dontCheck super.conduit;
      #   # slow
      #   mono-traversable = dontCheck super.mono-traversable;
      #   libyaml = dontCheck super.libyaml;
      #   text-short = dontCheck super.text-short;


      #   # Can be dropped when nixpkgs includes
      #   # https://github.com/NixOS/nixpkgs/pull/177489/files
      #   # Keep synced with kaleidogen for better cache usage!
      #   ghcjs-base = super.ghcjs-base.overrideAttrs(d: {
      #     src = pkgs.fetchFromGitHub {
      #       owner = "ghcjs";
      #       repo = "ghcjs-base";
      #       rev = "fbaae59b05b020e91783df122249095e168df53f";
      #       sha256 = "sha256-x6eCAK1Hne0QkV3Loi9YpxbleNHU593E4AO8cbk2vUc=";
      #     };
      #   });

       # new base dependency
       indexed-traversable = doJailbreak super.indexed-traversable;
       OneTuple = doJailbreak super.OneTuple;
       hashable = doJailbreak super.hashable;
     };})
    ).ghc.withPackages(p: with p; [
       aeson
       parsec
       tagged
       unbound-generics
       either
       ghcjs-base
       void
      ]))

    nodePackages.jshint
  ];
}

