# an ad-hoc nix setup, pending proper nixification of the build

# roughly release-22.05
with import (builtins.fetchTarball https://github.com/NixOS/nixpkgs/archive/d3551b986a714071360b75feffa0ce117417f09c.tar.gz) { };
with haskell.lib;
stdenv.mkDerivation rec {
  name = "env";
  buildInputs = [
    (haskell.packages.ghc8107.ghc.withPackages(p: with p; [
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
      ]))

    ((haskell.packages.ghcjs
      .override({ overrides = self: super: {
        # depends on doctest
        foldl = dontCheck super.foldl;
        # fails
        unbound-generics = dontCheck super.unbound-generics;
        unliftio = dontCheck super.unliftio;
        conduit = dontCheck super.conduit;
        # slow
        mono-traversable = dontCheck super.mono-traversable;
        libyaml = dontCheck super.libyaml;
        text-short = dontCheck super.text-short;


        ghcjs-base = super.ghcjs-base.overrideAttrs(d: {
          src = pkgs.fetchFromGitHub {
            owner = "ghcjs";
            repo = "ghcjs-base";
            rev = "fbaae59b05b020e91783df122249095e168df53f";
            sha256 = "sha256-x6eCAK1Hne0QkV3Loi9YpxbleNHU593E4AO8cbk2vUc=";
          };
        });

      };}))
      .ghc.withPackages(p: with p; [
       aeson
       parsec
       tagged
       unbound-generics
       either
       ghcjs-base
      ]))

    nodePackages.jshint
  ];
}

