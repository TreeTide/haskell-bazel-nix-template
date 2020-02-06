let
  # Note: below would also work if called via Bazel/rules_nix. But to avoid
  # chance of instantiating manually with the unpinned nixpkgs, we specify
  # explicitly.
  #
  #     pkgs = import <nixpkgs> {};
  pkgs = (import ./default.nix).pkgs;

  # See https://github.com/bgamari/nixpkgs/commit/cdf1a522cacfb209e340af9d51c2ac4dd84e7124
  markUnbroken = drv: pkgs.haskell.lib.overrideCabal drv (drv: { broken = false; });

  haskellPackages = pkgs.haskellPackages.override (old: {
     overrides =
      with pkgs;
      lib.composeExtensions (old.overrides or (_: _: {})) (self: super: {
        # To just mark unbroken.
        #whatever = markUnbroken (haskell.lib.doJailbreak super.whatever);

        # To disable tests, with marking unbroken as well.
        #foobar = markUnbroken (haskell.lib.dontCheck super.foobar);
     });
  });
in haskellPackages.ghcWithPackages (p: with p; [
  # Shipped with GHC
  base-unicode-symbols
  clock

  # Others
  attoparsec
  conduit
  hedgehog
  optparse-applicative
  protolude
  tasty
  tasty-hedgehog
  vector
  xml-conduit
  xml-types
  # tons of other packages here
])
