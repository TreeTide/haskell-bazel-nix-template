{ pkgs ? (import ./default.nix).pkgs
}:

with pkgs;

with rec {
  fetch = import ./nix/fetch.nix;
};
stdenv.mkDerivation {
  name = "dev-shell";
  buildInputs = [
    bazel
    python3 # for rules_haskell scripts in bazel

    (import ./ghc.nix)
  ] ++ (import ./tooling.nix);

  shellHook = ''
    # Some code-formatting helpers.
    brittanyPreviousCommit () {
      git log --stat HEAD~.. --name-only --format=format:"" | grep '\.hs$' | xargs -n1 brittany --write-mode inplace
      git diff
    }
    brittanyChanged () {
      git status --porcelain | grep '^\s*[AM]' | awk '{print $2}' | grep '\.hs$' | xargs -n1 brittany --write-mode inplace
    }
    hlintApply () {
      hlint --refactor --refactor-options="-i -s" $@
    }
  '';
}
