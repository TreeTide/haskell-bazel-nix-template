# Pinned nixpkgs, see
# https://nmattia.com/posts/2019-01-15-easy-peasy-nix-versions.html.

with { fetch = import ./nix/fetch.nix; };
with {
  pkgs = import fetch.nixpkgs {
    config = {
      # allowBroken = true;
    };
  };
  pkgsUnfree = import fetch.nixpkgs {
    config = {
      allowUnfree = true;
    };
  };
};
rec {
  inherit pkgs;

  gitignore = import fetch.gitignore { lib = pkgs.lib; };
}
