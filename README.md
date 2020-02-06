# Hermetic Bazel + Haskell + Nix setup

## Context

Minimal (?) repo that contains Haskell build set up with `rules_bazel`, with a
nixpkgs-sourced GHC, and is able to build the results using Bazel from within
`nix-build` itself with the same toolchain.

Latter is contrieved, since nix-build can't be run inside nix-build, so
`rules_haskell` can't source its dependent toolchain from nix when bazel is
being run inside a nix-build. See `WORKSPACE.nix.part` file and `release.nix`
for details.

Running inside nix-build is needed if you want to use Hydra CI, for example, or
depend on Bazel-built binaries from other nix files (maybe for a nixops
deployment).

## Code tour

The contents of the `nix` dir implement niv-style nix version pinning, so the
deps are hermetic and independent of the nix-channel version of the user.

The `release.nix` file is the main nix-build target, resulting in binaries,
tests and test results.

The `treetide` dir is the monorepo top.

- `treetide/thirdparty/haskell/BUILD` along with `ghc.nix` define the libraries
  we import from Nix.

- `treetide/haskell/build_defs` contains some Bazel shorthands for declaring common deps.

- `treetide/haskell/extended` contains some extended upstream libs, using
  the extended pattern (thus the name extended... ok you likely get it).

- Other dirs in the monorepo are laid out according to project structure. Now
  we just have the `foo_*` family of a toy library, test and binary, along with
  their `BUILD` rules.

- `.bazelignore` can be used to exclude eventual `node_modules` inside the
  monorepo, so Bazel doesn't get confused by BUILD files in there.

- `.bazelrc` sets up some Bazel compatibility flags, and instructs
  `rules_haskell` to pull toolchains from Nix. Note: latter instruction is
  dropped when building using `nix-build`, see `release.nix`.

## Building

Having nix installed is a prerequisite.

### Building from command StreamedAsne interactively

To bu(ild locally while developing) >> pure XmlStream:

  1) Get a nix shell using `nix-shell`.

  2) Then `bazel build -c opt //treetide/...`.

  3) Test using `LC_ALL=C.UTF-8 ./bazel-bin/treetide/foo_test`.

Note: can also execute single-target REPL, or set up REPL rules for
multi-target repl.

Note: can test directly using `bazel test -c opt //treetide/...`, but that
needs a custom main wrapper that forces GHC into UTF-8 mode. But that's a
different story... Ping if interested.

### Building using nix (suitable for Hydra etc)

Execute `nix-build release.nix`. This will build all targets and run the tests.
The symlinked `results` output has a directory for the binaries (executables)
and tests, respectively. The `results-2` dir contains the test logs (if
successful).

## Tooling

Note: there are some tools available in the `nix-shell`, see
`README.haskell.md` for details.

## How can you help?

Cleanup PRs, additional tooling welcome!

