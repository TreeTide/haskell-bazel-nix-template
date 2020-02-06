# Release config for all the bazel-built stuff.
#
# See some notes about using the binary cache when building locally at
# https://gist.github.com/robinp/f70bd2c5e8f868b770ced96278ad9f98.
#
with {
  inherit (import ./default.nix) pkgs gitignore;
};
with pkgs;
with {
    nix_ghc = (import ./ghc.nix);
    nix_cc_toolchain = (import ./cc_toolchain.nix);
    nix_glibc_locales = pkgs.glibcLocales;

    # NOTE: keep in sync with WORKSPACE
    rules_haskell = pkgs.fetchFromGitHub {
      owner = "tweag";
      repo = "rules_haskell";
      rev = "107ab5ccf0cdf884e19c1b3a37b9b8064c4e4e03";
      sha256 = "092cx9xix6r8735yn4j247kanzmsh1bjjifgk39jq8fidgldlhsj";
    };
};
with {
  # TODO(robinp): add some variants:
  #   - one which also runs the tests
  #   - one which ran the tests
  #   - (one which never ran the tests and omits them? sounds good for quick fixes)
  #   - way to omit test code... generally specify which targets are for testing,
  #     which for release
  #   - specify data packages (if any, like stopwords). Though these can be inline
  #     as well...
  buildBazelTargets = target: buildBazelPackage {
      # TODO(robinp): hm, including the name would be nice, but it changes fetch output
      # hash, which is inconvenient.
      name = "whatever-build"; # -" + builtins.replaceStrings ["/" "." ":"] ["S" "D" "C"] target;

      # We need to filter the sources interesting for Bazel.
      # Reasons are to avoid bloat of the generated source derivation, also
      # to prevent the source derivation to be recalculated if something
      # unrelated to the bazel build changes.
      #
      # It's a bit awkward that Nix needs to package up the sources all the time,
      # especially that we have some "larger" resources like images committed here,
      # that we don't manage with Bazel. We should filter these away, or eventually
      # store in a more suitable way.
      #
      # TODO(robinp): filter non-bazel-used nix files as well.
      #
      src = with
      { omitNonInteresting = src:
          lib.cleanSourceWith
          { name = "clean-unrelated-sources";
            inherit src;
            filter =
              path: t:
                let relPath = lib.removePrefix (toString src.origSrc + "/") (toString path);
                in !(lib.any (s: lib.hasPrefix s relPath) ["some/unrelated/source/tree/we/dont/want"]);
          };
      }; lib.sourceByRegex (omitNonInteresting (gitignore.gitignoreSource ./.))
            [ "WORKSPACE"
              "WORKSPACE.nix.part"
              "BUILD"
              "BUILD.nix.part"
              "treetide.*"
              "build.*"
              ".bazelrc"
              ".bazelignore"
            ];

      bazelFlags = [
        # Might use local (non-sandboxed) strategy for bazel, since nix-build
        # sandboxes enough already.
        # "--spawn_strategy=local"
      ];

      nativeBuildInputs = [ nix_ghc nix_cc_toolchain python3 rules_haskell ];

      bazelTarget = target;

      fetchAttrs = {
          # Fuzz this WHENEVER you touch workspace files - otherwise the change
          # won't be picked up due to Nix's fixed output derivation mechanism.
          #
          # TODO(robinp): the part file substitution should happen before the
          # fetch phase, to take repos in the part files as well into account.
          sha256 = "0dass65w5vf65w54i9nvxpdwlfz1p245m737ybvsmddjpqxvj2zq";

          # Copy-paste of original, except using //... instead of the given
          # target to fix
          # https://github.com/robinp/treetide-workspace/issues/69 where the
          # prefetch hash could vary based on what subset we are building.
          buildPhase = ''
            runHook preBuild
            # Bazel computes the default value of output_user_root before parsing the
            # flag. The computation of the default value involves getting the $USER
            # from the environment. I don't have that variable when building with
            # sandbox enabled. Code here
            # https://github.com/bazelbuild/bazel/blob/9323c57607d37f9c949b60e293b573584906da46/src/main/cpp/startup_options.cc#L123-L124
            #
            # On macOS Bazel will use the system installed Xcode or CLT toolchain instead of the one in the PATH unless we pass BAZEL_USE_CPP_ONLY_TOOLCHAIN
            # We disable multithreading for the fetching phase since it can lead to timeouts with many dependencies/threads:
            # https://github.com/bazelbuild/bazel/issues/6502
            BAZEL_USE_CPP_ONLY_TOOLCHAIN=1 \
            USER=homeless-shelter \
            bazel \
              --output_base="$bazelOut" \
              --output_user_root="$bazelUserRoot" \
              fetch \
              --loading_phase_threads=1 \
              $bazelFlags \
              $bazelFetchFlags \
              //...                    # <---- LOCAL CHANGE
            runHook postBuild
          '';
      };

      buildAttrs = {
          prePatch = ''
            echo PrePatch

            # See comment in preBuild.
            ln -s ${nix_ghc} prebuilt_ghc
            workdir=$(pwd)
            cd prebuilt_ghc  # this goes to nix-store via symlink
            ver=$(${nix_ghc}/bin/ghc --version | awk '{print $NF}')
            python3 ${rules_haskell}/haskell/private/pkgdb_to_bzl.py ghc lib/ghc-$ver/ > $workdir/BUILD.toolchain_libs.part
            cd $workdir

            awk -i inplace -f build/replacer.awk to_replace=WORKSPACE.nix.part WORKSPACE
            awk -i inplace -f build/replacer.awk to_replace=BUILD.toolchain_libs.part BUILD.nix.part
            awk -i inplace -f build/replacer.awk to_replace=BUILD.nix.part BUILD
          '';

          preBuild = ''
            # See https://github.com/NixOS/nixpkgs/pull/54780 and
            # https://github.com/bazelbuild/bazel/issues/6865 for --host_javabase.
            #
            # --noincompatible_use_python_toolchains is due to Bazel 0.27 changes.
            #
            # TODO(robinp): add --disk-cache on Hydra. Needs extra-sandbox-paths?
            #
            # NOTE: Overwriting the original, so we don't try to use the nix
            # platform (due to nix-in-nix will fail with fail_not_supported).
            cat > .bazelrc <<EOF
            build --host_javabase='@local_jdk//:jdk'
            build --noincompatible_use_python_toolchains
            build -c opt
            EOF

            # The funny names are the ones set in the WORKSPACE file, basically
            # pre-persisted versions of what rules_nixpkgs would have fetched.
            # But rules_nixpkgs couldn't run inside a nix build, see
            # https://github.com/tweag/rules_nixpkgs/issues/73.
            # Thus the magic.
            #
            ln -s ${nix_cc_toolchain} prebuilt_toolchain
            ln -s ${nix_glibc_locales} prebuilt_glibc_locales
          '';

          installPhase = ''
            mkdir -p $out/bin
            mkdir -p $out/test

            for f in $(bazel \
              --output_base="$bazelOut" \
              --output_user_root="$bazelUserRoot" \
              query 'kind(".*_binary", ${target})' | sed 's://::' | sed 's;:;/;')
            do
              mkdir -p $out/bin/$(dirname $f)
              cp bazel-bin/$f $out/bin/$f
            done

            for f in $(bazel \
              --output_base="$bazelOut" \
              --output_user_root="$bazelUserRoot" \
              query 'kind(".*_test", ${target})' | sed 's://::' | sed 's;:;/;')
            do
              mkdir -p $out/test/$(dirname $f)
              cp bazel-bin/$f $out/test/$f
            done
          '';
      };
  };

};
# Hydra expects a dictionary with the derivations as values.
rec {

  # TODO(robinp): separate to helper, keep this file releasable units only.
  inherit buildBazelTargets;

  all = buildBazelTargets "//treetide/...";

  tests = stdenv.mkDerivation {
      # We add a separate test derivation. This way the built binaries persist
      # regardless of test results, also it is easy to rerun the tests later,
      # or run them on-demand only.
      name = "whatever-tests";

      doCheck = true;

      checkInputs = [ all ];

      unpackPhase = ":";

      checkPhase = ''
          for f in $(find $nativeBuildInputs/test -type f)
          do
            echo ==== Testing $f
            p=$(echo $f | sed 's:^/::')
            mkdir -p test_result/$(dirname $p)
            LC_ALL=C.UTF-8 $f | tee test_result/$p.txt
          done
      '';

      installPhase = ''
        mkdir $out
        cp -r test_result $out/
      '';
  };
}

