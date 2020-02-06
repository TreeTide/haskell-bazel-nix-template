with (import ./default.nix).pkgs;
buildEnv {
  name = "bazel-cc-toolchain";
  paths = [ stdenv.cc binutils ];
}

