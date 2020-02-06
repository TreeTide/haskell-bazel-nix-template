#
# Vanilla WORKSPACE file, usable directly when invoking bazel manually (from
# within a `nix-shell`).
#
# See `release.nix` to see how the part files get inserted to get build working
# from within a `nix-build` command.
#

workspace(name = "treetide")

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load("@bazel_tools//tools/build_defs/repo:git.bzl", "git_repository")

#
# Nix / Haskell
#

# TODO(robinp): move this to the part file, but now the Bazel dep fetch happens
# only based on the non-part files (which is not right).
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

http_archive(
    name = "rules_sh",
    sha256 = "63eb736f7562ed80054e373392abb0d4950d70e22542665079158854bd2cd98f",
    strip_prefix = "rules_sh-39e356843071f93caf2caaf427d46cd10717501b",
    urls = ["https://github.com/tweag/rules_sh/archive/39e356843071f93caf2caaf427d46cd10717501b.tar.gz"],
)

load("@rules_sh//sh:repositories.bzl", "rules_sh_dependencies")

rules_sh_dependencies()

# NOTE: update release.nix if changing version of rules_haskell here.
http_archive(
    name = "rules_haskell",
    sha256 = "758f8190a9dd6e5e6fd7c9fb38a1bb4c5743a6e314d6678761e2cc070d8e465b",
    strip_prefix = "rules_haskell-107ab5ccf0cdf884e19c1b3a37b9b8064c4e4e03",
    urls = ["https://github.com/tweag/rules_haskell/archive/107ab5ccf0cdf884e19c1b3a37b9b8064c4e4e03.tar.gz"],
)

load("@rules_haskell//haskell:repositories.bzl", "rules_haskell_dependencies")

rules_haskell_dependencies()

http_archive(
    name = "io_tweag_rules_nixpkgs",
    sha256 = "f5af641e16fcff5b24f1a9ba5d93cab5ad26500271df59ede344f1a56fc3b17d",
    strip_prefix = "rules_nixpkgs-0.6.0",
    urls = ["https://github.com/tweag/rules_nixpkgs/archive/v0.6.0.tar.gz"],
)

### MARK: CUT START when building with nix

load("@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl", "nixpkgs_local_repository")

nixpkgs_local_repository(
    name = "nixpkgs",
    nix_file = "//nix:nixpkgs.nix",
    nix_file_deps = [
        "//nix:BUILD",
        "//nix:fetch.nix",
        "//nix:versions.json",
    ],
)

load("@rules_haskell//haskell:nixpkgs.bzl", "haskell_register_ghc_nixpkgs")

haskell_register_ghc_nixpkgs(
    attribute_path = None,
    compiler_flags =
        [
            "-threaded",  # Use multicore runtime
            # TODO(robinp): make these passed only for binaries? Now build
            # complains rtsoptsy options have no effect with -shared, which is
            # true and we don't care here.
            "-rtsopts",  # Enable specifying runtime options on command line.

            # Set default RTS options.
            # -maxN<X>: use up to X cores if available.
            # -qn4: only use 4 cores for parallel GC.
            # -A64m: use larger allocation area.
            # -n4m: use allocation chunks, which can be beneficial on multicore.
            # See https://simonmar.github.io/posts/2015-07-28-optimising-garbage-collection-overhead-in-sigma.html.
            # -T: make gc stats available in-program
            # -t: print one-line GC statistics to stderr on exit.
            "-with-rtsopts=-maxN8 -qn4 -A64m -n4m -T -t",

            # Switch on useful extra warnings, and make warnings compilation
            # error.
            "-Wall",
            "-Werror",
            "-Wcompat",
            "-Wincomplete-record-updates",
            "-Wincomplete-uni-patterns",
            "-Wredundant-constraints",
        ],
    nix_file = "//:ghc.nix",
    nix_file_deps = [
        "//:default.nix",
        # TODO(robinp): now below is copy-pasted from nixpkgs repo setup above,
        # but could we rather parametrize the repo on stock pkgs and inject our
        # own here somehow?
        "//nix:fetch.nix",
        "//nix:versions.json",
    ],
    repositories = {"nixpkgs": "@nixpkgs//nix:nixpkgs.nix"},
    version = "8.6.5",
)

load("@rules_haskell//haskell:toolchain.bzl", "rules_haskell_toolchains")

rules_haskell_toolchains(version = "8.6.5")

load(
    "@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl",
    "nixpkgs_cc_configure",
    "nixpkgs_python_configure",
)

nixpkgs_cc_configure(
    repositories = {"nixpkgs": "@nixpkgs//nix:nixpkgs.nix"},
)

nixpkgs_python_configure(
    repositories = {"nixpkgs": "@nixpkgs//nix:nixpkgs.nix"},
)

### MARK: CUT END for building with nix

# Some optional stuff below, not strictly necessary for nix or haskell.

#
# Go & Buildifier. Below copy-pasted from https://github.com/bazelbuild/buildtools/blob/master/buildifier/README.md. NOTE: using newer rules_go due to https://github.com/bazelbuild/rules_go/issues/2157.
#

# buildifier is written in Go and hence needs rules_go to be built.
# See https://github.com/bazelbuild/rules_go for the up to date setup instructions.

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

http_archive(
    name = "io_bazel_rules_go",
    sha256 = "f99a9d76e972e0c8f935b2fe6d0d9d778f67c760c6d2400e23fc2e469016e2bd",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/rules_go/releases/download/v0.21.2/rules_go-v0.21.2.tar.gz",
        "https://github.com/bazelbuild/rules_go/releases/download/v0.21.2/rules_go-v0.21.2.tar.gz",
    ],
)

load("@io_bazel_rules_go//go:deps.bzl", "go_register_toolchains", "go_rules_dependencies")

go_rules_dependencies()

go_register_toolchains()

http_archive(
    name = "bazel_gazelle",
    sha256 = "be9296bfd64882e3c08e3283c58fcb461fa6dd3c171764fcc4cf322f60615a9b",
    urls = [
        "https://storage.googleapis.com/bazel-mirror/github.com/bazelbuild/bazel-gazelle/releases/download/0.18.1/bazel-gazelle-0.18.1.tar.gz",
        "https://github.com/bazelbuild/bazel-gazelle/releases/download/0.18.1/bazel-gazelle-0.18.1.tar.gz",
    ],
)

load("@bazel_gazelle//:deps.bzl", "gazelle_dependencies")

gazelle_dependencies()

http_archive(
    name = "com_google_protobuf",
    strip_prefix = "protobuf-master",
    urls = ["https://github.com/protocolbuffers/protobuf/archive/master.zip"],
)

load("@com_google_protobuf//:protobuf_deps.bzl", "protobuf_deps")

protobuf_deps()

http_archive(
    name = "com_github_bazelbuild_buildtools",
    strip_prefix = "buildtools-master",
    url = "https://github.com/bazelbuild/buildtools/archive/master.zip",
)
