package(default_visibility = ["//visibility:public"])

load(
    "@rules_haskell//haskell:defs.bzl",
    "haskell_library",
    "haskell_test",
    "haskell_binary",
)
load(
    "//treetide/haskell/build_defs:build.bzl",
    "prebuilt",
    "extended",
)

haskell_library(
    name = "foo",
    srcs = ["foo.hs"],
    deps = [
        extended("attoparsec"),
        prebuilt("base"),
        prebuilt("text"),
    ],
)

haskell_test(
    name = "foo_test",
    srcs = ["foo_test.hs"],
    deps = [
        ":foo",
        prebuilt("base"),
        prebuilt("hedgehog"),
        prebuilt("tasty"),
        prebuilt("tasty-hedgehog"),
        prebuilt("text"),
    ],
)

haskell_binary(
    name = "foo_main",
    srcs = ["foo_main.hs"],
    deps = [
        ":foo",
        prebuilt("base"),
    ],
)

