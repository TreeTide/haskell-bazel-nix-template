package(default_visibility = ["//visibility:public"])

load(
    "@rules_haskell//haskell:defs.bzl",
    "haskell_library",
)
load(
    "//treetide/haskell/build_defs:build.bzl",
    "prebuilt",
)

haskell_library(
    name = "extended",
    srcs = ["text.hs"],
    deps = [
        prebuilt("attoparsec"),
        prebuilt("base"),
        prebuilt("text"),
        prebuilt("vector"),
    ],
)
