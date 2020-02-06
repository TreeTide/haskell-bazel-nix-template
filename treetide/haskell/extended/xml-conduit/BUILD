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
    srcs = ["XmlConduit.hs"],
    deps = [
        prebuilt("base"),
        prebuilt("conduit"),
        prebuilt("protolude"),
        prebuilt("text"),
        prebuilt("xml-conduit"),
        prebuilt("xml-types"),
    ],
)
