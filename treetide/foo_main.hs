{-# Language OverloadedStrings #-}
module Main (main) where

import TreeTide.Foo (foo)

main :: IO ()
main = print (foo "123")

