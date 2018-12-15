module Main where

import Test.Tasty

import Preprocessor.Tests

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
            [ preprocTests ]