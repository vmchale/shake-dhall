module Main ( main ) where

import           Dhall.Dep
import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = defaultMain $
    testGroup "Dhall.Deps"
        [ unitTest ]

unitTest :: TestTree
unitTest = testCase "test/data/expr2.dhall" $ do
    res <- getAllFileDeps "test/data/expr2.dhall"
    res @?= ["test/data/expr1.dhall", "test/data/expr0.dhall", "test/data/subdir/lib.dhall"]
