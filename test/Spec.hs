module Main ( main ) where

import           Dhall.Dep
import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = defaultMain $
    testGroup "Dhall.Deps"
        [ unitTest
        , regressionTest
        ]

regressionTest :: TestTree
regressionTest = testCase "test/data/regression.dhall" $ do
    res <- getAllFileDeps "test/data/regression.dhall"
    res @?= [ "test/data/foo.conf" ]

unitTest :: TestTree
unitTest = testCase "test/data/expr2.dhall" $ do
    res <- getAllFileDeps "test/data/expr2.dhall"
    res @?= ["test/data/expr1.dhall", "test/data/expr0.dhall", "test/data/subdir/lib.dhall"]
