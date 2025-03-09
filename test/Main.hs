module Main
    ( main
    )
where

import Test.Tasty
import Prelude

import Compose
import Coroutine
import Data
import Exception
import Lifecycle

tests :: TestTree
tests = testGroup "monadology" [testCoroutine, testLifecycle, testComposeInner, testException, testData]

main :: IO ()
main = defaultMain tests
