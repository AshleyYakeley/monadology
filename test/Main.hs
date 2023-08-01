module Main
    ( main
    ) where

import Compose
import Coroutine
import Data
import Exception
import Lifecycle
import Prelude
import Test.Tasty

tests :: TestTree
tests = testGroup "monadology" [testCoroutine, testLifecycle, testComposeInner, testException, testData]

main :: IO ()
main = defaultMain tests
