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
import Unlift

tests :: TestTree
tests = testGroup "monadology" [testCoroutine, testLifecycle, testCompose, testException, testData, testUnlift]

main :: IO ()
main = defaultMain tests
