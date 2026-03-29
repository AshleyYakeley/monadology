module Unlift
    ( testUnlift
    )
where

import Control.Concurrent
import Control.Concurrent.Async
import Test.Tasty
import Test.Tasty.HUnit
import Prelude

import Control.Monad.Ology
import Useful

testUnliftStateT :: TestTree
testUnliftStateT = testCase "unliftStateT"
    $ compareTest "aAbB=2"
    $ \appendStr -> do
        let
            example :: StateT Int IO ()
            example = liftWithUnlift $ \unlift -> do
                a <- async $ do
                    appendStr "a"
                    threadDelay 500
                    appendStr "b"
                    threadDelay 500
                    unlift $ modify succ
                threadDelay 250
                appendStr "A"
                threadDelay 500
                appendStr "B"
                threadDelay 250
                unlift $ modify succ
                wait a
        ((), newState) <- runStateT example 0
        appendStr $ "=" <> show newState
        return ()

testUnlift :: TestTree
testUnlift =
    testGroup
        "unlift"
        [ testUnliftStateT
        ]
