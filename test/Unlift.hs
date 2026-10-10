module Unlift
    ( testUnlift
    )
where

import Control.Concurrent
import Control.Concurrent.Async
import Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad.Ology
import Useful

testUnliftStateT :: TestTree
testUnliftStateT = testCase "unliftStateT" $
    compareTest "aAbB=2" $
        \appendStr -> do
            let
                example :: StateT Int IO ()
                example = liftWithUnlift $ \unlift -> do
                    lowerDone <- newEmptyMVar
                    upperDone <- newEmptyMVar
                    -- Synchronise the output explicitly; delays do not guarantee thread ordering.
                    withAsync
                        ( do
                            appendStr "a"
                            putMVar lowerDone ()
                            takeMVar upperDone
                            appendStr "b"
                            putMVar lowerDone ()
                            takeMVar upperDone
                            unlift $ modify succ
                        )
                        $ \a -> do
                            takeMVar lowerDone
                            appendStr "A"
                            putMVar upperDone ()
                            takeMVar lowerDone
                            appendStr "B"
                            putMVar upperDone ()
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
