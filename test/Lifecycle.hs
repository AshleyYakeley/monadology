module Lifecycle
    ( testLifecycle
    ) where

import Control.Monad.Ology
import Prelude
import Test.Tasty
import Test.Tasty.HUnit
import Useful

testLifecycleRun :: TestTree
testLifecycleRun =
    testCase "run" $
    compareTest "ACDB" $ \appendStr -> do
        let
            lc :: Lifecycle ()
            lc = do
                liftIO $ appendStr "A"
                lifecycleOnClose $ appendStr "B"
                liftIO $ appendStr "C"
                lifecycleOnClose $ appendStr "D"
        runLifecycle lc

testLifecycleWith :: TestTree
testLifecycleWith =
    testCase "with" $
    compareTest "ABECFD" $ \appendStr -> do
        let
            lc :: Lifecycle ()
            lc = do
                liftIO $ appendStr "A"
                s <-
                    lifecycleWith $ \call -> do
                        appendStr "B"
                        v <- call "C"
                        appendStr "D"
                        return v
                liftIO $ appendStr "E"
                liftIO $ appendStr s
                liftIO $ appendStr "F"
        runLifecycle lc

testLifecycle :: TestTree
testLifecycle = testGroup "lifecycle" [testLifecycleRun, testLifecycleWith]
