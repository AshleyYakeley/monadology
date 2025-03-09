module Lifecycle
    ( testLifecycle
    )
where

import Test.Tasty
import Test.Tasty.HUnit
import Prelude

import Control.Monad.Ology
import Useful

testLifecycleRun :: TestTree
testLifecycleRun =
    testCase "run"
        $ compareTest "ACDB"
        $ \appendStr -> do
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
    testCase "with"
        $ compareTest "ABECFD"
        $ \appendStr -> do
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

testLifeCycleGetState :: TestTree
testLifeCycleGetState =
    testCase "with"
        $ compareTest "ACEFDB"
        $ \appendStr -> do
            let
                lc1 :: Lifecycle ()
                lc1 = do
                    liftIO $ appendStr "A"
                    lifecycleOnClose $ appendStr "B"
                    liftIO $ appendStr "C"
                    lifecycleOnClose $ appendStr "D"
            ((), ls) <- getLifeState lc1
            let
                lc2 :: Lifecycle ()
                lc2 = do
                    liftIO $ appendStr "E"
                    addLifeState ls
                    liftIO $ appendStr "F"
            runLifecycle lc2

testLifecycle :: TestTree
testLifecycle = testGroup "lifecycle" [testLifecycleRun, testLifecycleWith, testLifeCycleGetState]
