{-# OPTIONS -fno-warn-orphans #-}
module Compose
    ( testCompose
    )
where

import Control.Applicative
import Data.Functor.Identity
import Data.IORef
import Data.Subsingular
import Test.Tasty
import Test.Tasty.HUnit
import Prelude

import Control.Monad.Ology

testComposeInnerApplicative :: TestTree
testComposeInnerApplicative =
    testCase "Applicative" $ do
        r1 <- newIORef False
        r2 <- newIORef False
        let
            c1 :: ComposeInner Maybe IO ()
            c1 = do
                lift $ writeIORef r1 True
                liftInner Nothing
            c2 :: ComposeInner Maybe IO ()
            c2 = lift $ writeIORef r2 True
        _ <- unComposeInner $ liftA2 (,) c1 c2
        v1 <- readIORef r1
        v2 <- readIORef r2
        assertEqual "v1" True v1
        assertEqual "v2" False v2

testComposeInnerAlternative :: TestTree
testComposeInnerAlternative =
    testCase "Alternative" $ do
        r1 <- newIORef False
        r2 <- newIORef False
        let
            c1 :: ComposeInner Maybe IO ()
            c1 = lift $ writeIORef r1 True
            c2 :: ComposeInner Maybe IO ()
            c2 = lift $ writeIORef r2 True
        _ <- unComposeInner $ c1 <|> c2
        v1 <- readIORef r1
        v2 <- readIORef r2
        assertEqual "v1" True v1
        assertEqual "v2" False v2

testComposeInner :: TestTree
testComposeInner = testGroup "inner" [testComposeInnerApplicative, testComposeInnerAlternative]

-- this is false
instance Subsingular Char where
    subsingle = Just 'A'

type M = WriterT String Identity

type C = ComposeCommute M []

execC :: C a -> String
execC ca = runIdentity $ execWriterT $ unComposeCommute ca

m :: C ()
m = MkComposeCommute $ return [(), ()]

g :: C ()
g = MkComposeCommute $ tell "G" >> return [()]

h :: C ()
h = MkComposeCommute $ tell "H" >> return [()]

testComposeCommute :: TestTree
testComposeCommute = testCase "Commute" $ do
    let
        r1 = execC $ (m >> g) >> h
        r2 = execC $ m >> (g >> h)
    assertBool "" $ r1 /= r2

testCompose :: TestTree
testCompose = testGroup "compose" [testComposeInner, testComposeCommute]
