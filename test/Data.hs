module Data
    ( testData
    ) where

import Control.Monad.Ology
import Data.Text
import Prelude
import Test.Tasty
import Test.Tasty.HUnit

lensFst :: Lens' (a, b) a
lensFst afa (a, b) = fmap (\a' -> (a', b)) $ afa a

lensSnd :: Lens' (a, b) b
lensSnd bfb (a, b) = fmap (\b' -> (a, b')) $ bfb b

type WM = WriterT ([Int], Text) IO

wmProd :: Prod WM ([Int], Text)
wmProd = writerProd

intsProd :: Prod WM [Int]
intsProd = lensMapProd lensFst wmProd

textProd :: Prod WM Text
textProd = lensMapProd lensSnd wmProd

testProdTell :: TestTree
testProdTell =
    testCase "tell" $ do
        found <-
            execWriterT $ do
                prodTell intsProd [34]
                prodTell textProd "abc"
                prodTell wmProd ([1, 2], "PQ")
                prodTell intsProd [17]
                prodTell textProd "def"
        assertEqual "" ([34, 1, 2, 17], "abcPQdef") found

testProdListen :: TestTree
testProdListen =
    testCase "listen" $ do
        ((fi, ft), found) <-
            runWriterT $ do
                prodTell wmProd ([0], "XYZ")
                fi <-
                    prodListen_ intsProd $ do
                        prodTell intsProd [34]
                        prodTell textProd "abc"
                        prodTell wmProd ([1, 2], "PQ")
                prodTell intsProd [17]
                ft <-
                    prodListen_ textProd $ do
                        prodTell intsProd [8]
                        prodTell textProd "ijk"
                        prodTell wmProd ([9, 10], "MN")
                prodTell textProd "def"
                return (fi, ft)
        assertEqual "output" ([0, 34, 1, 2, 17, 8, 9, 10], "XYZabcPQijkMNdef") found
        assertEqual "fi" [34, 1, 2] fi
        assertEqual "ft" "ijkMN" ft

testProdCollect :: TestTree
testProdCollect =
    testCase "collect" $ do
        ((fi, ft), found) <-
            runWriterT $ do
                prodTell wmProd ([0], "XYZ")
                fi <-
                    prodCollect_ intsProd $ do
                        prodTell intsProd [34]
                        prodTell textProd "abc"
                        prodTell wmProd ([1, 2], "PQ")
                prodTell intsProd [17]
                ft <-
                    prodCollect_ textProd $ do
                        prodTell intsProd [8]
                        prodTell textProd "ijk"
                        prodTell wmProd ([9, 10], "MN")
                prodTell textProd "def"
                return (fi, ft)
        assertEqual "output" ([0, 17, 8, 9, 10], "XYZabcPQdef") found
        assertEqual "fi" [34, 1, 2] fi
        assertEqual "ft" "ijkMN" ft

testProd :: TestTree
testProd = testGroup "prod" [testProdTell, testProdListen, testProdCollect]

testData :: TestTree
testData = testGroup "data" [testProd]
