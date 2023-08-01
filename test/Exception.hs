module Exception
    ( testException
    ) where

import Control.Applicative
import Control.Monad.Ology
import Data.IORef
import Prelude
import Test.Tasty
import Test.Tasty.HUnit
import Useful

testACatch ::
       forall m e. (Eq e, Show e, MonadCatch e m)
    => m --> IO -> e -> TestTree
testACatch runM ex =
    testCase "catchExc" $ do
        r <- runM $ try @m @e $ throw @e @m @() ex
        assertEqual "caught" (FailureResult ex) r

testABracket ::
       forall m. (MonadTunnelIO m, MonadException m)
    => m --> IO -> (forall a. m a) -> TestTree
testABracket runM th =
    testCase "bracket" $ do
        ref <- newIORef False
        _ <- tryExc $ runM $ finally @m th (liftIO $ writeIORef ref True)
        b <- readIORef ref
        assertEqual "finally" True b

testAnException ::
       forall m e. (Eq e, Show e, MonadException m, MonadCatch e m, MonadTunnelIO m)
    => String
    -> m --> IO -> e -> TestTree
testAnException name runM ex = testGroup name [testACatch runM ex, testABracket runM $ throw ex]

runComposeInner :: ComposeInner Maybe IO --> IO
runComposeInner (MkComposeInner ima) = do
    ma <- ima
    case ma of
        Just a -> return a
        Nothing -> fail "Nothing"

runExceptT' :: ExceptT () IO --> IO
runExceptT' eia = do
    ea <- runExceptT eia
    case ea of
        Right a -> return a
        Left _ -> fail "Left"

runResultT' :: ResultT () IO --> IO
runResultT' eia = do
    ea <- runResultT eia
    case ea of
        SuccessResult a -> return a
        FailureResult _ -> fail "Left"

data Info = forall m e. (MonadCatch e m, MonadIO m, Eq e, Show e) =>
                            MkInfo
    { iName :: String
    , iRun :: m --> IO
    , iExc :: Int -> e
    }

newtype TestExc =
    MkTestExc Int
    deriving (Eq, Show)

instance Exception TestExc

ioInfo :: Info
ioInfo = MkInfo {iName = "IO", iRun = \ioa -> ioa, iExc = MkTestExc}

iRunWithT :: WithT IO --> IO
iRunWithT tia = unWithT tia return

transformInfo :: Info
transformInfo = MkInfo {iName = "WithT IO", iRun = iRunWithT, iExc = MkTestExc}

testCatch :: Info -> TestTree
testCatch MkInfo {..} =
    testCase iName $
    compareTest "ABCE" $ \post -> do
        post "A"
        found <-
            iRun $
            try $ do
                liftIO $ post "B"
                reu <-
                    try $ do
                        liftIO $ post "C"
                        () <- throw $ iExc 52
                        liftIO $ post "D"
                liftIO $ assertEqual "exc1" (FailureResult $ iExc 52) reu
                liftIO $ post "E"
                () <- throw $ iExc 74
                liftIO $ post "F"
        assertEqual "exc2" (FailureResult $ iExc 74) found

testCatches :: TestTree
testCatches = testGroup "catch" [testCatch ioInfo, testCatch transformInfo]

testException :: TestTree
testException =
    testGroup
        "Exception"
        [ testAnException "IO" id $ ErrorCall "test"
        , testGroup "ExceptT () IO" $ pure $ testABracket @(ExceptT () IO) runExceptT' $ throwE ()
        , testGroup "ResultT () IO" $ pure $ testABracket @(ResultT () IO) runResultT' $ throwR ()
        , testGroup "ComposeInner Maybe IO" $
          pure $ testABracket @(ComposeInner Maybe IO) runComposeInner $ liftInner Nothing
        , testCatches
        ]
