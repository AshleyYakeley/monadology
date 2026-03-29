module Control.Monad.Ology.Specific.LifecycleT.LifeState
    ( LifeState
    , pattern NoLifeState
    , mkLifeState
    , lifeStateModify
    , closeLifeState
    , execLifeState
    )
where

import Control.Monad.Ology.General
import Import

-- | This represents all the actions that need to be done when closing the lifecycle.
newtype LifeState
    = MkLifeState (Maybe (IO (IO Any)))

instance Semigroup LifeState where
    MkLifeState Nothing <> q = q
    p <> MkLifeState Nothing = p
    MkLifeState (Just p) <> MkLifeState (Just q) = MkLifeState $ Just $ q <> p

instance Monoid LifeState where
    mempty = MkLifeState Nothing

pattern NoLifeState :: LifeState
pattern NoLifeState = MkLifeState Nothing

mkLifeState :: IO () -> LifeState
mkLifeState closer = MkLifeState
    $ Just
    $ do
        closer
        return $ return $ Any False

lifeStateModify :: (IO --> IO) -> LifeState -> LifeState
lifeStateModify _ (MkLifeState Nothing) = MkLifeState Nothing
lifeStateModify m (MkLifeState (Just ioioa)) = MkLifeState $ Just $ m $ fmap m ioioa

closeIOAny :: IO Any -> IO ()
closeIOAny ioa = do
    Any b <- ioa
    if b
        then closeIOAny ioa
        else return ()

closeLifeState' :: LifeState -> IO Any
closeLifeState' (MkLifeState (Just ioioa)) = do
    ioa <- ioioa
    closeIOAny ioa
    return $ Any True
closeLifeState' (MkLifeState Nothing) = return $ Any False

closeLifeState :: LifeState -> IO ()
closeLifeState ls = do
    _ <- closeLifeState' ls
    return ()

execLifeState :: IO LifeState -> LifeState
execLifeState iols =
    MkLifeState
        $ Just
        $ return
        $ do
            ls <- iols
            closeLifeState' ls
