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
newtype LifeState m
    = MkLifeState (Maybe (m (m Any)))

instance Monad m => Semigroup (LifeState m) where
    MkLifeState Nothing <> q = q
    p <> MkLifeState Nothing = p
    MkLifeState (Just p) <> MkLifeState (Just q) = MkLifeState $ Just $ liftA2 (liftA2 (<>)) q p

instance Monad m => Monoid (LifeState m) where
    mempty = MkLifeState Nothing

pattern NoLifeState :: LifeState m
pattern NoLifeState = MkLifeState Nothing

mkLifeState :: forall m. Monad m => m () -> LifeState m
mkLifeState closer = MkLifeState
    $ Just
    $ do
        closer
        return $ return $ Any False

lifeStateModify :: forall m1 m2. Monad m1 => (m1 --> m2) -> LifeState m1 -> LifeState m2
lifeStateModify _ (MkLifeState Nothing) = MkLifeState Nothing
lifeStateModify m (MkLifeState (Just ioioa)) = MkLifeState $ Just $ m $ fmap m ioioa

closeIOAny :: forall m. Monad m => m Any -> m ()
closeIOAny ioa = do
    Any b <- ioa
    if b
        then closeIOAny ioa
        else return ()

closeLifeState' :: forall m. Monad m => LifeState m -> m Any
closeLifeState' (MkLifeState (Just ioioa)) = do
    ioa <- ioioa
    closeIOAny ioa
    return $ Any True
closeLifeState' (MkLifeState Nothing) = return $ Any False

closeLifeState :: forall m. Monad m => LifeState m -> m ()
closeLifeState ls = do
    _ <- closeLifeState' ls
    return ()

execLifeState :: forall m. Monad m => m (LifeState m) -> LifeState m
execLifeState iols =
    MkLifeState
        $ Just
        $ return
        $ do
            ls <- iols
            closeLifeState' ls
