module Control.Monad.Ology.General.Trans.Unlift where

import Control.Monad.Ology.General.Extract
import Control.Monad.Ology.General.Function
import Control.Monad.Ology.General.IO
import Control.Monad.Ology.General.Outer
import Control.Monad.Ology.General.Trans.Constraint
import Control.Monad.Ology.General.Trans.Hoist
import Control.Monad.Ology.General.Trans.Tunnel
import Control.Monad.Ology.Specific.ComposeOuter
import Import

class
    ( MonadTransTunnel t
    , TransConstraint MonadFail t
    , TransConstraint MonadIO t
    , TransConstraint MonadFix t
    , TransConstraint Monad t
    , MonadExtract (Tunnel t)
    ) =>
    MonadTransUnlift t
    where
    -- | Lift with an unlifting function that accounts for the transformer's effects (using MVars where necessary).
    liftWithUnlift ::
        forall m r.
        MonadIO m =>
        (Unlift MonadTunnelIO t -> m r) ->
        t m r

    -- | Return an unlifting function that discards the transformer's effects (such as state change or output).
    getDiscardingUnlift ::
        forall m.
        Monad m =>
        t m (WUnlift MonadTunnelIO t)
    getDiscardingUnlift = tunnel $ \unlift -> pure $ pure $ MkWUnlift $ \tma -> fmap mToValue $ unlift tma

toDiscardingUnlift ::
    forall t.
    MonadTransUnlift t =>
    Unlift MonadUnliftIO t ->
    Unlift MonadUnliftIO t
toDiscardingUnlift run tmr = do
    MkWUnlift du <- run getDiscardingUnlift
    du tmr

wLiftWithUnlift ::
    forall t m.
    (MonadTransUnlift t, MonadTunnelIO m) =>
    WBackraised m (t m)
wLiftWithUnlift = MkWBackraised $ \call -> liftWithUnlift $ \unlift -> call unlift

composeUnliftRaised :: MonadUnliftIO m => Unlift Functor t -> (m --> n) -> (t m --> n)
composeUnliftRaised rt rm tma = rm $ rt tma

composeUnliftRaisedCommute ::
    (MonadTransUnlift t, MonadUnliftIO m, MonadUnliftIO n) => Unlift Functor t -> (m --> n) -> (t m --> n)
composeUnliftRaisedCommute rt rm tma = rt $ hoist rm tma

class (MonadFail m, MonadIO m, MonadFix m, MonadTunnelIO m, MonadExtract (TunnelIO m)) => MonadUnliftIO m where
    -- | Lift with an unlifting function that accounts for the effects over 'IO'.
    liftIOWithUnlift :: IO -/-> m

    -- | Return an unlifting function that discards the effects over 'IO'.
    getDiscardingIOUnlift :: m (WRaised m IO)
    getDiscardingIOUnlift = tunnelIO $ \unlift -> pure $ pure $ MkWRaised $ \mr -> fmap mToValue $ unlift mr

wLiftIOWithUnlift :: MonadUnliftIO m => WBackraised IO m
wLiftIOWithUnlift = MkWBackraised liftIOWithUnlift

instance MonadUnliftIO IO where
    liftIOWithUnlift call = call id

instance (MonadTransUnlift t, MonadUnliftIO m, MonadFail (t m), MonadIO (t m), MonadFix (t m)) => MonadUnliftIO (t m) where
    liftIOWithUnlift call = liftWithUnlift $ \tmama -> liftIOWithUnlift $ \maioa -> call $ maioa . tmama

instance MonadTransUnlift t => TransConstraint MonadUnliftIO t where
    hasTransConstraint =
        withTransConstraintDict @MonadFail $ withTransConstraintDict @MonadIO $ withTransConstraintDict @MonadFix $ Dict

instance forall outer. MonadOuter outer => MonadTransUnlift (ComposeOuter outer) where
    liftWithUnlift call =
        MkComposeOuter $ do
            MkWExtract extract <- getExtract
            return $ call $ extract . unComposeOuter

monoHoist ::
    forall (t :: TransKind) ma mb a b.
    (MonadTransUnlift t, MonadTunnelIO ma, MonadIO mb) =>
    (ma a -> mb b) ->
    (t ma a -> t mb b)
monoHoist f tma = liftWithUnlift $ \unlift -> f $ unlift tma
