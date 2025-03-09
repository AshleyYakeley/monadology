module Control.Monad.Ology.Specific.ComposeT
    ( ComposeT (..)
    , composeTUnlift
    , composeTWUnlift
    , liftOuterComposeT
    , liftInnerComposeT
    , liftOuterComposeTWithUnlift
    , liftInnerComposeTWithUnlift
    )
where

import Control.Monad.Ology.General
import Control.Monad.Ology.Specific.ComposeInner
import Import

-- | Compose two monad transformers.
type ComposeT :: TransKind -> TransKind -> TransKind
newtype ComposeT (outerT :: TransKind) (innerT :: TransKind) (m :: Type -> Type) (a :: Type) = MkComposeT
    { unComposeT :: outerT (innerT m) a
    }
    deriving newtype (Functor, Applicative, Alternative, Monad, MonadFail, MonadIO, MonadFix, MonadPlus)

liftOuterComposeT ::
    forall outerT innerT m a.
    (MonadTransTunnel outerT, MonadTrans innerT, TransConstraint Monad innerT, Monad m) =>
    outerT m a ->
    ComposeT outerT innerT m a
liftOuterComposeT oma =
    case hasTransConstraint @Monad @innerT @m of
        Dict -> MkComposeT $ hoist lift oma

liftInnerComposeT ::
    forall outerT innerT m a.
    (MonadTrans outerT, TransConstraint Monad innerT, Monad m) =>
    innerT m a ->
    ComposeT outerT innerT m a
liftInnerComposeT ima =
    case hasTransConstraint @Monad @innerT @m of
        Dict -> MkComposeT $ lift ima

liftOuterComposeTWithUnlift ::
    forall outerT innerT m r.
    (MonadTransTunnel outerT, MonadTransUnlift innerT, MonadTunnelIO m) =>
    ((forall a. ComposeT outerT innerT m a -> outerT m a) -> outerT m r) ->
    ComposeT outerT innerT m r
liftOuterComposeTWithUnlift call =
    case hasTransConstraint @MonadIO @innerT @m of
        Dict ->
            MkComposeT
                $ tunnel
                $ \tun -> liftWithUnlift $ \unlift -> tun $ call $ \(MkComposeT ttma) -> hoist unlift ttma

liftInnerComposeTWithUnlift ::
    forall outerT innerT m r.
    (MonadTransUnlift outerT, MonadTransUnlift innerT, MonadTunnelIO m) =>
    ((forall a. ComposeT outerT innerT m a -> innerT m a) -> innerT m r) ->
    ComposeT outerT innerT m r
liftInnerComposeTWithUnlift call =
    case hasTransConstraint @MonadTunnelIO @innerT @m of
        Dict -> MkComposeT $ liftWithUnlift $ \unlift -> call $ \(MkComposeT ttma) -> unlift ttma

composeTUnlift ::
    forall c outerT innerT.
    TransConstraint c innerT =>
    Unlift c outerT ->
    Unlift c innerT ->
    Unlift c (ComposeT outerT innerT)
composeTUnlift ua ub (MkComposeT tatbma) = ub $ withTransConstraintTM @c $ ua tatbma

composeTWUnlift ::
    forall c outerT innerT.
    TransConstraint c innerT =>
    WUnlift c outerT ->
    WUnlift c innerT ->
    WUnlift c (ComposeT outerT innerT)
composeTWUnlift (MkWUnlift ua) (MkWUnlift ub) = MkWUnlift $ composeTUnlift @c ua ub

instance (MonadTrans outerT, MonadTrans innerT, TransConstraint Monad innerT) => MonadTrans (ComposeT outerT innerT) where
    lift (ma :: m a) =
        case hasTransConstraint @Monad @innerT @m of
            Dict -> MkComposeT $ lift $ lift ma

instance
    (TransConstraint Functor outerT, TransConstraint Functor innerT) =>
    TransConstraint Functor (ComposeT outerT innerT)
    where
    hasTransConstraint ::
        forall m.
        Functor m =>
        Dict (Functor (ComposeT outerT innerT m))
    hasTransConstraint =
        case hasTransConstraint @Functor @innerT @m of
            Dict ->
                case hasTransConstraint @Functor @outerT @(innerT m) of
                    Dict -> Dict

instance
    (TransConstraint Applicative outerT, TransConstraint Applicative innerT) =>
    TransConstraint Applicative (ComposeT outerT innerT)
    where
    hasTransConstraint ::
        forall m.
        Applicative m =>
        Dict (Applicative (ComposeT outerT innerT m))
    hasTransConstraint =
        case hasTransConstraint @Applicative @innerT @m of
            Dict ->
                case hasTransConstraint @Applicative @outerT @(innerT m) of
                    Dict -> Dict

instance (TransConstraint Monad outerT, TransConstraint Monad innerT) => TransConstraint Monad (ComposeT outerT innerT) where
    hasTransConstraint ::
        forall m.
        Monad m =>
        Dict (Monad (ComposeT outerT innerT m))
    hasTransConstraint =
        case hasTransConstraint @Monad @innerT @m of
            Dict ->
                case hasTransConstraint @Monad @outerT @(innerT m) of
                    Dict -> Dict

instance
    (TransConstraint MonadIO outerT, TransConstraint MonadIO innerT) =>
    TransConstraint MonadIO (ComposeT outerT innerT)
    where
    hasTransConstraint ::
        forall m.
        MonadIO m =>
        Dict (MonadIO (ComposeT outerT innerT m))
    hasTransConstraint =
        case hasTransConstraint @MonadIO @innerT @m of
            Dict ->
                case hasTransConstraint @MonadIO @outerT @(innerT m) of
                    Dict -> Dict

instance
    (TransConstraint MonadFail outerT, TransConstraint MonadFail innerT) =>
    TransConstraint MonadFail (ComposeT outerT innerT)
    where
    hasTransConstraint ::
        forall m.
        MonadFail m =>
        Dict (MonadFail (ComposeT outerT innerT m))
    hasTransConstraint =
        case hasTransConstraint @MonadFail @innerT @m of
            Dict ->
                case hasTransConstraint @MonadFail @outerT @(innerT m) of
                    Dict -> Dict

instance
    (TransConstraint MonadFix outerT, TransConstraint MonadFix innerT) =>
    TransConstraint MonadFix (ComposeT outerT innerT)
    where
    hasTransConstraint ::
        forall m.
        MonadFix m =>
        Dict (MonadFix (ComposeT outerT innerT m))
    hasTransConstraint =
        case hasTransConstraint @MonadFix @innerT @m of
            Dict ->
                case hasTransConstraint @MonadFix @outerT @(innerT m) of
                    Dict -> Dict

instance
    (TransConstraint MonadPlus outerT, TransConstraint MonadPlus innerT) =>
    TransConstraint MonadPlus (ComposeT outerT innerT)
    where
    hasTransConstraint ::
        forall m.
        MonadPlus m =>
        Dict (MonadPlus (ComposeT outerT innerT m))
    hasTransConstraint =
        case hasTransConstraint @MonadPlus @innerT @m of
            Dict ->
                case hasTransConstraint @MonadPlus @outerT @(innerT m) of
                    Dict -> Dict

instance (MonadTransHoist outerT, MonadTransHoist innerT) => MonadTransHoist (ComposeT outerT innerT) where
    hoist ::
        forall m1 m2.
        (Monad m1, Monad m2) =>
        (m1 --> m2) ->
        ComposeT outerT innerT m1 --> ComposeT outerT innerT m2
    hoist f (MkComposeT ma) =
        case hasTransConstraint @Monad @innerT @m1 of
            Dict ->
                case hasTransConstraint @Monad @innerT @m2 of
                    Dict -> MkComposeT $ hoist (hoist f) ma

instance (MonadTransTunnel outerT, MonadTransTunnel innerT) => MonadTransTunnel (ComposeT outerT innerT) where
    type Tunnel (ComposeT outerT innerT) = ComposeInner (Tunnel outerT) (Tunnel innerT)
    tunnel ::
        forall m2 r.
        Monad m2 =>
        ((forall m1 a. Monad m1 => ComposeT outerT innerT m1 a -> m1 (ComposeInner (Tunnel outerT) (Tunnel innerT) a)) -> m2 (ComposeInner (Tunnel outerT) (Tunnel innerT) r)) ->
        ComposeT outerT innerT m2 r
    tunnel call =
        case hasTransConstraint @Monad @innerT @m2 of
            Dict ->
                MkComposeT
                    $ tunnel
                    $ \unlift1 ->
                        tunnel $ \unlift2 ->
                            fmap unComposeInner
                                $ call
                                $ \(MkComposeT ff :: _ m1 _) ->
                                    case hasTransConstraint @Monad @innerT @m1 of
                                        Dict -> fmap MkComposeInner $ unlift2 $ unlift1 $ ff

instance
    (MonadTransCoerce outerT, MonadTransCoerce innerT, TransConstraint Monad innerT) =>
    MonadTransCoerce (ComposeT outerT innerT)
    where
    transCoerce ::
        forall m1 m2.
        Coercible m1 m2 =>
        Dict (Coercible (ComposeT outerT innerT m1) (ComposeT outerT innerT m2))
    transCoerce =
        case transCoerce @innerT @m1 @m2 of
            Dict ->
                case transCoerce @outerT @(innerT m1) @(innerT m2) of
                    Dict -> Dict

instance (MonadTransUnlift outerT, MonadTransUnlift innerT) => MonadTransUnlift (ComposeT outerT innerT) where
    liftWithUnlift ::
        forall m r.
        MonadIO m =>
        (Unlift MonadTunnelIO (ComposeT outerT innerT) -> m r) ->
        ComposeT outerT innerT m r
    liftWithUnlift call =
        case hasTransConstraint @MonadIO @innerT @m of
            Dict ->
                MkComposeT
                    $ liftWithUnlift
                    $ \unlift1 ->
                        liftWithUnlift $ \unlift2 ->
                            call $ \(MkComposeT t1t2ma) -> unlift2 $ withTransConstraintTM @MonadTunnelIO $ unlift1 t1t2ma
    getDiscardingUnlift ::
        forall m.
        Monad m =>
        ComposeT outerT innerT m (WUnlift MonadTunnelIO (ComposeT outerT innerT))
    getDiscardingUnlift =
        case hasTransConstraint @Monad @innerT @m of
            Dict ->
                MkComposeT
                    $ withTransConstraintTM @Monad
                    $ do
                        unlift1 <- getDiscardingUnlift
                        unlift2 <- lift getDiscardingUnlift
                        return $ composeTWUnlift unlift1 unlift2

instance (MonadTransAskUnlift outerT, MonadTransAskUnlift innerT) => MonadTransAskUnlift (ComposeT outerT innerT) where
    askUnlift ::
        forall m.
        Monad m =>
        ComposeT outerT innerT m (WUnlift Monad (ComposeT outerT innerT))
    askUnlift =
        case hasTransConstraint @Monad @innerT @m of
            Dict ->
                MkComposeT
                    $ withTransConstraintTM @Monad
                    $ do
                        unlift1 <- askUnlift
                        unlift2 <- lift askUnlift
                        return $ composeTWUnlift unlift1 unlift2
