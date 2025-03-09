module Control.Monad.Ology.Specific.ResultT where

import Control.Monad.Ology.Data.Exn
import Control.Monad.Ology.General
import Control.Monad.Ology.Specific.ComposeInner
import Control.Monad.Ology.Specific.Result
import Import

type ResultT e = ComposeInner (Result e)

runResultT :: forall m e a. ResultT e m a -> m (Result e a)
runResultT = unComposeInner

-- | Throw the parameterised exception type.
throwR ::
    forall m e a.
    Monad m =>
    e ->
    ResultT e m a
throwR e = liftInner $ throwExc e

-- | Catch the parameterised exception type.
catchR ::
    forall m e e' a.
    Monad m =>
    ResultT e m a ->
    (e -> ResultT e' m a) ->
    ResultT e' m a
catchR (MkComposeInner rma) handler =
    MkComposeInner $ do
        rea <- rma
        case rea of
            SuccessResult a -> return $ SuccessResult a
            FailureResult e -> unComposeInner $ handler e

resultExn ::
    forall m e.
    Monad m =>
    Exn (ResultT e m) e
resultExn = MkExn throwR catchR
