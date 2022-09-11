module Control.Monad.Ology.Specific.ReaderStateT where

import Control.Monad.Ology.Data
import Control.Monad.Ology.General
import Control.Monad.Ology.Specific.StateT
import Import

type ReaderStateT f m = StateT (WRaised f m) m

evalReaderStateT :: Monad m => ReaderStateT f m a -> (f --> m) -> m a
evalReaderStateT rsa fm = evalStateT rsa (MkWRaised fm)

readerStateLift :: (Monad f, Monad m) => f --> ReaderStateT f m
readerStateLift fa = do
    MkWRaised fm <- get
    a <- lift $ fm fa
    put $ MkWRaised $ \c -> fm $ fa >> c
    return a

readerStateUpdate :: Monad m => (f --> f) -> ReaderStateT f m ()
readerStateUpdate ff = modify (\fm -> fm . MkWRaised ff)

readerStateParamRef ::
       forall f m. Monad m
    => Param f --> Ref (ReaderStateT f m)
readerStateParamRef (param :: _ a) = let
    refGet :: ReaderStateT f m a
    refGet = do
        MkWRaised ff <- get
        lift $ ff $ paramAsk param
    refPut :: a -> ReaderStateT f m ()
    refPut a = readerStateUpdate $ paramWith param a
    in MkRef {..}
