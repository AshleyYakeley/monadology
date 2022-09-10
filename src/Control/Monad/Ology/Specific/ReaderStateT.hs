module Control.Monad.Ology.Specific.ReaderStateT where

import Control.Monad.Ology.Data
import Control.Monad.Ology.General
import Control.Monad.Ology.Specific.StateT
import Import

type ReaderStateT f m = StateT (Raised f m) m

evalReaderStateT :: Monad m => ReaderStateT f m a -> (f --> m) -> m a
evalReaderStateT rsa fm = evalStateT rsa (MkRaised fm)

readerStateLift :: (Monad f, Monad m) => f --> ReaderStateT f m
readerStateLift fa = do
    MkRaised fm <- get
    a <- lift $ fm fa
    put $ MkRaised $ \c -> fm $ fa >> c
    return a

readerStateUpdate :: Monad m => (f --> f) -> ReaderStateT f m ()
readerStateUpdate ff = modify (\fm -> fm . MkRaised ff)

readerParamRef ::
       forall f m a. Monad m
    => Param f a
    -> Ref (ReaderStateT f m) a
readerParamRef param = let
    refGet :: ReaderStateT f m a
    refGet = do
        MkRaised ff <- get
        lift $ ff $ paramAsk param
    refPut :: a -> ReaderStateT f m ()
    refPut a = readerStateUpdate $ paramWith param a
    in MkRef {..}
