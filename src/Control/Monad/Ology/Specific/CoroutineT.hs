module Control.Monad.Ology.Specific.CoroutineT where

import Control.Monad.Ology.Specific.StepT
import Import

data Turn p q a
    = MkTurn
        p
        (q -> a)

instance Functor (Turn p q) where
    fmap ab (MkTurn p qa) = MkTurn p $ fmap ab qa

type CoroutineT p q = StepT (Turn p q)

runCoroutine :: Monad m => CoroutineT p p m a -> m a
runCoroutine = runSteps $ \(MkTurn p pa) -> pa p

yieldCoroutine :: Monad m => p -> CoroutineT p q m q
yieldCoroutine p = pendingStep $ MkTurn p id

joinCoroutines :: Monad m => CoroutineT q r m a -> (q -> CoroutineT p q m a) -> CoroutineT p r m a
joinCoroutines cqr qcpq =
    MkStepT $ do
        eqra <- unStepT cqr
        case eqra of
            Left a -> return $ Left a
            Right (MkTurn q rf) -> do
                epqa <- unStepT $ qcpq q
                return $ fmap (\(MkTurn p qa) -> MkTurn p $ \r -> joinCoroutines (rf r) qa) $ epqa
