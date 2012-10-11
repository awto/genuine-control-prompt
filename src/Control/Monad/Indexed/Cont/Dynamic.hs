module Control.Monad.Indexed.Cont.Dynamic(
                              runCont,prompt,control,shift',reset'
                              ) where

import Control.Monad.Indexed.Cont
import Control.Monad.Reader

newtype F m t = F (t -> ReaderT (Trail m t) m t)

type Trail m t = [F m t]

type Cont m t = IxContT (ReaderT (Trail m t) m)

liftCont :: Monad m => m a -> Cont m t i i a
liftCont c = IxContT $ \k -> lift c >>= k

runCont :: IxContT (ReaderT t m) a o b -> (b -> ReaderT t m o) -> t -> m a
runCont c k t = runReaderT (
                 runIxContT c k
                 ) t

theta1 :: Monad m => a -> ReaderT (Trail m a) m a
theta1 x = ReaderT $ \t -> case t of
                []          -> return x
                (F k1 : t1) -> runReaderT (k1 x) t1

control :: Monad m => ((a -> Cont m t b t t) -> Cont m r g r r) 
        -> Cont m t g b a
control e = IxContT $ \k -> ReaderT $ \t -> let
                 c = \x -> IxContT  
                     $ \k' -> local 
                              (\t' -> t ++ F (\x' -> k' x') : t') 
                              (k x)
                 in runCont (e c) theta1 []

prompt, reset' :: Monad m => Cont m t b t t -> m b
prompt e = runCont e theta1 []

reset' = prompt

shift' :: Monad m => ((a -> Cont m t' i i b) -> Cont m r g r r) 
       -> Cont m t g b a
shift' f = control (\k -> f (liftCont . reset' . k))

