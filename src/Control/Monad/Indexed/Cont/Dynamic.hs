module Control.Monad.Indexed.Cont.Dynamic(Cont(..),control,prompt,shift,reset,execCont) where

import Control.Monad.Indexed

newtype F t = F (t -> Trail t -> t)

type Trail t = [F t]

newtype Cont t r r' a = Cont { 
      runCont :: (a -> Trail t -> r') -> Trail t -> r
      }

execCont :: Cont t r r' r' -> r
execCont c = runCont c (\x [] -> x) []

instance IxPointed (Cont t) where
    ireturn v = Cont $ \k t -> k v t 

instance IxMonad (Cont t) where
    ibind e2 e1 = Cont $ \k t -> runCont e1 (
          \m1 t1 -> runCont (e2 m1) (\m2 t2 -> k m2 t2) t1) t

instance IxFunctor (Cont t) where
    imap f = ibind (ireturn . f)

instance IxApplicative (Cont t) where
    iap f v = ibind (flip imap v) f

control :: ((a -> Cont t b t t) -> Cont r g r r) -> Cont t g b a
control e = Cont $ \k t -> let
                 c = \x -> Cont  $ \k' t' -> k x (
                            t ++ F (\x' t'' -> k' x' t'') : t')
                 in runCont (e c) theta1 []

theta1 :: a -> Trail a -> a
theta1 x t = case t of
                []          -> x
                (F k1 : t1) -> k1 x t1

prompt, reset :: Cont t b t t -> b
prompt e = runCont e theta1 []

reset = prompt

shift :: ((a -> Cont t' i i b) -> Cont r g r r) -> Cont t g b a
shift f = control (\k -> f (ireturn . reset . k))
