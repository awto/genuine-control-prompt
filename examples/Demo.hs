module Demo where

import Control.Monad.Indexed
import Control.Monad.Indexed.Cont.Dynamic

foo xs = prompt (visit xs) 
    where
      visit [] = ireturn []
      visit (x : xs) = control (\k -> k xs >>>= ireturn . (x :)) >>>= visit

foo' xs = reset (visit xs)
    where
      visit [] = ireturn []
      visit (x : xs) = shift (\k -> k xs >>>= ireturn . (x :)) >>>= visit

run = do
  print (foo i)   --- [3,2,1]
  print (foo' i)  --- [1,2,3]
  where i = [1 :: Int,2,3]

