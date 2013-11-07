module Control.Exception.SmallPrint
 ((*@)
 ,(*@@)
 ,exception) where

infixl  5 *@
(*@) :: a -> (Bool, a) -> a
a *@ (cond,ex) =
 case cond of
  True  -> ex
  False -> a

infixl 5 *@@
(*@@) :: (Monad m) => m a -> (m Bool, m a) -> m a
a *@@ (cond,ex) = do
 cond' <- cond
 case cond' of
  True  -> ex
  False -> a

exception
 :: cond
 -> ex
 -> (cond,ex)
exception cond ex = (cond,ex)