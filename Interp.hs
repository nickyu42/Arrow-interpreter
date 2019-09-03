{-# LANGUAGE Arrows #-}
module Interp where

import Prelude hiding (lookup, fail)

import Data.Map as M
import Control.Arrow
import Control.Category

-- class Control.Category.Category a => Arrow (a :: * -> * -> *) where
--     arr :: (b -> c) -> a b c
--     first :: a b c -> a (b, d) (c, d)
--     second :: a b c -> a (d, b) (d, c)
--     (***) :: a b c -> a b' c' -> a (b, b') (c, c')
--     (&&&) :: a b c -> a b c' -> a b (c, c')

data Exp
    = Var String
    -- | Add Exp Exp

data Val
    = Num Int

type Env = M.Map String Val

data A b c = A (Env -> b -> (Either String c, Env))

instance Category A where
    id = A (\env b -> (Right b, env))
    A g . A f = A $ \env b -> case f env b of
        (Left err, env') -> (Left err, env')
        (Right c, env') -> g env' c

instance Arrow A where
    arr f = A $ \env b -> (Right (f b), env)
    first (A f) = A $ \env (b, d) -> case f env b of
        (Left err, env') -> (Left err, env')
        (Right c, env') -> (Right (c, d), env')

lookup :: (Ord k, Show k) => A (k, M.Map k a) a
lookup = proc (k, map) -> case M.lookup k map of
    Nothing -> fail -< "Variable " ++ k ++ " does not exist"
    Just v -> returnA -< v

-- eval :: Exp -> A Env (Maybe Val)
-- eval (Var s) = arr (lookup' s)

fail :: A String a
fail = \env err -> (Left err, env)