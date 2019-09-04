{-# LANGUAGE Arrows #-}
module Interp where

import Prelude hiding (lookup, fail)

import qualified Data.Map as M
import Control.Arrow
import Control.Category

-- class Control.Category.Category a => Arrow (a :: * -> * -> *) where
--     arr :: (b -> c) -> a b c
--     first :: a b c -> a (b, d) (c, d)
--     second :: a b c -> a (d, b) (d, c)
--     (***) :: a b c -> a b' c' -> a (b, b') (c, c')
--     (&&&) :: a b c -> a b c' -> a b (c, c')

data Expr
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

instance ArrowChoice A where
    left (A f) = A $ \env e -> case e of
        Left b -> case f env b of
            (Left err, env') -> (Left err, env')
            (Right c, env') -> (Right (Left c), env')
        Right d -> (Right (Right d), env)

lookup :: (Ord k, Show k) => A (k, M.Map k a) a
lookup = proc (k, map) -> case M.lookup k map of
    Nothing -> fail -< "Variable not bound"
    Just v -> returnA -< v

fail :: A String a
fail = A $ \env err -> (Left err, env)

getEnv :: A () Env
getEnv = A $ \env _ -> (Right env, env)

eval :: A Expr Val
eval = proc e -> case e of
    Var s -> do
        env <- getEnv -< ()
        lookup -< (s, env)
