{-# LANGUAGE Arrows #-}
{-# OPTIONS_GHC -Wall #-}
module Interp where

import Prelude hiding (lookup, fail)

import qualified Data.Map as M
import Control.Arrow
import Control.Category

data Expr
    = Lit Int
    | Var String
    | Add Expr Expr
    deriving (Show, Eq)
    -- | Sub Expr Exp

data Val
    = Num Int
    deriving (Show, Eq)

data Command
    = Skip
    | Declare String Expr Command
    | Print Expr
    | Assign String Expr
    | Seq Command Command
    deriving (Show, Eq)
    -- | While Expr Command
    -- | If Expr Command Command

type Env = M.Map String Val

newtype A b c = A { unpack :: (Env -> b -> (Either String c, Env)) }

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

insert :: A (String, Val) ()
insert = A $ \env (k, val) -> (Right (), M.insert k val env)

lookup :: A String Val
lookup = A $ \env k -> case M.lookup k env of
    Nothing -> (Left "Variable not bound", env)
    Just v -> (Right v, env)

fail :: A String a
fail = A $ \env err -> (Left err, env)

getEnv :: A () Env
getEnv = A $ \env _ -> (Right env, env)

add :: A (Val, Val) Val
add = proc e -> case e of
    (Num x, Num y) -> do 
        returnA -< Num (x + y)

skip :: A () String
skip = A $ \env _ -> (Right "", env)

eval :: A Expr Val
eval = proc e -> case e of
    Lit x -> returnA -< Num x
    Var s -> do
        lookup -< s
    Add e1 e2 -> do
        v1 <- eval -< e1
        v2 <- eval -< e2
        case (v1, v2) of
            (Num x, Num y) -> returnA -< Num (x + y)

run :: A Command String
run = proc c -> case c of
    Skip -> do
        skip -< ()
    Declare s e c' -> do
        val <- eval -< e
        insert -< (s, val)
        run -< c'
    Print e -> do
        val <- eval -< e
        returnA -< (show val) ++ "\n"
    Assign s e -> do
        val <- eval -< e
        lookup -< s -- guard
        insert -< (s, val)
        skip -< ()
    Seq c1 c2 -> do
        s1 <- run -< c1
        s2 <- run -< c2
        returnA -< s1 ++ s2

exec :: (Show c) => Env -> A b c -> b -> IO ()
exec env a b = case unpack a env b of
    (Right c, _) -> putStrLn $ show c
    (Left err, env') -> putStrLn $ err ++ "\n" ++ show env'
