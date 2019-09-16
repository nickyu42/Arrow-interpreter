{-# LANGUAGE Arrows #-}
{-# OPTIONS_GHC -Wall #-}
module Interp where

import Prelude hiding (lookup, fail)

import qualified Data.Map as M
import Control.Arrow
import Control.Category
import Debug.Trace

import Syntax

data Val
    = Num Int
    | Bl Bool
    deriving (Eq)

instance Show Val where
    show (Num x) = show x
    show (Bl x) = show x

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

skip :: A () String
skip = A $ \env _ -> (Right "", env)

appN :: (Int -> Int -> Int) -> A (Expr, Expr) Val
appN f = proc p -> do
    v <- (eval *** eval) -< p
    case v of
        (Num x, Num y) -> returnA -< Num (x `f` y)
        _ -> fail -< "Cannot apply on non-numbers"

eval :: A Expr Val
eval = proc e -> case e of
    Lit x -> returnA -< Num x
    Var s -> lookup -< s
    Add e1 e2 -> appN (+) -< (e1, e2)
    Sub e1 e2 -> appN (-) -< (e1, e2)
    Mult e1 e2 -> appN (*) -< (e1, e2)
    Div e1 e2 -> appN div -< (e1, e2)
    Rem e1 e2 -> appN rem -< (e1, e2)

evalB :: A Boolean Bool
evalB = proc e -> case e of
    Boolean b -> returnA -< b
    Equal e1 e2 -> do
        v <- (eval *** eval) -< (e1, e2)
        case v of 
            (Num x, Num y) -> returnA -< x == y
            _ -> fail -< "Cannot apply on non-numbers"
    Less e1 e2 -> do
        v <- (eval *** eval) -< (e1, e2)
        case v of 
            (Num x, Num y) -> returnA -< x < y
            _ -> fail -< "Cannot apply on non-numbers"
    And e1 e2 -> do
        (x, y) <- (evalB *** evalB) -< (e1, e2)
        returnA -< x && y
    Or e1 e2 -> do
        (x, y) <- (evalB *** evalB) -< (e1, e2)
        returnA -< x || y
    Not e' -> do
        v <- evalB -< e'
        returnA -< not v
        

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
    w @ (While x c') -> do
        run -< If x (Seq c' w) Skip
    If x c1 c2 -> do
        cond <- evalB -< x
        if cond
            then run -< c1
            else run -< c2
    Trace e c' -> do
        v <- eval -< e
        let _ = trace $ "[debug]" ++ show v
        run -< c'

exec :: (Show c) => Env -> A b c -> b -> IO ()
exec env a b = case unpack a env b of
    (Right c, _) -> putStrLn $ show c
    (Left err, env') -> putStr $ err ++ "\n" ++ show env'
