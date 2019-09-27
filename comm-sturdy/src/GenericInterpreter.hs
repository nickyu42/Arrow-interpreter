{-# LANGUAGE Arrows #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module GenericInterpreter where

import           Prelude hiding (lookup, and, fail, not)

import           Control.Arrow
import           Control.Arrow.Fix
import           Control.Arrow.Environment
import qualified Control.Arrow.Environment as Env
import           Control.Arrow.Store
import qualified Control.Arrow.Store as Store
import           Control.Arrow.Fail
import           GHC.Exts

import SyntaxSturdy(Expr(..), Command(..))

class Arrow c => IsValue val c | c -> val where
    numLit :: c Int val
    boolLit :: c Bool val
    add :: c (val, val) val
    and :: c (val, val) val
    lt :: c (val, val) val
    eq :: c (val, val) val
    not :: c val val
    if_ :: c x () -> c y () -> c (val,x,y) ()

eval :: (Show addr, IsString err, IsValue val c, ArrowChoice c,
        ArrowEnv String addr c, ArrowStore addr val c, ArrowFail err c,
        Env.Join val c, Store.Join val c
  ) => c Expr val
eval = proc e -> case e of
    Lit x -> numLit -< x
    BoolLit b -> boolLit -< b
    Var s -> lookup'' read' -< s
    Add e1 e2 -> do
        v <- (eval *** eval) -< (e1, e2)
        add -< v
    Equal e1 e2 -> do
        v <- (eval *** eval) -< (e1, e2)
        eq -< v
    And e1 e2 -> do
        v <- (eval *** eval) -< (e1, e2)
        and -< v
    Not e' -> do
        v <- eval -< e'
        not -< v