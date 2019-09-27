{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
module SyntaxSturdy where

import GHC.Generics

data Expr
    = Lit Int
    | BoolLit Bool
    | Var String
    | Add Expr Expr
    | Equal Expr Expr
    | And Expr Expr
    | Not Expr
    deriving (Show, Eq, Generic)
    -- | Sub Expr Expr
    -- | Mult Expr Expr
    -- | Div Expr Expr
    -- | Rem Expr Expr
    -- | Less Expr Expr
    -- | Or Expr Expr

data Command
    = Skip
    | Declare String Expr Command
    | Print Expr
    | Assign String Expr
    | Seq Command Command
    | While Expr Command
    | If Expr Command Command
    deriving (Show, Eq, Generic)