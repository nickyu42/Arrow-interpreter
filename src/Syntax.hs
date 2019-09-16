module Syntax where

data Expr
    = Lit Int
    | Var String
    | Add Expr Expr
    | Sub Expr Expr
    | Mult Expr Expr
    | Div Expr Expr
    | Rem Expr Expr
    deriving (Show, Eq)

data Boolean 
    = Boolean Bool
    | Equal Expr Expr
    | Less Expr Expr
    | And Boolean Boolean
    | Or Boolean Boolean
    | Not Boolean
    deriving (Show, Eq)

data Command
    = Skip
    | Declare String Expr Command
    | Print Expr
    | Assign String Expr
    | Seq Command Command
    | While Boolean Command
    | If Boolean Command Command
    | Trace Expr Command
    deriving (Show, Eq)