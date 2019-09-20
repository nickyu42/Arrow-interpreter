module Main where

import qualified Data.Map as M

import Syntax
import Interp

example :: Command
example = do
    Declare "n" (Lit 100) $
        Declare "k" (Lit 2) $
            While (Less (Var "k") (Var "n")) $
                Declare "i" (Lit 2) $
                    Declare "isPrime" (Lit 1) $
                        Seq
                            (While (And (Equal (Var "isPrime") (Lit 1)) (Less (Mult (Var "i") (Var "i")) (Add (Var "k") (Lit 1)))) $
                                Seq
                                    (If (Equal (Rem (Var "k") (Var "i")) (Lit 0))
                                        (Assign "isPrime" (Lit 0))
                                        Skip)
                                    (Assign "i" (Add (Var "i") (Lit 1)))
                            )
                            (Seq
                                (If (Equal (Var "isPrime") (Lit 1))
                                    (Print (Var "k"))
                                    Skip)
                                (Assign "k" (Add (Var "k") (Lit 1))))
            

main :: IO ()
main = exec M.empty run example
