{- Assignment 2 - A Racket Interpreter

This module is the main program for the interpreter.
All of your work should go into this file.

We have provided a skeleton interpreter which can be run
successfully on sample.rkt; it is your job to extend this
program to handle the full range of Paddle.

In the space below, please list your group member(s):
Gilberto Aristizabal, g3aristi
<Full name>, <CDF Account>
-}

module Interpreter (main) where

import BaseParser (BaseExpr(LiteralInt, LiteralBool, Atom, Compound), parseFile)
import Data.List
import System.Environment (getArgs)


-- |Run interpreter on an input file,
--  either from commandline or user input.
--  You should not need to change this function.
main :: IO ()
main =
    getArgs >>= \args ->
    if length args > 0
    then
        --(head args) gets the first argument given in the command line
        parseFile (head args) >>= \baseTree ->
        putStr (interpretPaddle baseTree)
    else
        putStrLn "Enter the name of a file: " >>
        getLine >>= \file ->
        parseFile file >>= \baseTree ->
        putStr (interpretPaddle baseTree)


-- |Take the output of the base parser and interpret it,
--  first constructing the AST, then evaluating it,
--  and finally returning string representations of the results.
--  You will need to make this function more robust against errors.

-- **************************** Make it handle errors [a TypeError is emitted.]
-- AST Abstract Syntax Tree
interpretPaddle :: Maybe [BaseExpr] -> String
interpretPaddle (Just exprs) =
    let ast = map parseExpr exprs
        vals = map evaluate ast
    in
        -- String representations of each value, joined with newlines
        unlines (map show vals)


-- ******************EVERY NEW FEATURE WILL CHANGE THIS DECLARATION***************
-- An expression data type
data Expr = Number Integer |
            Boolean Bool |
            If Expr Expr Expr  | -- Here are the arithmetic, comparison, equality, and boolean operations
            AddOp Expr Expr


instance Show Expr where
    show (Number x) = show x
    show (Boolean True) = "#t"
    show (Boolean False) = "#f"
    -- Note: the following definition is not necessary for this assignment,
    -- but you may find it helpful to define string representations of all
    -- expression forms.
    show (If e1 e2 e3) =
        "(if " ++ show e1 ++ " " ++ show e2 ++ " " ++ show e3 ++ ")"


-- ******************EVERY NEW FEATURE WILL CHANGE THIS FUNCTION ***************
-- |Take a base tree produced by the starter code,
--  and transform it into a proper AST.
parseExpr :: BaseExpr -> Expr
parseExpr (LiteralInt n) = Number n
parseExpr (LiteralBool b) = Boolean b
parseExpr (Compound [Atom "if", b, x, y]) =
    If (parseExpr b) (parseExpr x) (parseExpr y)
parseExpr (Compound [Atom "+", LiteralInt a, LiteralInt b]) =
    AddOp (Number a) (Number b)
parseExpr (Compound [Atom "-", LiteralInt a, LiteralInt b]) =
    SubOp (Number a) (Number b)


-- ******************EVERY NEW FEATURE WILL CHANGE THIS FUNCTION ***************
-- |Evaluate an AST by simplifying it into
--  a number, boolean, list, or function value.
evaluate :: Expr -> Expr
evaluate (Number n) = Number n
evaluate (Boolean b) = Boolean b
-- The proble with if is that it is not recursive
evaluate (If cond x y) =
    case cond of
        Boolean True -> x
        Boolean False -> y
evaluate(AddOp a b) =
    (lift (+0))(a)(b)
evaluate(SubOp a b) =
    (lift (+0))(a)(b)

lift :: (Integer -> Integer) -> Expr -> Expr -> Expr
lift f (Number x) (Number y) = Number (f (x + y))