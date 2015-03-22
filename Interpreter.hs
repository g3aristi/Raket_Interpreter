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
            AddOp Expr Expr |
            SubOp Expr Expr |
            MulOp Expr Expr | 
            LtOp Expr Expr |
            Not Expr | 
            List [Expr] 

instance Show Expr where
    show (Number x) = show x
    show (Boolean True) = "#t"
    show (Boolean False) = "#f"
    -- Note: the following definition is not necessary for this assignment,
    -- but you may find it helpful to define string representations of all
    -- expression forms.
    show (If e1 e2 e3) =
        "(if " ++ show e1 ++ " " ++ show e2 ++ " " ++ show e3 ++ ")"


    --Show lists
    show (List []) = "'()"
    show (List a) = (showList a "'(")

    --Must comment on stuff
    showList [] curr_lst = curr_lst
    showList [a] curr_lst = curr_lst ++ show a ++ ")"
    showList (aH:aT) curr_lst = showList aT (curr_lst ++ show aH ++ " ")


-- ******************EVERY NEW FEATURE WILL CHANGE THIS FUNCTION ***************
-- |Take a base tree produced by the starter code,
--  and transform it into a proper AST.
parseExpr :: BaseExpr -> Expr
parseExpr (LiteralInt n) = Number n
parseExpr (LiteralBool b) = Boolean b

parseExpr (Compound [Atom "if", b, x, y]) =
    If (parseExpr b) (parseExpr x) (parseExpr y)

--addition function
parseExpr (Compound [Atom "+", LiteralInt a, LiteralInt b]) =
    AddOp (Number a) (Number b)
--subtraction function
parseExpr (Compound [Atom "-", LiteralInt a, LiteralInt b]) =
    SubOp (Number a) (Number b)
--multiply function
parseExpr (Compound [Atom "*", LiteralInt a, LiteralInt b]) =
    MulOp (Number a) (Number b)
--less than function
parseExpr (Compound [Atom "<", LiteralInt a, LiteralInt b]) =
    LtOp (Number a) (Number b)
--not function
parseExpr (Compound [Atom "not", LiteralBool a]) =
    Not (Boolean a)
--list functions
parseExpr (Atom "list") = 
    List []
parseExpr (Compound ((Atom "list"):vals)) =
    List (map parseExpr vals)

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
-- Arithmetic operations [NOT  SURE ABOUT /, sqrt, log ...]
-- Addition
evaluate(AddOp a b) =
    (addExpr (+) a b)
-- Subtration
evaluate(SubOp a b) =
    (subExpr (-) a b)
-- Multiplication
evaluate(MulOp a b) =
    (mulExpr (*) a b)
-- Comparison operations (<)
evaluate(LtOp a b) =
    (ltExpr (<) a b)

evaluate(Not a) =
    (notExpr (not) a)

evaluate(List a) =
    (List (map evaluate a))
-- Equality operations (equals?)

-- Boolean operations (not) [NOT SURE ABOUT ^ NAND ...]
-- AND (&&)

-- lift :: (Integer -> Integer) -> Expr -> Expr -> Expr
-- lift f (Number x) (Number y) = Number (f (x + y))

addExpr :: (Integer -> Integer -> Integer) -> Expr -> Expr -> Expr
addExpr f (Number x) (Number y) = Number (f x y)

subExpr :: (Integer -> Integer -> Integer) -> Expr -> Expr -> Expr
subExpr f (Number x) (Number y) = Number (f x y)

mulExpr :: (Integer -> Integer -> Integer) -> Expr -> Expr -> Expr
mulExpr f (Number x) (Number y) = Number (f x y)

ltExpr :: (Integer -> Integer -> Bool) -> Expr -> Expr -> Expr
ltExpr f (Number x) (Number y) = Boolean (f x y)

notExpr :: (Bool -> Bool) -> Expr -> Expr
notExpr f (Boolean a) = Boolean (f a)

