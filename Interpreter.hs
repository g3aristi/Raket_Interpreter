{- Assignment 2 - A Racket Interpreter

This module is the main program for the interpreter.
All of your work should go into this file.

We have provided a skeleton interpreter which can be run
successfully on sample.rkt; it is your job to extend this
program to handle the full range of Paddle.

In the space below, please list your group member(s):
Gilberto Aristizabal, g3aristi
Tyler Pham, g3phamty

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
        parseFile (head args) >>= \baseTree ->
        putStr (interpretPaddle baseTree)
    else
        putStrLn "Enter the name of a file: " >>
        getLine >>= \file ->
        parseFile file >>= \baseTree ->
        putStr (interpretPaddle baseTree)

-- Evaluates expressions in a list format
listExprs :: [Expr] -> [([Char], Expr)] -> [Expr]
listExprs ast ids = 
    if ((length ast) == 1)
        then let (_, output) = (evaluate ids (head ast))
        in output:[]
        else let (n_ids, output) = (evaluate ids (head ast))
        in output:(listExprs (tail ast) n_ids)

-- |Take the output of the base parser and interpret it,
--  first constructing the AST, then evaluating it,
--  and finally returning string representations of the results.
--  You will need to make this function more robust against errors.
interpretPaddle :: Maybe [BaseExpr] -> String
interpretPaddle (Just exprs) =
    let ast = map parseExpr exprs 
        vals = listExprs ast []
    in
        -- String representations of each value, joined with newlines
        unlines (map showLists (chk4Error (filter notNull vals) []))

-- An expression data type
data Expr = Number Integer |
            Boolean Bool |
            If Expr Expr Expr |
            TwoArgExpr BaseExpr Expr Expr |
            Not Expr |
            List [Expr] |
            Cond [Expr] |
            Identifier [Char] Expr |
            Function [Char] Expr |
            FunctionApp Expr [Expr] |
            Error [Char] |
            Null

-- Helper for Show lists
outputList [] init = init
outputList [x] init = init ++ show x ++ ")"
outputList (x:xs) init = outputList xs (init ++ show x ++ " ")

instance Show Expr where
    show (Number x) = show x
    show (Boolean True) = "#t"
    show (Boolean False) = "#f"
    -- Note: the following definition is not necessary for this assignment,
    -- but you may find it helpful to define string representations of all
    -- expression forms.
    show (If e1 e2 e3) =
        "(if " ++ show e1 ++ " " ++ show e2 ++ " " ++ show e3 ++ ")"
    show (List []) = "'()"
    show (List vals) = (outputList vals "(")
    show (Identifier _ expr) = show expr
    show (Function _ _) = "#<procedure>"
    show (Error errType) = errType
    show (Null) = ""

-- |Take a base tree produced by the starter code,
--  and transform it into a proper AST.
parseExpr :: BaseExpr -> Expr
parseExpr (LiteralInt n) = Number n
parseExpr (LiteralBool b) = Boolean b
parseExpr (Compound [Atom "if", b, x, y]) =
    If (parseExpr b) (parseExpr x) (parseExpr y)
parseExpr (Compound [Atom "else", expr]) =
   parseExpr expr

-- Parse any two argument expression
parseExpr (Compound [(Atom opStr), x, y]) =
    TwoArgExpr (Atom opStr) (parseExpr x) (parseExpr y)

-- Parse a 'not' expression
parseExpr (Compound [Atom "not", x]) =
    Not (parseExpr x)

-- Parse a 'list' expression
parseExpr (Atom "list") =
    List []
parseExpr (Compound ((Atom "list"):vals)) =
    List (map parseExpr vals)

-- Parse a 'cond' expression
parseExpr (Compound ((Atom "cond"):exprs)) =
    Cond (map parseExpr exprs)

-- Parse an 'identifier' expression
parseExpr (Atom identName) =
    (Identifier identName Null)

-- Parse a 'define' expression
parseExpr (Compound [Atom "define", Atom name, expr]) =
    Identifier name (parseExpr expr)

-- Parse a 'let' expression
parseExpr (Compound ((Atom "let"):exprs)) = 
    Number 1

-- Parse a 'let*' expression
parseExpr (Compound ((Atom "let*"):exprs)) = 
    Number 1

parseExpr (Compound [x, expr]) = 
    let output = parseExpr x
    in case output of
           Boolean True -> parseExpr expr
           _ -> Null
    

-- |Evaluate an AST by simplifying it into
--  a number, boolean, list, or function value.
evaluate :: [([Char], Expr)] -> Expr -> ([([Char], Expr)], Expr)
evaluate ids (Number n) = (ids, (Number n))
evaluate ids (Boolean b) = (ids, (Boolean b))

-- If Statement with subexpression for all three parameters
evaluate identifier (If (Boolean cond) x y) =
    case cond of
        True -> (evaluate identifier x)
        False -> (evaluate identifier y)

evaluate identifier (If cond x y) =
    let (_, output) = (evaluate identifier cond)
    in (evaluate identifier (If output x y))

-- Evaluate some kind of binary expression
evaluate identifier (TwoArgExpr (Atom opStr) (Number x) (Number y)) =
    case opStr of
        "+" -> (identifier, (Number (x + y)))
        "*" -> (identifier, (Number (x * y)))
        "equal?" -> (identifier, (Boolean (x == y)))

evaluate identifier (TwoArgExpr (Atom opStr) (Boolean x) (Boolean y)) =
   case opStr of
       "and" -> (identifier, (Boolean (x && y)))
       "or" -> (identifier, (Boolean (x || y)))
       "equal?" -> (identifier, (Boolean (x == y)))

evaluate identifier (TwoArgExpr (Atom opStr) x y) =
    let (identX, outputX) = (evaluate identifier x)
    in let (identY, outputY) = (evaluate identifier y)
    in (evaluate identifier (TwoArgExpr (Atom opStr) outputX outputY))

-- Evaluate a 'not' expression
evaluate identifier (Not (Boolean x)) = (identifier, (Boolean (not x)))
evaluate identifier (Not x) = 
    let (_, output) = (evaluate identifier x)
    in (evaluate identifier (Not output))

-- Evaluate a 'list'
evaluate identifier (List vals) = 
    (identifier, (List (map (\x -> let (_, output) = (evaluate identifier x) in output) vals)))

evaluate identifier (Identifier name Null) =
    (identifier, (getIdentifier name identifier))
evaluate identifier (Identifier name expr) =
    ((name, expr):identifier, Null)

-- Evaluate an 'Error'
evaluate identifier (Error errorType) =
    (identifier, (Error errorType))

-- Evaluate a 'Cond'
evaluate identifier (Cond exprs) = 
    let (_, output) = (evaluate identifier (head (filter notNull exprs)))
    in (identifier, output)


-- ********** HELPER FUNCTIONS **********

-- appends values to show output 
showLists :: Expr -> [Char]
showLists expr = 
    case expr of
        (List _) -> "'"++(show expr)
        _ -> (show expr)

flaw :: Expr -> Bool
flaw expr = 
    case expr of
        (Error _) -> True
        _ -> False

-- Helper that checks a list of expressions for an error.
-- Returns the list up to the error, otherwise the
-- entire list is returned
chk4Error :: [Expr] -> [Expr] -> [Expr]
chk4Error [] acc = acc
chk4Error exprs acc =
    if (flaw (head exprs))
        then acc++[(head exprs)]
        else chk4Error (tail exprs) (acc++[(head exprs)])

-- returns True iff an Expr is a 'Null' type
notNull :: Expr -> Bool
notNull expr = 
    case expr of
        Null -> False
        _ -> True

getMaybe (Just val) = val
getMaybe (Nothing) = (Error "Syntax error")

-- Helper function that searches for an identifier and returns its corresponding value
getIdentifier :: [Char] -> [([Char], Expr)] -> Expr
getIdentifier ident expr = (getMaybe (lookup ident expr))