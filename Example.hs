module Expr where

import Parser
import Data.Monoid
import Control.Applicative
import Data.Maybe


exampleExpr :: Expr
exampleExpr = Operator Add (Operator Mul (Operator Add (Function Sin (X)) (Function Cos (X))) (X)) (Operator Mul (Num 3.14) (Function Sin X))


data Expr
  = Num Double
  | Operator Op Expr Expr
  | Function Fun Expr
  | X
 deriving ( Eq )
 
data Fun = Sin | Cos deriving Eq
data Op = Add | Mul deriving Eq

instance Show Fun where
    show Sin = "sin"
    show Cos = "cos"


instance Show Expr where
  show = showExpr

showExpr :: Expr -> String
showExpr (Num n)   = show n
showExpr (Operator Add a b) = showExpr a ++ " + " ++ showExpr b
showExpr (Operator Mul a b) = showFactor a ++ " * " ++ showFactor b
showExpr (Function f a) = show f ++ showOperand a
showExpr (X)   = "x"

showOperand :: Expr -> String
showOperand e@(Operator _ _ _) = "(" ++ showExpr e ++ ")"
showOperand e = " " ++ showExpr e

showFactor :: Expr -> String
showFactor e@(Operator Add _ _) = "(" ++ showExpr e ++")"
showFactor e         = showExpr e


eval :: Expr -> Double -> Double
eval (Num n) _   = n
eval (Operator op a b) val = (evalOp op) (eval a val) (eval b val)
eval (X) val = val
eval (Function f a) val = (evalFun f) (eval a val)

evalOp :: Op -> Double -> Double -> Double
evalOp Mul = (*)
evalOp Add = (+)

evalFun :: Fun -> Double -> Double
evalFun Sin = sin
evalFun Cos = cos


expr :: Parser Expr
expr = addition <|> term

addition :: Parser Expr
addition = do
    a <- term
    many whitespace
    token '+'
    many whitespace
    b <- expr
    return (Operator Add a b)

term :: Parser Expr
term = multiplication <|> factor


multiplication :: Parser Expr
multiplication = do
    a <- factor
    many whitespace
    token '*'
    many whitespace
    b <- term
    return (Operator Mul a b)

factor :: Parser Expr
factor = cosp <|> sinp <|> x <|> number <|> parens

sinp :: Parser Expr
sinp = do
    string "sin"
    many whitespace
    a <- factor
    return (Function Sin a)

cosp :: Parser Expr
cosp = do
    string "cos"
    many whitespace
    a <- factor
    return (Function Cos a)


x :: Parser Expr
x = fmap (\_ -> X) $ token 'x' <|> token 'X'

number :: Parser Expr
number = fmap Num float

parens :: Parser Expr
parens = do
    token '('
    many whitespace
    a <- expr
    many whitespace
    token ')'
    return a

readExpr :: String -> Maybe Expr
readExpr s = case parse expr s of
    (v, []) -> v
    _       -> Nothing


