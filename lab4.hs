import Parser
import Data.Monoid
import Control.Applicative

data Expr =   Number Double
            | X
            | Add Expr Expr
            | Mul Expr Expr
            | Sin Expr
            | Cos Expr

instance Show Expr where
    show (Number x) = show x
    show (X) = "x"
    show (Add a b) = "(" ++ show a ++ " + " ++ show b ++ ")"
    show (Mul a b) = show a ++ " * " ++ show b
    show (Sin a) = "sin " ++ show a
    show (Cos a) = "cos " ++ show a

exampleExpr :: Expr
exampleExpr = Add (Mul (Cos X) 
                       (Add (Number 3)
                            (Number 8))) 
                  (Sin X)

eval :: Expr -> Double -> Double
eval (Number x) _ = x
eval (X) x = x
eval (Add a b) x = (eval a x) + (eval b x)
eval (Mul a b) x = (eval a x) * (eval b x)
eval (Sin a) x = sin (eval a x)
eval (Cos a) x = cos (eval a x)


-- PARSING

expr :: Parser Expr
expr = addition <|> term

addition :: Parser Expr
addition = do
    a <- term
    many whitespace
    token '+'
    many whitespace
    b <- expr
    return (Add a b)

term :: Parser Expr
term = multiplication <|> factor

multiplication :: Parser Expr
multiplication = do
    a <- factor
    many whitespace
    token '*'
    many whitespace
    b <- factor
    return (Mul a b)

factor :: Parser Expr
factor = cos <|> sin <|> x <|> number <|> parens

sin :: Parser Expr
sin = undefined




x :: Parser Expr
x = fmap (\_ -> X) $ token 'x' <|> token 'X'

number :: Parser Expr
number = fmap Number float

parens :: Parser Expr
parens = do
    token '('
    many whitespace
    a <- expr
    many whitespace
    token ')'
    return a

readExpr :: String -> Maybe Expr
readExpr = undefined
