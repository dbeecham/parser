import Parser

data Expr = Number Double | X | Operator Op Expr Expr | Function Fun Expr
data Op = Add | Mul
data Fun = Sin | Cos

instance Show Op where
    show Add = "+"
    show Mul = "*"

instance Show Fun where
    show Sin = "sin"
    show Cos = "cos"

instance Show Expr where
    show (Number x) = show x
    show (X) = "x"
    show (Operator Add a b) = "(" ++ show a ++ " + " ++ show b ++ ")"
    show (Operator Mul a b) = show a ++ " * " ++ show b
    show (Function f a) = show f ++ "(" ++ show a ++ ")"

exampleExpr :: Expr
exampleExpr = Operator Add (Operator Mul (Function Cos X) 
                                         (Operator Add (Number 3)
                                                       (Number 8))) 
                           (Function Sin X)



eval :: Expr -> Double -> Double
eval (Number x) _ = x
eval (X) x = x
eval (Operator Add a b) x = (eval a x) + (eval b x)
eval (Operator Mul a b) x = (eval a x) * (eval b x)
eval (Function Sin a) x = sin (eval a x)
eval (Function Cos a) x = cos (eval a x)

parseNumber :: Parser Expr
parseNumber = fmap Number float

parseX :: Parser Expr
parseX = fmap (\_ -> X) (token 'x' <|> token 'X')

parseOperator :: Parser Expr
parseOperator = do
    

readExpr :: String -> Maybe Expr
readExpr = undefined
