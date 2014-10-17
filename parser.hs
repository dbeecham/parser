import Control.Monad

-- hej
type Parser a = String -> Maybe (a, String)

data Expr = Num Double | X | Bin BinOps Expr Expr | Un UnOps Expr deriving Show
data BinOps = Add | Mul | Sub | Div deriving Show
data UnOps = Sin | Cos deriving Show


spot :: (Char -> Bool) -> Parser Char
spot f (x:xs)
    | f x       = Just (x, xs)
    | otherwise = Nothing
spot _ []       = Nothing


find :: String -> Parser String
find str1 str2
    | take (length str1) str2 == str1 = Just (str1, drop (length str1) str2)
    | otherwise = Nothing


parseNum :: Parser Expr
parseNum xs = case reads xs of
    [(x, rest)] -> Just (Num x, rest)
    [] -> Nothing


parseX :: Parser Expr
parseX xs = do
    (token, rest) <- spot (== 'x') xs
    return (X, rest)


parseVal :: Parser Expr
parseVal = parseX <|> parseNum


(<|>) :: Parser a -> Parser a -> Parser a
p <|> q = \st -> mplus (p st) (q st)


chain1 :: (a -> a -> a) -> Char -> Parser a -> Parser a
chain1 op f p xs = do
    (a, rest) <- p xs
    (_, rest) <- spot (== f) rest
    (b, rest) <- (chain op f p <|> p) rest
    return (op a b, rest)


chain :: (a -> a -> a) -> Char -> Parser a -> Parser a
chain op f p = chain1 op f p <|> p


parseAdd, parseSub, parseMul, parseDiv :: Parser Expr
parseAdd = chain (Bin Add) '+' parseSub
parseSub = chain (Bin Sub) '-' parseMul
parseMul = chain (Bin Mul) '*' parseDiv
parseDiv = chain (Bin Div) '/' parseVal


parseUn :: UnOps -> String -> Parser Expr -> Parser Expr
parseUn op name p st = do
    (_, rest) <- find name st
    (a, rest) <- p rest
    return (Un op a, rest)


parseSin, parseCos :: Parser Expr
parseSin = parseUn Sin "sin" parseVal
parseCos = parseUn Cos "cos" parseVal

parseExpr :: Parser Expr
parseExpr = parseAdd
