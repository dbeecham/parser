import Control.Monad

type Parser a = String -> Maybe (a, String)

data Expr = Num Double | X | Y | Add Expr Expr deriving Show

spot :: (Char -> Bool) -> Parser Char
spot l (x:xs)
    | l x    = Just (x, xs)
    | otherwise = Nothing
spot _ [] = Nothing

parseNum :: Parser Expr
parseNum xs = case reads xs of
    [(x, rest)] -> Just (Num x, rest)
    [] -> Nothing

parseX :: Parser Expr
parseX xs = do
    (token, rest) <- spot (== 'x') xs
    return (X, rest)

parseY :: Parser Expr
parseY xs = do
    (token, rest) <- spot (== 'y') xs
    return (Y, rest)

(<|>) :: Parser a -> Parser a -> Parser a
p <|> q = \st -> mplus (p st) (q st)

parseXY :: Parser Expr
parseXY = parseX <|> parseY

parseAdd :: Parser Expr
parseAdd xs = do
    (a, rest) <- parseXY xs
    (_, rest) <- spot (== '+') rest
    (b, rest) <- (parseAdd <|> parseXY) rest
    return (Add a b, rest)


parseExpr = parseAdd <|> parseXY <|> parseNum
