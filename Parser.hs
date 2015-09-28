module Parser where

import Control.Applicative
import Data.Char

newtype Parser a = Parser { parse :: String -> (Maybe a, String) }

instance Functor Parser where
    fmap f a = Parser (\x -> let (res, rest) = parse a x in
                        (fmap f res, rest))

instance Monoid (Parser a) where
    mempty = Parser (\str -> (Nothing, str))
    p <> q = Parser (\str -> case parse p str of
                        (Just a, rest) -> case parse q rest of
                            (Just b, rest') -> (Just b, rest') -- TODO
                            (Nothing, _) -> (Nothing, 

    p >>= f = Parser (\str -> case parse p str of
                        (Just a, rest) -> case parse (f a) rest of
                            (Just b, rest') -> (Just b, rest')
                            (Nothing, _) -> (Nothing, str)
                        (Nothing, _) -> (Nothing, str))

instance Alternative Parser where
    empty = Parser (\str -> (Nothing, str))
    p <|> q = Parser (choice' p q)
        where choice' p q str = case parse p str of
                (Just v, rest) -> (Just v, rest)
                (Nothing, _) -> parse q str
        

instance Applicative Parser where
    f <*> p = Parser (\str -> let (g, rest) = parse f str 
                                  (c, rest') = parse p rest in
                                (g <*> c, rest'))
                              
    pure x = (Parser (\str -> (Just x, str)))

instance Monad Parser where
    p >>= f = Parser (\str -> case parse p str of
                        (Just a, rest) -> case parse (f a) rest of
                            (Just b, rest') -> (Just b, rest')
                            (Nothing, _) -> (Nothing, str)
                        (Nothing, _) -> (Nothing, str))
    return = pure


anything :: Parser Char
anything = Parser any'
    where any' [] = (Nothing, [])
          any' (c:rest) = (Just c, rest)

token :: Char -> Parser Char
token c = Parser (token' c)
    where token' _ [] = (Nothing, [])
          token' t (c:rest) | t == c = (Just c, rest)
                            | otherwise = (Nothing, c:rest)

anyofp :: [Parser a] -> Parser a
anyofp [] = empty
anyofp [x] = x
anyofp (x:xs) = x <|> (anyofp xs)

anyof :: String -> Parser Char
anyof str = anyofp (map token str)

char :: Parser Char
char = anyof "abcdefghijklmnopqrstuvwzyxABCDEFGHIJKLMNOPQRSTUVWXYZ"

digit :: Parser Char
digit = anyof "0123456789"

integer :: Parser Integer
integer = fmap read (some digit)

expdotDouble :: Parser Double
expdotDouble = do
    d0 <- some digit
    token '.'
    d1 <- some digit
    token 'e'
    d2 <- some digit
    return $ read $ d0 ++ "." ++ d1 ++ "e" ++ d2

expDouble :: Parser Double
expDouble = do
    d0 <- some digit
    token 'e'
    d1 <- some digit
    return $ read $ d0 ++ "e" ++ d1

dotDouble :: Parser Double
dotDouble = do
    d0 <- some digit
    token '.'
    d1 <- some digit
    return $ read $ d0 ++ "." ++ d1

simpleDouble :: Parser Double
simpleDouble = do
    d0 <- some digit
    return (read d0)

float :: Parser Double
float = expdotDouble <|> expDouble <|> dotDouble <|> simpleDouble 

test :: Parser String
test = do
    a <- token 'a'
    b <- token 'b'
    c <- anything
    return [a, b, c]

