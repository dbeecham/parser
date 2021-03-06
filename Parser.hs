module Parser where

import Control.Applicative
import Data.Monoid
import Data.Char

(&) :: a -> (a -> b) -> b
(&) = flip ($)

newtype Parser a = Parser { parse :: String -> (Maybe a, String) }

instance Functor Parser where
    fmap f a = Parser (\x -> let (res, rest) = parse a x in
                        (fmap f res, rest))

-- This monoid instance has 'case' on both parsers (p <> q) so that if
-- one parser fails to parse, the joined parser does not "eat up" the string.
instance (Monoid a) => Monoid (Parser a) where
    mempty = Parser (\str -> (Just mempty, str))
    mappend p q = Parser (\str -> case parse p str of
                        (Just a, rest) -> case parse q rest of
                            (Just b, rest') -> (Just (a <> b), rest') 
                            (Nothing, _) -> (Nothing, str)
                        (Nothing, _) -> (Nothing, str))


instance Alternative Parser where
    empty = Parser (\str -> (Nothing, str))
    p <|> q = Parser (choice' p q)
        where choice' p q str = case parse p str of
                (Just v, rest) -> (Just v, rest)
                (Nothing, _) -> parse q str
        
-- Applicative instance is not used directly, but is required for Parser to be
-- a Monad in newer versions of GHC.
instance Applicative Parser where
    f <*> p = Parser (\str -> let (g, rest) = parse f str 
                                  (c, rest') = parse p rest in
                                (g <*> c, rest'))
                              
    pure x = (Parser (\str -> (Just x, str)))

-- The same argument as for monoid applies here as well; 'cases' on both parsers
-- so that joint parsing is atomic.
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

string :: String -> Parser String
string str = fmap token str &
             (fmap.fmap) tolist &
             mconcat
    where tolist x = [x]

digit :: Parser Char
digit = anyof "0123456789"

whitespace :: Parser Char
whitespace = anyof " \n\t"

integer :: Parser Integer
integer = fmap read (some digit)


float :: Parser Double
float = Parser f where
    f str = case reads str of
        [] -> (Nothing, str)
        (x,rest):_ -> (Just x, rest)

