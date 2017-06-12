{-
    Copyright (C) 2017, Garret Wassermann.

    This file is part of tigerc, the Tiger language compiler,
    based on the Tiger language in "Modern Compiler Implementation
    in ML" by Andrew W. Appel.

    tigerc is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    tigerc is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with tigerc.  If not, see <http://www.gnu.org/licenses/>.
-}


{- Based on the NanoParsec parser in "Write You a Haskell" by Stephen Diehl -}
module Tiger.Lexer.NanoLexer where

import Data.Char
import Control.Monad
import Control.Applicative (Alternative(..), many, some)

{- BEGIN CORE LIBRARY FUNCTIONS -}
    
-- a Parser reads character stream, returning a token of
-- type a, and the rest of the unparsed stream
newtype Parser a = Parser { parse :: String -> [(a, String)] }

-- takes a Parser and a String and applies the parse
-- function to recover the parsed value, or error
runParser :: Parser a -> String -> a
runParser m s = 
    case parse m s of
         [(res, [])] -> res
         [(_, rs)]   -> error "Parser did not consume entire stream."
         _           -> error "Parser error"
         
-- advances the stream one character at a time
item :: Parser Char
item = Parser $ \s ->
    case s of
         []     -> []
         (c:cs) -> [(c, cs)]
         
-- combines effects of two parsers sequentially, by
-- first applying Parser a to the input stream,, then
-- applying the second Parse b parse function to the result,
-- before concating the results and returning
bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s

-- injects a pure result without reading the parse stream
unit :: a -> Parser a
unit a = Parser (\s -> [(a,s)])

-- Parser is a Monad, and therefore also a Functor and Applicative
instance Monad Parser where
    return = unit
    (>>=)  = bind
    
instance Applicative Parser where
    pure = return
    (Parser cs1) <*> (Parser cs2) = Parser (\s -> [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1])
    
instance Functor Parser where
    fmap f (Parser cs) = Parser (\s -> [(f a, b) | (a, b) <- cs s])
    
-- combine results from two parsers by applying
-- two different parsers to the same input stream
combine :: Parser a -> Parser a -> Parser a
combine p q = Parser (\s -> parse p s ++ parse q s)

-- halts reading input stream and returns empty stream
failure :: Parser a
failure = Parser (\cs -> [])


-- combine and failure form a monoidal structure,
-- where combine is like the plus operator and
-- failure acts like the identity (0)
instance MonadPlus Parser where
    mzero = failure
    mplus = combine

-- option tries one parser and returns that Parse result
-- if successful, otherwise returns result of the
-- second parser if the first fails
option :: Parser a -> Parser a -> Parser a
option p q = Parser $ \s ->
    case parse p s of
         []     -> parse q s
         res    -> res
         
instance Alternative Parser where
    empty = mzero
    (<|>) = option
        
        
-- NOTE: many is defined in Control.Applicative
-- repeatedly applies function until it fails,
-- then returns list of results to that point
--many :: f a -> f [a]
--many v = many_v
--    where 
--        many_v = some_v <|> pure []
--        some_v = (:) <$> v <*> many_v
      
-- NOTE: some is defined in Control.Applicative
-- repeatedly applies function until it fails,
-- then returns list of results to that point,
-- except that it fails if not at least one match
--some :: f a -> f [a]
--some v = some_v
--    where 
--        many_v = some_v <|> pure []
--        some_v = (:) <$> v <*> many_v
        
-- check if current character from input stream
-- matches a given predicate p, otherwise fail
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item `bind` \c ->
    if p c
       then unit c
       else (Parser (\cs -> []))
       
{- BEGIN HIGHER LEVEL LIBRARY FUNCTIONS -}

-- takes in a String to check against,
-- and when applied to a single character,
-- returns a Parser based on whether character
-- is an element of the previously applied String.
oneOf :: [Char] -> Parser Char
oneOf s = satisfy (flip elem s)

chainlp :: Parser a -> Parser (a -> a) -> a -> Parser a
chainlp p op a = (p `chainlp1` op) <|> return a

chainlp1 :: Parser a -> Parser (a -> a) -> Parser a
p `chainlp1` op = do {a <- p; rest a}
                    where rest a = (do f <- op
                                       rest (f a))
                                   <|> return a

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) <|> return a

-- parse one or more occurances of p, seperated by op,
-- and returns value obtained by recursing until failure on left-hand
-- side of input stream.
-- Useful for parsing left-recursive grammars!
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do {a <- p; rest a}
                    where rest a = (do f <- op
                                       b <- p
                                       rest (f a b))
                                   <|> return a
                                   
char :: Char -> Parser Char
char c = satisfy (c ==)

digit :: Parser Char
digit = satisfy isDigit

natural :: Parser Integer
natural = read <$> some (satisfy isDigit)

string :: String -> Parser String
string [] = return []
string (c:cs) = do { char c; string cs; return (c:cs)}
                   
-- checks if negative sign in front, reads some
-- characters as long as they digits, then returns the
-- digits concatenated and read into an Int value
number :: Parser Int
number = do
    s <- string "-" <|> return []
    cs <- some digit
    return $ read (s ++ cs)
                   
spaces :: Parser String
spaces = many $ oneOf " \n\r"
                   
token :: Parser a -> Parser a
token p = do { a <- p; spaces; return a}
             
-- checks that given String is a substring of input stream,
-- and returns that token if so
reserved :: String -> Parser String
reserved s = token (string s)

parens :: Parser a -> Parser a
parens m = do
    reserved "("
    n <- m
    reserved ")"
    return n

prefixOp :: String -> (a -> a) -> Parser (a -> a)
prefixOp x f = reserved x >> return f

infixOp :: String -> (a -> a -> a) -> Parser (a -> a -> a)
infixOp x f = reserved x >> return f


-- does nothing, placeholder until compiler works
someParseFunc :: IO ()
someParseFunc = putStrLn "someParseFunc"
