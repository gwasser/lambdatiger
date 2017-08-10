{
--    Copyright (C) 2017, Garret Wassermann.
--
--    This file is part of tigerc, the Tiger language compiler,
--    based on the Tiger language in "Modern Compiler Implementation
--    in ML" by Andrew W. Appel.
--
--    tigerc is free software: you can redistribute it and/or modify
--    it under the terms of the GNU General Public License as published by
--    the Free Software Foundation, either version 3 of the License, or
--    (at your option) any later version.
--
--    tigerc is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY; without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--    GNU General Public License for more details.
--
--    You should have received a copy of the GNU General Public License
--    along with tigerc.  If not, see <http://www.gnu.org/licenses/>.

module Tiger.Lexical.Lexer (scanner, monadicLexer) where 

-- This implementation is partly based on the work of
-- Jyotirmoy Bhattacharya, documented in book "Alex and Happy" at
-- <https://www.jyotirmoy.net/posts/2015-08-17-alex-happy-startcodes.html>.
-- The example code was originally released on GitHub
-- under terms of the GNU GPLv3 license. See:
-- <https://github.com/jmoy/alexhappy/tree/master/startcode>.

import Tiger.Lexical.Tokens (Token(..))

import Control.Monad.State (State, get, put, runState, evalState, execState)
--import Control.Monad.Except
--import Control.Monad
import Codec.Binary.UTF8.String (encode)
import Data.Word (Word8)

-- This code doesn't use a wrapper, since it implements the basic
-- functions Alex requires directly at a low level
-- see <https://www.haskell.org/alex/doc/html/basic-api.html>
}

$digit = 0-9                -- digits
$alpha = [a-zA-Z]           -- alphabetic characters

tokens :-

  <0>$white+               ;
  
  <0>array                 { plainTok ARRAY }
  <0>break                 { plainTok BREAK }
  <0>do                    { plainTok DO }
  <0>else                  { plainTok ELSE }
  <0>end                   { plainTok END }
  <0>for                   { plainTok FOR }
  <0>function              { plainTok FUNCTION }
  <0>if                    { plainTok IF }
  <0>in                    { plainTok IN }
  <0>let                   { plainTok LET }
  <0>nil                   { plainTok NIL }
  <0>of                    { plainTok OF }
  <0>to                    { plainTok TO }
  <0>var                   { plainTok VAR }
  <0>then                  { plainTok THEN }
  <0>type                  { plainTok TYPE }
  <0>while                 { plainTok WHILE }
  
  <0>\+                    { plainTok PLUS }
  <0>\-                    { plainTok MINUS }
  <0>\*                    { plainTok STAR }
  <0>\/                    { plainTok SLASH }
  <0>\&                    { plainTok AMPERSAND }
  <0>\|                    { plainTok PIPE }
  
  <0>\:\=                  { plainTok DEFINE }
  
  <0>\=                    { plainTok EQUAL }
  <0>\<\>                  { plainTok NOTEQUAL }
  <0>\>\=                  { plainTok GREATEROREQUAL }
  <0>\<\=                  { plainTok LESSOREQUAL }
  <0>\>                    { plainTok GREATERTHAN }
  <0>\<                    { plainTok LESSTHAN }
  
  <0>\;                    { plainTok SEMICOLON }
  <0>\:                    { plainTok COLON }
  <0>\.                    { plainTok PERIOD }
  <0>\,                    { plainTok COMMA }
  <0>\{                    { plainTok LBRACE }
  <0>\}                    { plainTok RBRACE }
  <0>\(                    { plainTok LPAREN }
  <0>\)                    { plainTok RPAREN }
  <0>\[                    { plainTok LBRACKET }
  <0>\]                    { plainTok RBRACKET }
  
  <0>$digit+                       { textTok (\s -> NUM (read s :: Int)) }
  <0>$alpha [$alpha $digit \_]*    { textTok (\s -> ID s) }
  
  <0>\"                   { beginString }
  <stringSC>\"            { endString }
  <stringSC>.             { appendString }
  <stringSC>\\[nt\"]      { escapeString }
  
  <0,commentSC>"/*"       { beginComment }
  <commentSC>"*/"         { endComment }
  <commentSC>[.\n]        ;

{
-- happy expects lexer to be a continuation,
-- for historical reasons
monadicLexer :: (Token -> Lex a) -> Lex a
monadicLexer cont = readToken >>= cont
  
--scanner :: String -> Token
-- evalLex str $ readToken = evalState m (initialState s)
scanner str = tokenList str
  where tokenList s = do
                 let nextTok = runState (readToken) (initialState s)
                 case fst $ nextTok of
                         TEOF -> [TEOF]
                         t -> t : tokenList (airest $ input $ snd nextTok)
        
  
--
-- LEXER STATES AND TYPES
--

-- actions (functions in braces above) have following type;
-- according to manual, alex doesn't care what the type of
-- these functions are, as long as they are consistently defined.
type LexAction = Int -> String -> Lex (Maybe Token)
-- NOTE: because of currying, this effectively adds two more
-- parameters to any function that yields a LexAction,
-- as seen in many of the functions below.

-- Define a lexer state to track comments and strings
data LexerState = 
     LexerState {input :: AlexInput,
                 lexSC :: Int,       --Lexer start code
                 commentDepth :: Int,--Comment depth
                 stringBuf :: String --Temporary storage for strings
                }
     deriving Show

-- Generate initial lexer state from an input String
initialState :: String -> LexerState
initialState s = LexerState {   input = AlexInput {aiprev='\n',
                                                   aibytes=[],
                                                   airest=s,
                                                   aipos = (1,1)},
                                lexSC = 0,
                                commentDepth = 0,
                                stringBuf = ""
                                }

-- Our Lexer monad
type Lex a = State LexerState a

--
-- FUNCTIONS TO READ TOKENS
--

-- monadic wrapper for ordinary tokens
plainTok :: Token -> LexAction
plainTok t _ _ = return (Just t)

-- monadic wrapper for tokens that hold some type of input
-- (like a string or an integer), uses a function
-- to convert input String to appropriate type
textTok :: (String -> Token) -> LexAction
textTok cons _ s = return $ Just (cons s)


beginString :: LexAction
beginString _ _ = do
  s <- get
  put s{lexSC = stringSC}
  return Nothing

appendString :: LexAction
appendString _ (c:_) = do
  s <- get
  put s{stringBuf = c:(stringBuf s)}
  return Nothing
  
escapeString :: LexAction
escapeString _ (_:c:_) = do
  let unesc =
        case c of
          'n' -> '\n'
          't' -> '\t'
          '"' -> '"'
  s <- get
  put s{stringBuf = unesc:(stringBuf s)}
  return Nothing
        
endString :: LexAction
endString _ _ = do
  s <- get
  let buf = stringBuf s
  put s{lexSC = 0, stringBuf = ""}
  return $ Just $ STR (reverse buf)


beginComment :: LexAction
beginComment _ _ = do
  s <- get
  put s {lexSC = commentSC,
         commentDepth = (commentDepth s)+1}
  return Nothing
  
endComment :: LexAction
endComment _ _ = do
  s <- get
  let cd = commentDepth s
  let sc' = if cd==1 then 0 else commentSC
  put s {lexSC=sc',commentDepth=cd-1}
  return Nothing


readToken :: Lex Token
readToken = do
  s <- get
  case alexScan (input s) (lexSC s) of
    AlexEOF -> return TEOF
    AlexError inp' -> error $ "Lexical error at position " ++ (show $ aipos inp')      
    AlexSkip inp' _ -> do    
      put s{input = inp'}
      readToken
    AlexToken inp' n act -> do 
      let (AlexInput{airest=buf}) = input s
      put s{input = inp'}
      res <- act n (take n buf)
      case res of
        Nothing -> readToken
        Just t -> return t


getLineNo :: Lex Int
getLineNo = do
  s <- get
  return . fst . aipos . input $ s
  
getColNo :: Lex Int
getColNo = do
  s <- get
  return . snd . aipos . input $ s

-- take input and a Lex Token, return a Token
evalLex :: String -> Lex a -> a
evalLex s m = evalState m (initialState s)

--
-- ALEX-REQUIRED TYPES AND FUNCTIONS
-- These functions that must be provided to Alex's basic interface
-- (see <https://www.haskell.org/alex/doc/html/basic-api.html>)
--

-- The input: last character, unused bytes, remaining string
data AlexInput = AlexInput {
    aiprev::Char,
    aibytes::[Word8],
    airest::String,
    aipos::(Int,Int)} -- (row, col) of position of lexer
    deriving Show

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte ai
  = case (aibytes ai) of
    (b:bs) -> Just (b, ai{aibytes=bs})
    [] -> case (airest ai) of
      [] -> Nothing
      (c:cs) -> let (m,n) = (aipos ai)
                    newpos = if c=='\n' then (m+1,0) else (m,n+1)
                    (b:bs) = encode [c] in
                Just (b,AlexInput {aiprev=c,
                                   aibytes=bs,
                                   airest=cs,
                                   aipos=newpos})

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (AlexInput {aiprev=c}) = c

}
