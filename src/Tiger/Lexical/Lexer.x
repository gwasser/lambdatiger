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

module Tiger.Lexical.Lexer (alexMonadScanTokens, alexMonadScanTokensWithMeta) where 

-- This implementation is partly based on the work of
-- Jyotirmoy Bhattacharya, documented in book "Alex and Happy" at
-- <https://www.jyotirmoy.net/posts/2015-08-17-alex-happy-startcodes.html>.
-- The example code was originally released on GitHub
-- under terms of the GNU GPLv3 license. See:
-- <https://github.com/jmoy/alexhappy/tree/master/startcode>.

import Tiger.Lexical.Tokens (Token(..), LexicalToken, TokenMeta(..))

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
  <stringSC>\\[nt\"\\]    { escapeString }
  <stringSC>[.\n]         { appendString }
  
  <0,commentSC>"/*"       { beginComment }
  <commentSC>"*/"         { endComment }
  <commentSC>[.\n]        ;

{
-- happy in monad mode expects to provide lexer with a 
-- continuation for historical reasons.
-- Unneeded unless using monadic lexer mode.
-- See <https://www.haskell.org/happy/doc/html/sec-monads.html>.
--monadicLexer :: (LexicalToken -> Lex a) -> Lex a
--monadicLexer cont = readToken >>= cont

-- |Strip away metadata from the lexer output, useful for
-- simple unit testing that doesn't need the meta.
alexMonadScanTokens :: String -> [Token]
alexMonadScanTokens str = map (fst) $ alexMonadScanTokensWithMeta str
  
alexMonadScanTokensWithMeta :: String -> [LexicalToken]
alexMonadScanTokensWithMeta str = tokenList (initialState str)
  where tokenList lexst = do
                 let nextTok = runState (readToken) lexst
                 case fst $ nextTok of
                         (TEOF, _) -> [(TEOF, Nothing)]
                         t -> t : tokenList (snd nextTok)
        
  
--
-- LEXER STATES AND TYPES
--

-- actions (functions in braces above) have following type;
-- according to manual, alex doesn't care what the type of
-- these functions are, as long as they are consistently defined.
type LexAction = Int -- the state of the lexer (starts at 0)
               -> String -- processed input
               -> Lex (Maybe Token)
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
initialState s = LexerState {   input = AlexInput {aiprev='\0',
                                                   aibytes=[],
                                                   airest=s,
                                                   aipos = TokenMeta {row=1,col=1}},
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
          '\\' -> '\\'
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


readToken :: Lex LexicalToken
readToken = do
  s <- get
  case alexScan (input s) (lexSC s) of
    AlexEOF -> return (TEOF, Nothing)
    AlexError inp' -> error $ "Lexical error at position " ++ (show $ aipos inp') ++ " : <<" ++ (show $ airest inp') ++ ">>"
    AlexSkip inp' _ -> do    
      put s{input = inp'}
      readToken
    AlexToken inp' n act -> do 
      let (AlexInput{airest=buf, aipos=pos}) = input s
      put s{input = inp'}
      res <- act n (take n buf)
      case res of
        Nothing -> readToken
        Just t -> return (t, Just pos)

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
    aipos::TokenMeta} -- (row, col) of position of lexer
    deriving Show

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte ai
  = case (aibytes ai) of
    (b:bs) -> Just (b, ai{aibytes=bs})
    [] -> case (airest ai) of
      [] -> Nothing
      (c:cs) -> let m = row $ aipos ai
                    n = col $ aipos ai
                    newpos = if ((aiprev ai)=='\n')
                             then TokenMeta {row=m+1,col=1}
                             else TokenMeta {row=m, col=n+1}
                    (b:bs) = encode [c] in
                Just (b,AlexInput {aiprev=c,
                                   aibytes=bs,
                                   airest=cs,
                                   aipos=newpos})

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (AlexInput {aiprev=c}) = c

}
