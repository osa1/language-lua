-- TODO:
-- * Multi-line comments and strings

{
module Language.Lua.Lexer
  ( llex
  , LTok
  , AlexPosn(..)
  ) where

import Language.Lua.Token
import Control.Applicative ((<$>))
import Control.Monad (forM_, unless)
}

%wrapper "monadUserState"

$space = [ \ \t ]                        -- horizontal white space
$eol   = \n                              -- end of line

$letter      = [a-zA-Z_]                 -- first letter of variables
$identletter = [a-zA-Z_0-9]              -- letters for rest of variables

$digit    = 0-9                          -- decimal digits
$octdigit = 0-7                          -- octal digits
$hexdigit = [0-9a-fA-F]                  -- hexadecimal digits

$dqstr    = \0-\255 # [ \\ \" \n ]       -- valid character in a string literal with dquotes
$sqstr    = \0-\255 # [ \\ \' \n ]       -- valid character in a string literal with quotes
$longstr  = \0-\255                      -- valid character in a long string
$anyButNL = \0-\255 # \n

@sp = $space*

-- escape characters
@charescd  = \\ ([ntvbrfaeE\\\?\"] | $octdigit{1,3} | x$hexdigit+ | X$hexdigit+)
@charescs  = \\ ([ntvbrfaeE\\\?\'] | $octdigit{1,3} | x$hexdigit+ | X$hexdigit+)

@digits    = $digit+
@hexdigits = $hexdigit+

@mantpart = (@digits \. @digits) | @digits \. | \. @digits
@exppart  = [eE][\+\-]? @digits

@hexprefix = 0x | 0X
@mantparthex = (@hexdigits \. @hexdigits) | @hexdigits \. | \. @hexdigits
@expparthex  = [pP][\+\-]? @hexdigits

tokens :-

    <0> $white+  ;
    <0> "--" [^\n]* ;

    <0> $letter $identletter* { ident }

    <0> @digits                              { tokWValue LTokNum }
    <0> @digits @exppart                     { tokWValue LTokNum }
    <0> @mantpart @exppart?                  { tokWValue LTokNum }
    <0> @hexprefix @hexdigits                { tokWValue LTokNum }
    <0> @hexprefix @hexdigits @expparthex    { tokWValue LTokNum }
    <0> @hexprefix @mantparthex @expparthex? { tokWValue LTokNum }

    <0> \"($dqstr|@charescd)*\" { \(posn,_,_,s) _ -> return (LTokSLit (tail . init $ s), Right posn) }
    <0> \'($sqstr|@charescs)*\' { \(posn,_,_,s) _ -> return (LTokSLit (tail . init $ s), Right posn) }

    -- long strings
    <0> \[ \=* \[            { enterString `andBegin` state_string }
    <state_string> \] \=* \] { testAndEndString }
    <state_string> $longstr  { addCharToString }

    <0> "+"   { tok LTokPlus }
    <0> "-"   { tok LTokMinus}
    <0> "*"   { tok LTokStar}
    <0> "/"   { tok LTokSlash }
    <0> "%"   { tok LTokPercent }
    <0> "^"   { tok LTokExp }
    <0> "#"   { tok LTokSh }
    <0> "=="  { tok LTokEqual }
    <0> "~="  { tok LTokNotequal }
    <0> "<="  { tok LTokLEq }
    <0> ">="  { tok LTokGEq }
    <0> "<"   { tok LTokLT }
    <0> ">"   { tok LTokGT }
    <0> "="   { tok LTokAssign }
    <0> "("   { tok LTokLParen }
    <0> ")"   { tok LTokRParen }
    <0> "{"   { tok LTokLBrace }
    <0> "}"   { tok LTokRBrace }
    <0> "["   { tok LTokLBracket }
    <0> "]"   { tok LTokRBracket }
    <0> "::"  { tok LTokDColon }
    <0> ";"   { tok LTokSemic }
    <0> ":"   { tok LTokColon }
    <0> ","   { tok LTokComma }
    <0> "."   { tok LTokDot }
    <0> ".."  { tok LTokDDot }
    <0> "..." { tok LTokEllipsis }

    \EOF  { tok LTokEof }

{

data AlexUserState = AlexUserState { stringState     :: !Bool
                                   , stringDelimLen  :: !Int
                                   , stringPosn      :: !AlexPosn
                                   , stringValue     :: !String
                                   } deriving Show

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState { stringState     = False
                                  , stringDelimLen  = 0
                                  , stringPosn      = AlexPn 1 2 3
                                  , stringValue     = ""
                                  }

initString :: Int -> AlexPosn -> Alex ()
initString i posn = Alex $ \s -> Right(s{alex_ust=(alex_ust s){stringState=True,stringValue="",stringDelimLen=i,stringPosn=posn}}, ())

getStringDelimLen :: Alex Int
getStringDelimLen = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, stringDelimLen ust)

getStringPosn :: Alex AlexPosn
getStringPosn = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, stringPosn ust)

getStringValue :: Alex String
getStringValue = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, stringValue ust)

getStringState :: Alex Bool
getStringState = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, stringState ust)

addCharToStringValue :: Char -> Alex ()
addCharToStringValue c = Alex $ \s -> Right (s{alex_ust=(alex_ust s){stringValue=c:stringValue (alex_ust s)}}, ())

enterString :: AlexAction LTok
enterString (posn,_,_,_) len = do
  initString len posn
  alexMonadScan

addString :: AlexAction LTok
addString (_,_,_,s) len = do
  forM_ (take len s) addCharToStringValue
  alexMonadScan

addCharToString :: AlexAction LTok
addCharToString (_,_,_,s) len = do
  addCharToStringValue (head s)
  alexMonadScan

endString :: Alex ()
endString = Alex $ \s -> Right(s{alex_ust=(alex_ust s){stringState=False}}, ())

testAndEndString ::AlexAction LTok
testAndEndString (_,_,_,s) len = do
  startlen <- getStringDelimLen
  if startlen /= len
    then do forM_ (take len s) addCharToStringValue
            alexMonadScan
    else do endString
            val <- getStringValue
            posn <- getStringPosn
            alexSetStartCode 0
            return (LTokSLit (reverse val), Right posn)

data EOF = EOF deriving Show

-- | Lua token with position information.
type LTok = (LToken, Either EOF AlexPosn)

-- type AlexAction result = AlexInput -> Int -> Alex result

-- Helper to make LTokens with string value (like LTokNum, LTokSLit etc.)
tokWValue :: (String -> LToken) -> AlexInput -> Int -> Alex LTok
tokWValue tok (posn,_,_,s) len = return (tok (take len s), Right posn)

tok :: LToken -> AlexInput -> Int -> Alex LTok
tok t (posn,_,_,_) _ = return (t, Right posn)

{-# INLINE ident #-}
ident :: AlexAction LTok
ident (posn,_,_,s) len = return (tok, Right posn)
  where tok = case (take len s) of
          "and"      -> LTokAnd
          "break"    -> LTokBreak
          "do"       -> LTokDo
          "else"     -> LTokElse
          "elseif"   -> LTokElseIf
          "end"      -> LTokEnd
          "false"    -> LTokFalse
          "for"      -> LTokFor
          "function" -> LTokFunction
          "goto"     -> LTokGoto
          "if"       -> LTokIf
          "in"       -> LTokIn
          "local"    -> LTokLocal
          "nil"      -> LTokNil
          "not"      -> LTokNot
          "or"       -> LTokOr
          "repeat"   -> LTokRepeat
          "return"   -> LTokReturn
          "then"     -> LTokThen
          "true"     -> LTokTrue
          "until"    -> LTokUntil
          "while"    -> LTokWhile
          ident'     -> LTokIdent ident'

--data AlexPosn = AlexPn !Int  -- absolute character offset
--                       !Int  -- line number
--                       !Int  -- column number
--
--type AlexInput = (AlexPosn,     -- current position,
--                  Char,         -- previous char
--                  [Byte],       -- rest of the bytes for the current char
--                  String)       -- current input string

alexEOF :: Alex LTok
alexEOF = return (LTokEof, Left EOF)

scanner :: String -> Either String [LTok]
scanner str = runAlex str loop
  where loop = do
          t@(tok, _) <- alexMonadScan
          if tok == LTokEof
            then do stringState <- getStringState
                    if stringState
                      then alexError "String not closed at end of file"
                      else return [t]
            else do toks <- loop
                    return (t:toks)

-- | Lua lexer.
llex :: String -> [LTok]
llex s = case scanner s of
           Left err -> error err
           Right r  -> r
}
