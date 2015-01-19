{
{-# OPTIONS_GHC -w #-}

module Language.Lua.Annotated.Lexer
  ( llex
  , llexFile
  , LTok
  , AlexPosn(..)
  ) where

import Language.Lua.Annotated.Syntax
import Language.Lua.Token
import Control.Applicative ((<$>))
import Control.Monad (forM_, unless, when)
import Data.Char (isNumber)
import Safe (readMay)
}

%wrapper "monadUserState"

$space = [ \ \t ]                        -- horizontal white space

$letter      = [a-zA-Z_]                 -- first letter of variables
$identletter = [a-zA-Z_0-9]              -- letters for rest of variables

$digit    = 0-9                          -- decimal digits
$hexdigit = [0-9a-fA-F]                  -- hexadecimal digits

$dqstr    = \0-\255 # [ \" \n \\ ]       -- valid character in a string literal with dquotes
$sqstr    = \0-\255 # [ \' \n \\ ]       -- valid character in a string literal with quotes
$longstr  = \0-\255                      -- valid character in a long string

-- escape characters
@charescd  = \\ ([ntvbrfa\\\?\"] | $digit{1,3} | x$hexdigit{2} | \n)
@charescs  = \\ ([ntvbrfa\\\?\'] | $digit{1,3} | x$hexdigit{2} | \n)

@digits    = $digit+
@hexdigits = $hexdigit+

@mantpart = (@digits \. @digits) | @digits \. | \. @digits
@exppart  = [eE][\+\-]? @digits

@hexprefix   = 0x | 0X
@mantparthex = (@hexdigits \. @hexdigits) | @hexdigits \. | \. @hexdigits
@expparthex  = [pP][\+\-]? @hexdigits

tokens :-

    <0> $white+  ;

    <0> $letter $identletter* { ident }

    <0> @digits                              { tokWValue LTokNum }
    <0> @digits @exppart                     { tokWValue LTokNum }
    <0> @mantpart @exppart?                  { tokWValue LTokNum }
    <0> @hexprefix @hexdigits                { tokWValue LTokNum }
    <0> @hexprefix @hexdigits @expparthex    { tokWValue LTokNum }
    <0> @hexprefix @mantparthex @expparthex? { tokWValue LTokNum }

    <0> \"($dqstr|@charescd)*\" { \(posn,_,_,s) l -> return $ mkString True  s l posn }
    <0> \'($sqstr|@charescs)*\' { \(posn,_,_,s) l -> return $ mkString False s l posn }

    -- long strings
    <0> \[ \=* \[ \n?        { enterString `andBegin` state_string }
    <state_string> \] \=* \] { testAndEndString }
    <state_string> $longstr  { addCharToString }

    <0> "--"                      { enterComment `andBegin` state_comment }
    <state_comment> . # \n        ;
    <state_comment> \n            { testAndEndComment }
    <state_comment> \[ \=* \[ \n? { enterString `andBegin` state_string }

    <0> "+"   { tok LTokPlus }
    <0> "-"   { tok LTokMinus }
    <0> "*"   { tok LTokStar }
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

{

data AlexUserState = AlexUserState { stringState     :: !Bool
                                   , stringDelimLen  :: !Int
                                   , stringPosn      :: !AlexPosn
                                   , stringValue     :: !String
                                   -- comments
                                   , commentState    :: !Bool
                                   }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState { stringState     = False
                                  , stringDelimLen  = 0
                                  , stringPosn      = AlexPn 0 0 0
                                  , stringValue     = ""
                                  , commentState    = False
                                  }

initString :: Int -> AlexPosn -> Alex ()
initString i posn = Alex $ \s -> Right(s{alex_ust=(alex_ust s){stringState=True,stringValue="",stringDelimLen=i,stringPosn=posn}}, ())

initComment :: Alex ()
initComment = Alex $ \s -> Right(s{alex_ust=(alex_ust s){commentState=True}}, ())

getStringDelimLen :: Alex Int
getStringDelimLen = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, stringDelimLen ust)

getStringPosn :: Alex AlexPosn
getStringPosn = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, stringPosn ust)

getStringValue :: Alex String
getStringValue = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, stringValue ust)

getStringState :: Alex Bool
getStringState = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, stringState ust)

getCommentState :: Alex Bool
getCommentState = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, commentState ust)

addCharToStringValue :: Char -> Alex ()
addCharToStringValue c = Alex $ \s -> Right (s{alex_ust=(alex_ust s){stringValue=c:stringValue (alex_ust s)}}, ())

enterString :: AlexAction LTok
enterString (posn,_,_,s) len = do
  initString (if (s !! (len-1) == '\n') then len-1 else len) posn
  alexMonadScan'

enterComment :: AlexAction LTok
enterComment _ _ = do
  initComment
  alexMonadScan'

addString :: AlexAction LTok
addString (_,_,_,s) len = do
  forM_ (take len s) addCharToStringValue
  alexMonadScan'

addCharToString :: AlexAction LTok
addCharToString (_,_,_,s) len = do
  addCharToStringValue (head s)
  alexMonadScan'

endString :: Alex ()
endString = Alex $ \s -> Right(s{alex_ust=(alex_ust s){stringState=False}}, ())

endComment :: Alex ()
endComment = Alex $ \s -> Right(s{alex_ust=(alex_ust s){commentState=False}}, ())

testAndEndComment :: AlexAction LTok
testAndEndComment _ _ = do
  ss <- getStringState
  if ss then alexMonadScan' else endComment >> alexSetStartCode 0 >> alexMonadScan'

testAndEndString :: AlexAction LTok
testAndEndString (_,_,_,s) len = do
  startlen <- getStringDelimLen
  if startlen /= len
    then do forM_ (take len s) addCharToStringValue
            alexMonadScan'
    else do endString
            alexSetStartCode 0
            cs <- getCommentState
            if cs
              then do
                endComment
                alexMonadScan'
              else do
                val  <- getStringValue
                posn <- getStringPosn
                return (LTokSLit (reverse val), posn)

{-# INLINE mkString #-}
mkString :: Bool -> String -> Int -> AlexPosn -> LTok
mkString True s l posn =
    -- double quoted string, to make it Haskell readable:
    -- replace \\n with \n
    -- replace character codes with characters manually
    (LTokSLit (readString posn $ r (replaceCharCodes (take l s))), posn)
  where
    r ('\\' : '\n' : rest) = '\n' : r rest
    r (c : rest) = c : r rest
    r [] = []
mkString False s l posn =
    -- single quoted string, to make it Haskell readable:
    -- replace \\n with \n
    -- replace wrapping single quotes with double quotes
    -- replace escaped single quotes in the string with single quotes
    -- replace non-escaped double quotes in the string with escaped double quotes
    -- replace character codes with characters manually
    (LTokSLit (readString posn $ '"' : r (replaceCharCodes (take (l-2) $ drop 1 s)) ++ "\""), posn)
  where
    r ('\\' : '\n' : rest) = '\n' : r rest
    r ('\\' : '\'' : rest) = '\'' : r rest
    r ('"' : rest) = '\\' : '"' : r rest
    r (c : rest) = c : r rest
    r [] = []

replaceCharCodes :: String -> String
replaceCharCodes s =
  case s of
    ('\\' : 'x' : h1 : h2 : rest) -> toEnum (hexToInt h1 * 16 + hexToInt h2) : replaceCharCodes rest
    ('\\' : c1 : c2 : c3 : rest)
      | isNumber c1 && isNumber c2 && isNumber c3 ->
          toEnum (decToNum c1 * 100 + decToNum c2 * 10 + decToNum c3) : replaceCharCodes rest
      | isNumber c1 && isNumber c2 ->
          toEnum (decToNum c1 * 10 + decToNum c2) : replaceCharCodes (c3 : rest)
      | isNumber c1 ->
          toEnum (decToNum c1) : replaceCharCodes (c2 : c3 : rest)
      | otherwise ->
          '\\' : c1 : replaceCharCodes (c2 : c3 : rest)
    ['\\', c1, c2]
      | isNumber c1 && isNumber c2 ->
          [toEnum (decToNum c1 * 10 + decToNum c2)]
      | isNumber c1 ->
          toEnum (decToNum c1) : replaceCharCodes [c2]
      | otherwise -> s
    ['\\', c1]
      | isNumber c1 -> [toEnum (decToNum c1)]
      | otherwise -> s
    (c : rest) -> c : replaceCharCodes rest
    [] -> []

hexToInt :: Char -> Int
hexToInt c =
  case c of
    'A' -> 10
    'a' -> 10
    'B' -> 11
    'b' -> 11
    'C' -> 12
    'c' -> 12
    'D' -> 13
    'd' -> 13
    'E' -> 14
    'e' -> 14
    'F' -> 15
    'f' -> 15
    _   -> decToNum c

{-# INLINE decToNum #-}
decToNum :: Char -> Int
decToNum c = fromEnum c - fromEnum '0'


readString :: AlexPosn -> String -> String
readString (AlexPn _ line col) s =
  case readMay s of
    Nothing -> error $ concat
      [ "lexical error near line: ", show line, " col: ", show col, ": Cannot read string " ++ show s ]
    Just s' -> s'

data EOF = EOF deriving Show

-- | Lua token with position information.
type LTok = (LToken, AlexPosn)

-- type AlexAction result = AlexInput -> Int -> Alex result

-- Helper to make LTokens with string value (like LTokNum, LTokSLit etc.)
tokWValue :: (String -> LToken) -> AlexInput -> Int -> Alex LTok
tokWValue tok (posn,_,_,s) len = return (tok (take len s), posn)

tok :: LToken -> AlexInput -> Int -> Alex LTok
tok t (posn,_,_,_) _ = return (t, posn)

{-# INLINE ident #-}
ident :: AlexAction LTok
ident (posn,_,_,s) len = return (tok, posn)
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
alexEOF = return (LTokEof, AlexPn (-1) (-1) (-1))

alexMonadScan' :: Alex LTok
alexMonadScan' = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> do cs <- getCommentState
                  when cs endString
                  alexEOF
    AlexError ((AlexPn _ line col),ch,_,_) -> alexError $ concat
        [ "lexical error near line: " , show line , " col: " , show col , " at char " , [ch] ]
    AlexSkip  inp' len -> do
        alexSetInput inp'
        alexMonadScan'
    AlexToken inp' len action -> do
        alexSetInput inp'
        action (ignorePendingBytes inp) len

scanner :: String -> Either String [LTok]
scanner str = runAlex str loop
  where loop = do
          t@(tok, _) <- alexMonadScan'
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

-- | Run Lua lexer on a file.
llexFile :: FilePath -> IO [LTok]
llexFile p = llex <$> readFile p
}
