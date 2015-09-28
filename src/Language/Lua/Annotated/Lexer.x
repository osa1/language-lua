{

{-# OPTIONS_GHC -w #-}

module Language.Lua.Annotated.Lexer
  ( llex
  , llexFile
  , LTok
  , AlexPosn(..)
  , interpretStringLiteral
  ) where

import Language.Lua.Token
import Control.Monad (ap, liftM, forM_, when)
import Data.Char (GeneralCategory(..),generalCategory,isAscii,isNumber,isSpace,chr)
import Data.List (foldl')
import Data.Word (Word8)
import Data.Bits ((.&.),shiftR)
import Data.Monoid (mempty, mappend)
import Control.Applicative (Applicative(..))
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as B

}

$space = [ \ \t ]                        -- horizontal white space

$letter      = [a-zA-Z_]                 -- first letter of variables
$identletter = [a-zA-Z_0-9]              -- letters for rest of variables

$digit    = 0-9                          -- decimal digits
$hexdigit = [0-9a-fA-F]                  -- hexadecimal digits

$dqstr    = \0-\255 # [ \" \n \\ ]       -- valid character in a string literal with dquotes
$sqstr    = \0-\255 # [ \' \n \\ ]       -- valid character in a string literal with quotes
$longstr  = \0-\255                      -- valid character in a long string

-- escape characters
@charescd  = \\ ([ntvbrfa\\'"] | $digit{1,3} | x$hexdigit{2} | u\{$hexdigit{1,}\} | \n | z [$space \n\r\f\v]*)
@charescs  = \\ ([ntvbrfa\\"'] | $digit{1,3} | x$hexdigit{2} | u\{$hexdigit{1,}\} | \n | z [$space \n\r\f\v]*)

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

    <0> \"($dqstr|@charescd)*\" { \(posn,_,s) l -> return $ mkString s l posn }
    <0> \'($sqstr|@charescs)*\' { \(posn,_,s) l -> return $ mkString s l posn }

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
    <0> "//"  { tok LTokDSlash }
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
    <0> "&"   { tok LTokAmpersand }
    <0> "|"   { tok LTokPipe }
    <0> "~"   { tok LTokTilde }
    <0> "<<"  { tok LTokDLT }
    <0> ">>"  { tok LTokDGT }

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

putInputBack :: String -> Alex ()
putInputBack str = Alex $ \s -> Right (s{alex_inp=str ++ alex_inp s}, ())

enterString :: AlexAction LTok
enterString (posn,_,s) len = do
  initString (if (s !! (len-1) == '\n') then len-1 else len) posn
  alexMonadScan'

enterComment :: AlexAction LTok
enterComment _ _ = do
  initComment
  alexMonadScan'

addCharToString :: AlexAction LTok
addCharToString (_,_,s) len = do
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
testAndEndString (_,_,s) len = do
  startlen <- getStringDelimLen
  if startlen /= len
    then do addCharToStringValue (head s)
            putInputBack (tail $ take len s)
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
mkString :: String -> Int -> AlexPosn -> LTok
mkString s l posn = (LTokSLit (take (l-2) (drop 1 s)), posn)

skipWS :: String -> String
skipWS (' '  : rest) = skipWS rest
skipWS ('\n' : rest) = skipWS rest
skipWS ('\r' : rest) = skipWS rest
skipWS ('\f' : rest) = skipWS rest
skipWS ('\t' : rest) = skipWS rest
skipWS ('\v' : rest) = skipWS rest
skipWS str           = str

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


interpretStringLiteral :: String -> B.ByteString
interpretStringLiteral = B.toLazyByteString . aux
  where
  aux xxs =
    case xxs of
      [] -> mempty
      '\\' : 'x' : h1 : h2 : rest ->
        B.word8 (fromIntegral (hexToInt h1 * 16 + hexToInt h2)) `mappend` aux rest

      '\\' : 'u' : '{' : rest ->
        case break (=='}') rest of
          (ds,_:rest')
             | code <= 0x10ffff -> encodeChar (chr code) `mappend` aux rest'
             where code = foldl' (\acc d -> acc * 16 + hexToInt d) 0 ds
          _ -> encodeChar '\xFFFD' `mappend` aux (dropWhile (/='}') rest)

      '\\' : c1 : c2 : c3 : rest
        | isNumber c1 && isNumber c2 && isNumber c3 ->
            let code = decToNum c1 * 100 + decToNum c2 * 10 + decToNum c3
            in B.word8 (fromIntegral code) `mappend` aux rest

      '\\' : c1 : c2 : rest
        | isNumber c1 && isNumber c2 ->
            let code = decToNum c1 * 10 + decToNum c2
            in B.word8 (fromIntegral code) `mappend` aux rest

      '\\' : c1 : rest
        | isNumber c1 -> B.word8 (fromIntegral (decToNum c1)) `mappend` aux rest

      '\\' : 'a'  : rest -> B.char8 '\a' `mappend` aux rest
      '\\' : 'b'  : rest -> B.char8 '\b' `mappend` aux rest
      '\\' : 'f'  : rest -> B.char8 '\f' `mappend` aux rest
      '\\' : 'n'  : rest -> B.char8 '\n' `mappend` aux rest
      '\\' : '\n' : rest -> B.char8 '\n' `mappend` aux rest
      '\\' : 'r'  : rest -> B.char8 '\r' `mappend` aux rest
      '\\' : 't'  : rest -> B.char8 '\t' `mappend` aux rest
      '\\' : 'v'  : rest -> B.char8 '\v' `mappend` aux rest
      '\\' : '\\' : rest -> B.char8 '\\' `mappend` aux rest
      '\\' : '"'  : rest -> B.char8 '"' `mappend` aux rest
      '\\' : '\'' : rest -> B.char8 '\'' `mappend` aux rest
      '\\' : 'z'  : rest -> aux (skipWS rest)
      c : rest -> encodeChar c `mappend` aux rest

-- | Lua token with position information.
type LTok = (LToken, AlexPosn)

type AlexAction result = AlexInput -> Int -> Alex result

-- Helper to make LTokens with string value (like LTokNum, LTokSLit etc.)
tokWValue :: (String -> LToken) -> AlexInput -> Int -> Alex LTok
tokWValue tok (posn,_,s) len = return (tok (take len s), posn)

tok :: LToken -> AlexInput -> Int -> Alex LTok
tok t (posn,_,_) _ = return (t, posn)

{-# INLINE ident #-}
ident :: AlexAction LTok
ident (posn,_,s) len = return (tok, posn)
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
    AlexError ((AlexPn _ line col),ch,_) -> alexError ("at char " ++ [ch])
    AlexSkip  inp' len -> do
        alexSetInput inp'
        alexMonadScan'
    AlexToken inp' len action -> do
        alexSetInput inp'
        action inp len

scanner :: String -> Either (String,AlexPosn) [LTok]
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

-- | Drop the first line of a Lua file when it starts with a '#'
dropSpecialComment :: String -> String
dropSpecialComment ('#':xs) = dropWhile (/='\n') xs
dropSpecialComment xs = xs
-- Newline is preserved in order to ensure that line numbers stay correct

-- | Lua lexer.
llex :: String -> Either (String,AlexPosn) [LTok]
llex = scanner . dropSpecialComment

-- | Run Lua lexer on a file.
llexFile :: FilePath -> IO (Either (String,AlexPosn) [LTok])
llexFile = fmap llex . readFile

------------------------------------------------------------------------
-- Custom Alex wrapper
------------------------------------------------------------------------

data AlexPosn = AlexPn !Int  -- absolute character offset
                       !Int  -- line number
                       !Int  -- column number
  deriving (Show,Eq)

type AlexInput = (AlexPosn,     -- current position,
                  Char,         -- previous char
                  String)       -- current input string

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte (_,_,[]) = Nothing
alexGetByte (p,_,c:cs) = Just (byteForChar c,(p',c,cs))
  where p' = alexMove p c

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn ix line column) c =
  case c of
    '\t' -> AlexPn (ix + 1) line (((column + 7) `div` 8) * 8 + 1)
    '\n' -> AlexPn (ix + 1) (line + 1) 1
    _    -> AlexPn (ix + 1) line (column + 1)

------------------------------------------------------------------------
-- Embed all of unicode, kind of, in a single byte!
------------------------------------------------------------------------

byteForChar :: Char -> Word8
byteForChar c
  | c <= '\6' = non_graphic
  | isAscii c = fromIntegral (ord c)
  | otherwise = case generalCategory c of
                  LowercaseLetter       -> lower
                  OtherLetter           -> lower
                  UppercaseLetter       -> upper
                  TitlecaseLetter       -> upper
                  DecimalNumber         -> digit
                  OtherNumber           -> digit
                  ConnectorPunctuation  -> symbol
                  DashPunctuation       -> symbol
                  OtherPunctuation      -> symbol
                  MathSymbol            -> symbol
                  CurrencySymbol        -> symbol
                  ModifierSymbol        -> symbol
                  OtherSymbol           -> symbol
                  Space                 -> space
                  ModifierLetter        -> other
                  NonSpacingMark        -> other
                  SpacingCombiningMark  -> other
                  EnclosingMark         -> other
                  LetterNumber          -> other
                  OpenPunctuation       -> other
                  ClosePunctuation      -> other
                  InitialQuote          -> other
                  FinalQuote            -> other
                  _                     -> non_graphic
  where
  non_graphic     = 0
  upper           = 1
  lower           = 2
  digit           = 3
  symbol          = 4
  space           = 5
  other           = 6

-- perform an action for this token, and set the start code to a new value
andBegin :: AlexAction result -> Int -> AlexAction result
(action `andBegin` code) input len = do alexSetStartCode code; action input len

alexSetStartCode :: Int -> Alex ()
alexSetStartCode sc = Alex $ \s -> Right (s{alex_scd=sc}, ())

data AlexState = AlexState {
        alex_pos :: !AlexPosn,  -- position at current input location
        alex_inp :: String,     -- the current input
        alex_chr :: !Char,      -- the character before the input
        alex_scd :: !Int        -- the current startcode

      , alex_ust :: AlexUserState -- AlexUserState will be defined in the user program

    }

-- Compile with -funbox-strict-fields for best results!

runAlex :: String -> Alex a -> Either (String,AlexPosn) a
runAlex input (Alex f)
   = case f (AlexState {alex_pos = alexStartPos,
                        alex_inp = input,
                        alex_chr = '\n',

                        alex_ust = alexInitUserState,

                        alex_scd = 0}) of Left e -> Left e
                                          Right ( _, a ) -> Right a

newtype Alex a = Alex { unAlex :: AlexState -> Either (String,AlexPosn) (AlexState, a) }

alexError :: String -> Alex a
alexError message = Alex $ \s -> Left (message, alex_pos s)

alexGetStartCode :: Alex Int
alexGetStartCode = Alex $ \s@AlexState{alex_scd=sc} -> Right (s, sc)

alexSetInput :: AlexInput -> Alex ()
alexSetInput (pos,c,inp)
 = Alex $ \s -> case s{alex_pos=pos,alex_chr=c,alex_inp=inp} of
                  s@(AlexState{}) -> Right (s, ())

alexGetInput :: Alex AlexInput
alexGetInput
 = Alex $ \s@AlexState{alex_pos=pos,alex_chr=c,alex_inp=inp} ->
        Right (s, (pos,c,inp))

instance Functor Alex where
  fmap = liftM

instance Applicative Alex where
  pure a = Alex $ \s -> Right (s,a)
  (<*>) = ap

instance Monad Alex where
  return = pure
  m >>= k  = Alex $ \s -> case unAlex m s of
                                Left msg -> Left msg
                                Right (s',a) -> unAlex (k a) s'

encodeChar :: Char -> B.Builder
encodeChar c
   | oc <= 0x7f       = asByte oc

   | oc <= 0x7ff      = asByte (0xc0 + (oc `shiftR` 6))
              `mappend` asByte (0x80 + oc .&. 0x3f)

   | oc <= 0xffff     = asByte (0xe0 + (oc `shiftR` 12))
              `mappend` asByte (0x80 + ((oc `shiftR` 6) .&. 0x3f))
              `mappend` asByte (0x80 + oc .&. 0x3f)

   | otherwise        = asByte (0xf0 + (oc `shiftR` 18))
              `mappend` asByte (0x80 + ((oc `shiftR` 12) .&. 0x3f))
              `mappend` asByte (0x80 + ((oc `shiftR` 6) .&. 0x3f))
              `mappend` asByte (0x80 + oc .&. 0x3f)
  where
    asByte = B.word8 . fromIntegral
    oc = ord c

}
