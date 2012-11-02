-- TODO:
-- * Multi-line comments and strings

{
module Language.Lua.Lexer
  ( llex
  , LTok
  , AlexPosn(..)
  ) where

import Language.Lua.Token
}

%wrapper "posn"

$space = [ \ \t ]                        -- horizontal white space
$eol   = \n                              -- end of line

$letter      = [a-zA-Z]                  -- first letter of variables
$identletter = [a-zA-Z_0-9]              -- letters for rest of variables

$digit    = 0-9                          -- decimal digits
$octdigit = 0-7                          -- octal digits
$hexdigit = [0-9a-fA-F]                  -- hexadecimal digits

$instr    = \0-\255 # [ \\ \" \n ]       -- valid character in a string literal
$anyButNL = \0-\255 # \n

@sp = $space*

-- escape characters
@charesc  = \\ ([ntvbrfaeE\\\?\'\"] | $octdigit{1,3} | x$hexdigit+ | X$hexdigit+)

@digits    = $digit+
@hexdigits = $hexdigit+

@mantpart = (@digits \. @digits) | @digits \. | \. @digits
@exppart  = [eE][\+\-]? @digits

@hexprefix = 0x | 0X
@mantparthex = (@hexdigits \. @hexdigits) | @hexdigits \. | \. @hexdigits
@expparthex  = [pP][\+\-]? @hexdigits

tokens :-

    $white+  ;
    "--" [^\n]* ;

    $letter $identletter* { ident }

    @digits               { \posn s -> (LTokNum s, posn) }
    @digits @exppart      { \posn s -> (LTokNum s, posn) }
    @mantpart @exppart?   { \posn s -> (LTokNum s, posn) }
    @hexprefix @hexdigits { \posn s -> (LTokNum s, posn) }
    @hexprefix @hexdigits @expparthex    { \posn s -> (LTokNum s, posn) }
    @hexprefix @mantparthex @expparthex? { \posn s -> (LTokNum s, posn) }

    \"($instr|@charesc)*\" { \posn s -> (LTokSLit s, posn) }

    "+"   { \posn _ -> (LTokPlus, posn) }
    "-"   { \posn _ -> (LTokMinus, posn) }
    "*"   { \posn _ -> (LTokStar, posn) }
    "/"   { \posn _ -> (LTokSlash, posn) }
    "%"   { \posn _ -> (LTokPercent, posn) }
    "^"   { \posn _ -> (LTokExp, posn) }
    "#"   { \posn _ -> (LTokSh, posn) }
    "=="  { \posn _ -> (LTokEqual, posn) }
    "~="  { \posn _ -> (LTokNotequal, posn) }
    "<="  { \posn _ -> (LTokLEq, posn) }
    ">="  { \posn _ -> (LTokGEq, posn) }
    "<"   { \posn _ -> (LTokLT, posn) }
    ">"   { \posn _ -> (LTokGT, posn) }
    "="   { \posn _ -> (LTokAssign, posn) }
    "("   { \posn _ -> (LTokLParen, posn) }
    ")"   { \posn _ -> (LTokRParen, posn) }
    "{"   { \posn _ -> (LTokLBrace, posn) }
    "}"   { \posn _ -> (LTokRBrace, posn) }
    "["   { \posn _ -> (LTokLBracket, posn) }
    "]"   { \posn _ -> (LTokRBracket, posn) }
    "::"  { \posn _ -> (LTokDColon, posn) }
    ";"   { \posn _ -> (LTokSemic, posn) }
    ":"   { \posn _ -> (LTokColon, posn) }
    ","   { \posn _ -> (LTokComma, posn) }
    "."   { \posn _ -> (LTokDot, posn) }
    ".."  { \posn _ -> (LTokDDot, posn) }
    "..." { \posn _ -> (LTokEllipsis, posn) }


{

type LTok = (LToken, AlexPosn)
type AlexAction = AlexPosn -> String -> LTok

{-# INLINE ident #-}
ident :: AlexAction
ident posn "and"    = (LTokAnd, posn)
ident posn "break"  = (LTokBreak, posn)
ident posn "do"     = (LTokDo, posn)
ident posn "else"   = (LTokElse, posn)
ident posn "elseif" = (LTokElseIf, posn)
ident posn "end"    = (LTokEnd, posn)
ident posn "false"  = (LTokFalse, posn)
ident posn "for"    = (LTokFor, posn)
ident posn "function" = (LTokFunction, posn)
ident posn "goto"   = (LTokGoto, posn)
ident posn "if"     = (LTokIf, posn)
ident posn "in"     = (LTokIn, posn)
ident posn "local"  = (LTokLocal, posn)
ident posn "nil"    = (LTokNil, posn)
ident posn "not"    = (LTokNot, posn)
ident posn "or"     = (LTokOr, posn)
ident posn "repeat" = (LTokRepeat, posn)
ident posn "return" = (LTokReturn, posn)
ident posn "then"   = (LTokThen, posn)
ident posn "true"   = (LTokTrue, posn)
ident posn "until"  = (LTokUntil, posn)
ident posn "while"  = (LTokWhile, posn)
ident posn name     = (LTokIdent name, posn)

--data AlexPosn = AlexPn !Int  -- absolute character offset
--                       !Int  -- line number
--                       !Int  -- column number
--
--type AlexInput = (AlexPosn,     -- current position,
--                  Char,         -- previous char
--                  [Byte],       -- rest of the bytes for the current char
--                  String)       -- current input string

--alexScanTokens :: String -> [token]
--alexScanTokens str = go (alexStartPos,'\n',[],str)
--  where go inp@(pos,_,_,str) =
--          case alexScan inp 0 of
--                AlexEOF -> []
--                AlexError ((AlexPn _ line column),_,_,_) -> error $ "lexical error at " ++ (show line) ++ " line, " ++ (show column) ++ " column"
--                AlexSkip  inp' len     -> go inp'
--                AlexToken inp' len act -> act pos (take len str) : go inp'

llex :: String -> [LTok]
llex = alexScanTokens

main = do
    s <- getContents
    print (alexScanTokens s)
}