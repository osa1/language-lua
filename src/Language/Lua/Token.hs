module Language.Lua.Token where

-- | Lua tokens
data LToken = LTokPlus               -- ^+
            | LTokMinus              -- ^\-
            | LTokStar               -- ^\*
            | LTokSlash              -- ^/
            | LTokPercent            -- ^%
            | LTokExp                -- ^^
            | LTokSh                 -- ^#
            | LTokEqual              -- ^==
            | LTokNotequal           -- ^~=
            | LTokLEq                -- ^<=
            | LTokGEq                -- ^\>=
            | LTokLT                 -- ^<
            | LTokGT                 -- ^\>
            | LTokAssign             -- ^=
            | LTokLParen             -- ^(
            | LTokRParen             -- ^)
            | LTokLBrace             -- ^{
            | LTokRBrace             -- ^}
            | LTokLBracket           -- ^\[
            | LTokRBracket           -- ^]
            | LTokDColon             -- ^::
            | LTokSemic              -- ^;
            | LTokColon              -- ^:
            | LTokComma              -- ^,
            | LTokDot                -- ^.
            | LTokDDot               -- ^..
            | LTokEllipsis           -- ^...
            | LTokDLT                -- ^<<
            | LTokDGT                -- ^>>
            | LTokAmpersand          -- ^&
            | LTokPipe               -- ^|
            | LTokDSlash             -- ^//
            | LTokTilde              -- ^~

            | LTokAnd                -- ^and
            | LTokBreak              -- ^break
            | LTokDo                 -- ^do
            | LTokElse               -- ^else
            | LTokElseIf             -- ^elseif
            | LTokEnd                -- ^end
            | LTokFalse              -- ^false
            | LTokFor                -- ^for
            | LTokFunction           -- ^function
            | LTokGoto               -- ^goto
            | LTokIf                 -- ^if
            | LTokIn                 -- ^in
            | LTokLocal              -- ^local
            | LTokNil                -- ^nil
            | LTokNot                -- ^not
            | LTokOr                 -- ^or
            | LTokRepeat             -- ^repeat
            | LTokReturn             -- ^return
            | LTokThen               -- ^then
            | LTokTrue               -- ^true
            | LTokUntil              -- ^until
            | LTokWhile              -- ^while

            | LTokNum       String   -- ^number constant
            | LTokSLit      String  -- ^string constant. Includes quotes!
            | LTokIdent     String   -- ^identifier
            | LTokEof                -- ^end of file
    deriving Eq

instance Show LToken where
    show LTokPlus          = "`+`"
    show LTokMinus         = "`-`"
    show LTokStar          = "`*`"
    show LTokSlash         = "`/`"
    show LTokPercent       = "`%`"
    show LTokExp           = "`^`"
    show LTokSh            = "`#`"
    show LTokEqual         = "`==`"
    show LTokNotequal      = "`~=`"
    show LTokLEq           = "`<=`"
    show LTokGEq           = "`>=`"
    show LTokLT            = "`<`"
    show LTokGT            = "`>`"
    show LTokAssign        = "`=`"
    show LTokLParen        = "`(`"
    show LTokRParen        = "`)`"
    show LTokLBrace        = "`{`"
    show LTokRBrace        = "`}`"
    show LTokLBracket      = "`[`"
    show LTokRBracket      = "`]`"
    show LTokDColon        = "`::`"
    show LTokSemic         = "`;`"
    show LTokColon         = "`:`"
    show LTokComma         = "`,`"
    show LTokDot           = "`.`"
    show LTokDDot          = "`..`"
    show LTokEllipsis      = "`...`"
    show LTokDLT           = "`<<`"
    show LTokDGT           = "`>>`"
    show LTokAmpersand     = "`&`"
    show LTokPipe          = "`|`"
    show LTokDSlash        = "`//`"
    show LTokTilde         = "`~`"

    show LTokAnd           = "`and`"
    show LTokBreak         = "`break`"
    show LTokDo            = "`do`"
    show LTokElse          = "`else`"
    show LTokElseIf        = "`elseif`"
    show LTokEnd           = "`end`"
    show LTokFalse         = "`false`"
    show LTokFor           = "`for`"
    show LTokFunction      = "`function`"
    show LTokGoto          = "`goto`"
    show LTokIf            = "`if`"
    show LTokIn            = "`in`"
    show LTokLocal         = "`local`"
    show LTokNil           = "`nil`"
    show LTokNot           = "`not`"
    show LTokOr            = "`or`"
    show LTokRepeat        = "`repeat`"
    show LTokReturn        = "`return`"
    show LTokThen          = "`then`"
    show LTokTrue          = "`true`"
    show LTokUntil         = "`until`"
    show LTokWhile         = "`while`"

    show (LTokNum   n)     = "number: " ++ show n
    show (LTokSLit  s)     = "string: " ++ show s
    show (LTokIdent i)     = "identifier: " ++ show i
    show LTokEof           = "EOF"
