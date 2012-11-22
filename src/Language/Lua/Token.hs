module Language.Lua.Token (LToken(..), tokenValue) where

-- ^Lua tokens
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
            | LTokSLit      String   -- ^string constant
            | LTokIdent     String   -- ^identifier
            | LTokEof                -- ^end of file
    deriving (Show, Eq)

-- | Partial function, returns value of `LTokNum`, `LTokSLit` and `LTokIdent`.
tokenValue :: LToken -> String
tokenValue (LTokNum n)   = n
tokenValue (LTokSLit s)  = s
tokenValue (LTokIdent i) = i
tokenValue tok           = error ("trying to get value of " ++ show tok)