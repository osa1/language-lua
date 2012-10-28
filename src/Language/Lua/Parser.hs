{-# OPTIONS_GHC -Wall
                -fno-warn-hi-shadowing
                -fno-warn-name-shadowing
                -fno-warn-unused-do-bind #-}
module Language.Lua.Parser where

import Prelude hiding (exp, LT, GT, EQ, repeat)

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Control.Applicative ((<*), (<$>), (<*>))

import Language.Lua.Types

spString :: String -> Parser String
spString s = string s <* spaces

spChar :: Char -> Parser Char
spChar c = char c <* spaces

name :: Parser Name
name = do
    fc <- oneOf firstChar
    r  <- optionMaybe (many $ oneOf rest)
    spaces
    return $ case r of
               Nothing -> [fc]
               Just s  -> fc : s
  where firstChar = ['A'..'Z'] ++ ['a'..'z'] ++ "_"
        rest      = firstChar ++ ['0'..'9']

number :: Parser String
--number = try hexNum <|> try expNum <|> num
number = num

num :: Parser String
num = do
    f <- many1 int
    dot <- optionMaybe $ char '.'
    case dot of
      Nothing -> return f
      Just _ -> do
          r <- many1 int
          return $ f ++ "." ++ r
  where int = oneOf ['0'..'9']

--hexNum :: Parser String
--hexNum = undefined -- TODO

--expNum :: Parser String
--expNum = undefined -- TODO

var :: Parser Var
var = --choice [ try select, try selectName, name' ]
      --choice [ try selectName, name' ]
      choice [ name' ]
  where select = Select <$> prefixExp <*> exp
        selectName = SelectName <$> prefixExp <*> name
        name' = Name <$> name

stringlit :: Parser String
stringlit = do
  char '"'
  s <- many $ noneOf "\"" -- FIXME
  char '"'
  return s

prefixExp :: Parser PrefixExp
prefixExp =
  choice [ Paren <$> (spChar '(' >> exp <* spChar ')')
         , PEFunCall <$> try funCall
         , PEVar <$> var
         ]

--funCall :: Parser FunCall
--funCall = do
--    prefix <- prefixExp
--    method <- optionMaybe methodName
--    arg <- funArg
--    return $ case method of
--                 Nothing -> NormalFunCall prefix arg
--                 Just m  -> MethodCall prefix m arg
--  where methodName = spChar ':' >> name


funCall :: Parser FunCall
funCall = do
    prefix <- prefixExp'
    rest <- many1 argPart
    let ((methodName, args):rest') = rest
        firstCall = case methodName of
                        Nothing -> NormalFunCall prefix args
                        Just m  -> MethodCall prefix m args
    return $ buildFunCall firstCall rest'

  where methodName :: Parser Name
        methodName = spChar ':' >> name

        argPart :: Parser (Maybe Name, FunArg)
        argPart = (,) <$> optionMaybe methodName <*> funArg

        prefixExp' :: Parser PrefixExp
        prefixExp' = choice [ Paren <$> (spChar '(' >> exp <* spChar ')')
                            , PEVar <$> var
                            ]

        buildFunCall :: FunCall -> [(Maybe Name, FunArg)] -> FunCall
        buildFunCall prefix [] = prefix
        buildFunCall prefix ((methodName, args):xs) =
            case methodName of
                Nothing -> buildFunCall (NormalFunCall (PEFunCall prefix) args) xs
                Just m  -> buildFunCall (MethodCall (PEFunCall prefix) m args) xs


funArg :: Parser FunArg
funArg = tableArg <|> stringArg <|> parlist
  where tableArg = TableArg <$> table
        stringArg = StringArg <$> stringlit
        parlist = do
            spChar '('
            exps <- exp `sepBy` spChar ','
            spChar ')'
            return $ Args exps

funBody :: Parser FunBody
funBody = do
    (params, vararg) <- parlist
    body <- block
    return $ FunBody params vararg body

  where parlist = do
            spChar '('
            vars <- name `sepBy` spChar ','
            vararg <- optionMaybe $ (try $ spString "...") <|> (spString ",")
            spChar ')'
            return $ case vararg of
                         Nothing -> (vars, False)
                         Just "..." -> (vars, True)
                         _ -> (vars, False)

block :: Parser Block
block = do
  stats <- stat `sepBy` spaces
  ret <- retstat
  return $ Block stats ret

retstat :: Parser (Maybe [Exp])
retstat = do
  spString "return"
  exps <- optionMaybe (exp `sepBy` spChar ',')
  optionMaybe (spChar ';')
  return exps

tableField :: Parser TableField
tableField = choice [ expField
                    , try namedField
                    , field
                    ]
  where expField :: Parser TableField
        expField = do
            spChar '['
            e1 <- exp
            spChar ']'
            spChar '='
            e2 <- exp
            return $ ExpField e1 e2

        namedField :: Parser TableField
        namedField = do
            name' <- name
            spChar '='
            val <- exp
            return $ NamedField name' val

        field :: Parser TableField
        field = Field <$> exp

table :: Parser Table
table = do
    spChar '{'
    fields <- tableField `sepBy` fieldSep
    optionMaybe fieldSep
    return $ Table fields
  where fieldSep = spChar ',' <|> spChar ';'

---------------------------------------------------------------------
-- Expressions

nilExp, boolExp, numberExp, stringExp, varargExp, fundefExp,
  prefixexpExp, tableconstExp, opExp, exp, exp' :: Parser Exp

nilExp = string "nil" >> return Nil

boolExp = (string "true" >> return (Bool "true")) <|>
            (string "false" >> return (Bool "false"))

numberExp = Number <$> number

stringExp = String <$> stringlit

varargExp = string "..." >> return Vararg

fundefExp = do
  spString "function"
  body <- funBody
  return $ EFunDef (FunDef body)


prefixexpExp = PrefixExp <$> prefixExp

tableconstExp = TableConst <$> table

binary :: String -> (a -> a -> a) -> Assoc -> Operator Char () a
binary name fun assoc = Infix (spString name >> return fun) assoc

prefix :: String -> (a -> a) -> Operator Char () a
prefix name fun       = Prefix (spString name >> return fun)

opTable :: [[Operator Char () Exp]]
opTable = [ [ binary "^"   (Binop Exp)    AssocRight ]
          , [ prefix "not" (Unop Not)
            , prefix "#"   (Unop Len)
            , prefix "-"   (Unop Neg)
            ]
          , [ binary "*"   (Binop Mul)    AssocLeft
            , binary "/"   (Binop Div)    AssocLeft
            , binary "%"   (Binop Mod)    AssocLeft
            ]
          , [ binary "+"   (Binop Add)    AssocLeft
            , binary "-"   (Binop Sub)    AssocLeft
            ]
          , [ binary ".."  (Binop Concat) AssocRight ]
          , [ binary ">"   (Binop GT)     AssocLeft
            , binary "<"   (Binop LT)     AssocLeft
            , binary ">="  (Binop GTE)    AssocLeft
            , binary "<="  (Binop LTE)    AssocLeft
            ]
          , [ binary "and" (Binop And)    AssocLeft ]
          , [ binary "or"  (Binop Or)     AssocLeft ]
          ]

opExp = buildExpressionParser opTable exp' <?> "opExp"

exp = spaces >>
    choice [ try opExp
           , try nilExp
           , try boolExp
           , try numberExp
           , try stringExp
           , try varargExp
           , try fundefExp
           , try prefixexpExp -- stucks in a loop
           , try tableconstExp
           --, binopExp
           --, unopExp
           ] <* spaces

exp' = spaces >>
    choice [ try nilExp
           , try boolExp
           , try numberExp
           , try stringExp
           , try varargExp
           , try fundefExp
           , try prefixexpExp
           , try tableconstExp
           ] <* spaces

---------------------------------------------------------------------
-- Statements

assignStat, funCallStat, labelStat, breakStat, gotoStat,
    doStat, whileStat, repeatStat, ifStat, forRangeStat,
    forInStat, funAssignStat, localFunAssignStat, localAssignStat, stat :: Parser Stat

emptyStat :: Parser ()
emptyStat = optionMaybe (spChar ';') >> return ()

assignStat = do
    vars <- var `sepBy` spChar ','
    spChar '='
    exps <- exp `sepBy` spChar ','
    return $ Assign vars exps

funCallStat = FunCall <$> funCall

labelStat = Label <$> label
  where label = spString "::" >> (name <* spString "::")

breakStat = spString "break" >> return Break

gotoStat = Goto <$> (spString "goto" >> name)

doStat = do
    spString "do"
    b <- block
    spString "end"
    return $ Do b

whileStat = do
    spString "while"
    cond <- exp
    spString "do"
    body <- block
    spString "end"
    return $ While cond body

repeatStat = do
    spString "repeat"
    body <- block
    spString "until"
    cond <- exp
    return $ Repeat body cond

ifStat = do
    f <- ifPart
    conds <- many elseifPart
    l <- optionMaybe elsePart
    spString "end"
    return $ If (f : conds) l
  where ifPart :: Parser (Exp, Block)
        ifPart = do
            spString "if"
            cond <- exp
            spString "then"
            body <- block
            return (cond, body)

        elseifPart :: Parser (Exp, Block)
        elseifPart = do
            spString "elseif"
            cond <- exp
            spString "then"
            body <- block
            return (cond, body)

        elsePart :: Parser Block
        elsePart = spString "else" >> block

forRangeStat = do
    spString "for"
    name' <- name
    spChar '='
    start <- exp
    spChar ','
    end <- exp
    range <- optionMaybe $ spChar ',' >> exp
    spString "do"
    body <- block
    spString "end"
    return $ ForRange name' start end range body

forInStat = do
    spString "for"
    names <- name `sepBy` spChar ','
    spString "in"
    exps <- exp `sepBy` spChar ','
    spString "do"
    body <- block
    spString "end"
    return $ ForIn names exps body

funAssignStat = do
    spString "function"
    name' <- name
    body <- funBody
    return $ FunAssign name' body

localFunAssignStat = do
    spString "local"
    spString "function"
    name' <- name
    body <- funBody
    return $ LocalFunAssign name' body

localAssignStat = do
    spString "local"
    names <- name `sepBy` spChar ','
    rest <- optionMaybe $ spChar '=' >> exp `sepBy` spChar ','
    return $ LocalAssign names rest

stat = spaces >>
    choice [ try assignStat
           , try funCallStat
           , try labelStat
           , try breakStat
           , try gotoStat
           , try doStat
           , try whileStat
           , try repeatStat
           , try ifStat
           , try forRangeStat
           , try forInStat
           , try funAssignStat
           , try localFunAssignStat
           , try localAssignStat
           ] <* spaces