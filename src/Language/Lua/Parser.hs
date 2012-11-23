{-# OPTIONS_GHC -Wall
                -fno-warn-hi-shadowing
                -fno-warn-name-shadowing
                -fno-warn-unused-do-bind #-}
module Language.Lua.Parser where

import Prelude hiding (exp, LT, GT, EQ, repeat)

import Language.Lua.Lexer
import Language.Lua.Token
import Language.Lua.Types

import Text.Parsec hiding (string)
import Text.Parsec.LTok
import Text.Parsec.Expr
import Control.Applicative ((<*), (<$>), (<*>))
import Control.Monad (void)

--parseText :: Parsec [LTok] () a -> String -> Either ParseError a
--parseText p s = parse p "test" (llex s)

parseText :: Parsec [LTok] () a -> String -> a
parseText p s = let tokens = llex s
                in case parse p "test" tokens of
                     Right r  -> r
                     Left err -> error (show err)

parens :: Monad m => ParsecT [LTok] u m a -> ParsecT [LTok] u m a
parens = between (tok LTokLParen) (tok LTokRParen)

name :: Parser String
name = tokenValue <$> anyIdent

number :: Parser String
number = tokenValue <$> anyNum

var :: Parser Var
var = do
    n <- (Left <$> name) <|> (Right <$> parens exp)
    case n of
      Left n' -> do
        r <- rest many
        return $ buildVar (Name n') r
      Right e -> do
        (x:xs) <- rest many1
        return $ case x of
          (Left e')  -> buildVar (Select (Paren e) e') xs
          (Right n') -> buildVar (SelectName (Paren e) n') xs

  where rest c =
          c $ (Left <$> between (tok LTokLBracket) (tok LTokRBracket) exp)
             <|> (Right <$> (tok LTokDot >> name))

        buildVar :: Var -> [Either Exp String] -> Var
        buildVar var [] = var
        buildVar var (Left exp:xs) = buildVar (Select (PEVar var) exp) xs
        buildVar var (Right name:xs) = buildVar (SelectName (PEVar var) name) xs

stringlit :: Parser String
stringlit = tokenValue <$> string

prefixExp :: Parser PrefixExp
prefixExp = (PEFunCall <$> try funCall)
        <|> (Paren <$> parens exp)
        <|> (PEVar <$> var)

funCall :: Parser FunCall
funCall = do
    f <- func
    cs <- calls
    let pe = case f of
               Left e  -> Paren e
               Right s -> PEVar s
    return $ buildFunCall pe cs

  where func :: Parser (Either Exp Var)
        func = (Left <$> parens exp) <|> (Right <$> var)

        calls :: Parser [(Maybe String, FunArg)]
        calls = many1 $ do
          m <- optionMaybe (tok LTokColon >> name)
          args <- funArg
          return (m, args)

        buildFunCall :: PrefixExp -> [(Maybe String, FunArg)] -> FunCall
        buildFunCall pe [(Just n, args)] = MethodCall pe n args
        buildFunCall pe [(Nothing, args)] = NormalFunCall pe args
        buildFunCall pe ((Just n, args):xs) = buildFunCall (PEFunCall (MethodCall pe n args)) xs
        buildFunCall pe ((Nothing, args):xs) = buildFunCall (PEFunCall (NormalFunCall pe args)) xs

funArg :: Parser FunArg
funArg = tableArg <|> stringArg <|> parlist
  where tableArg = TableArg <$> table
        stringArg = StringArg <$> stringlit
        parlist = parens (do exps <- exp `sepBy` tok LTokComma
                             return $ Args exps)

funBody :: Parser FunBody
funBody = do
    (params, vararg) <- parlist
    body <- block
    tok LTokEnd
    return $ FunBody params vararg body

  where parlist = parens $ do
          vars <- name `sepEndBy` tok LTokComma
          vararg <- optionMaybe $ try (tok LTokEllipsis) <|> tok LTokComma
          return $ case vararg of
                       Nothing -> (vars, False)
                       Just LTokEllipsis -> (vars, True)
                       _ -> (vars, False)

block :: Parser Block
block = do
  stats <- many (try stat)
  ret <- optionMaybe retstat
  return $ Block stats ret

retstat :: Parser [Exp]
retstat = do
  tok LTokReturn
  exps <- exp `sepBy` tok LTokComma
  optionMaybe (tok LTokSemic)
  return exps

tableField :: Parser TableField
tableField = expField <|> try namedField <|> field
  where expField :: Parser TableField
        expField = do
            e1 <- between (tok LTokLBracket)
                          (tok LTokRBracket)
                          exp
            tok LTokAssign
            e2 <- exp
            return $ ExpField e1 e2

        namedField :: Parser TableField
        namedField = do
            name' <- name
            tok LTokAssign
            val <- exp
            return $ NamedField name' val

        field :: Parser TableField
        field = Field <$> exp

table :: Parser Table
table = between (tok LTokLBrace)
                (tok LTokRBrace)
                (do fields <- tableField `sepEndBy` fieldSep
                    return $ Table fields)
  where fieldSep = tok LTokComma <|> tok LTokSemic

-----------------------------------------------------------------------
---- Expressions

nilExp, boolExp, numberExp, stringExp, varargExp, fundefExp,
  prefixexpExp, tableconstExp, opExp, exp, exp' :: Parser Exp

nilExp = tok LTokNil >> return Nil

boolExp = (tok LTokTrue >> return (Bool True)) <|>
            (tok LTokFalse >> return (Bool False))

numberExp = Number <$> number

stringExp = String <$> stringlit

varargExp = tok LTokEllipsis >> return Vararg

fundefExp = do
  tok LTokFunction
  body <- funBody
  return $ EFunDef (FunDef body)

prefixexpExp = PrefixExp <$> prefixExp

tableconstExp = TableConst <$> table

binary :: Monad m => LToken -> (a -> a -> a) -> Assoc -> Operator [LTok] u m a
binary op fun = Infix (tok op >> return fun)

prefix :: Monad m => LToken -> (a -> a) -> Operator [LTok] u m a
prefix op fun       = Prefix (tok op >> return fun)

opTable :: Monad m => [[Operator [LTok] u m Exp]]
opTable = [ [ binary LTokExp       (Binop Exp)    AssocRight ]
          , [ prefix LTokNot       (Unop Not)
            , prefix LTokSh        (Unop Len)
            , prefix LTokMinus     (Unop Neg)
            ]
          , [ binary LTokStar      (Binop Mul)    AssocLeft
            , binary LTokSlash     (Binop Div)    AssocLeft
            , binary LTokPercent   (Binop Mod)    AssocLeft
            ]
          , [ binary LTokPlus      (Binop Add)    AssocLeft
            , binary LTokMinus     (Binop Sub)    AssocLeft
            ]
          , [ binary LTokDDot      (Binop Concat) AssocRight ]
          , [ binary LTokGT        (Binop GT)     AssocLeft
            , binary LTokLT        (Binop LT)     AssocLeft
            , binary LTokGEq       (Binop GTE)    AssocLeft
            , binary LTokLEq       (Binop LTE)    AssocLeft
            , binary LTokNotequal  (Binop NEQ)    AssocLeft
            , binary LTokEqual     (Binop EQ)     AssocLeft
            ]
          , [ binary LTokAnd       (Binop And)    AssocLeft ]
          , [ binary LTokOr        (Binop Or)     AssocLeft ]
          ]

opExp = buildExpressionParser opTable exp' <?> "opExp"

exp =
    choice [ try opExp
           , try nilExp
           , try boolExp
           , try numberExp
           , try stringExp
           , try varargExp
           , try fundefExp
           , try prefixexpExp
           , try tableconstExp
           ]

exp' =
    choice [ try nilExp
           , try boolExp
           , try numberExp
           , try stringExp
           , try varargExp
           , try fundefExp
           , try prefixexpExp
           , try tableconstExp
           ]

-----------------------------------------------------------------------
---- Statements

assignStat, funCallStat, labelStat, breakStat, gotoStat,
    doStat, whileStat, repeatStat, ifStat, forRangeStat,
    forInStat, funAssignStat, localFunAssignStat, localAssignStat, stat :: Parser Stat

emptyStat :: Parser ()
emptyStat = void (tok LTokSemic)

assignStat = do
    vars <- var `sepBy` tok LTokComma
    tok LTokAssign
    exps <- exp `sepBy` tok LTokComma
    return $ Assign vars exps

funCallStat = FunCall <$> funCall

labelStat = Label <$> label
  where label = between (tok LTokDColon) (tok LTokDColon) name

breakStat = tok LTokBreak >> return Break

gotoStat = Goto <$> (tok LTokGoto >> name)

doStat = Do <$> between (tok LTokDo) (tok LTokEnd) block

whileStat =
  between (tok LTokWhile)
          (tok LTokEnd)
          (do cond <- exp
              tok LTokDo
              body <- block
              return $ While cond body)

repeatStat = do
    tok LTokRepeat
    body <- block
    tok LTokUntil
    cond <- exp
    return $ Repeat body cond

ifStat =
  between (tok LTokIf)
          (tok LTokEnd)
          (do f <- ifPart
              conds <- many elseifPart
              l <- optionMaybe elsePart
              return $ If (f:conds) l)

  where ifPart :: Parser (Exp, Block)
        ifPart = do
            cond <- exp
            tok LTokThen
            body <- block
            return (cond, body)

        elseifPart :: Parser (Exp, Block)
        elseifPart = do
            tok LTokElseIf
            cond <- exp
            tok LTokThen
            body <- block
            return (cond, body)

        elsePart :: Parser Block
        elsePart = tok LTokElse >> block

forRangeStat =
    between (tok LTokFor)
            (tok LTokEnd)
            (do name' <- name
                tok LTokAssign
                start <- exp
                tok LTokComma
                end <- exp
                range <- optionMaybe $ tok LTokComma >> exp
                tok LTokDo
                body <- block
                return $ ForRange name' start end range body)

forInStat =
    between (tok LTokFor)
            (tok LTokEnd)
            (do names <- name `sepBy` tok LTokComma
                tok LTokIn
                exps <- exp `sepBy` tok LTokComma
                tok LTokDo
                body <- block
                return $ ForIn names exps body)

funAssignStat = do
    tok LTokFunction
    name' <- funName
    body <- funBody
    return $ FunAssign name' body
  where funName :: Parser FunName
        funName = FunName <$> name
                          <*> optionMaybe (tok LTokDot >> name)
                          <*> many (tok LTokColon >> name)

localFunAssignStat = do
    tok LTokLocal
    tok LTokFunction
    name' <- name
    body <- funBody
    return $ LocalFunAssign name' body

localAssignStat = do
    tok LTokLocal
    names <- name `sepBy` tok LTokComma
    rest <- optionMaybe $ tok LTokAssign >> exp `sepBy` tok LTokComma
    return $ LocalAssign names rest

stat =
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
           ]

chunk :: Parser Block
chunk = block <* tok LTokEof