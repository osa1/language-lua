{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
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

parseText :: Show a => Parsec [LTok] () a -> String -> IO ()
parseText p = parseTest p . llex

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
        case x of
          (Left e') -> return $ buildVar (Select (Paren e) e') xs
          (Right n') -> return $ buildVar (SelectName (Paren e) n') xs

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
prefixExp = choice [ Paren <$> parens exp
                   , PEFunCall <$> try funCall
                   , PEVar <$> var
                   ]

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
        methodName = tok LTokColon >> name

        argPart :: Parser (Maybe Name, FunArg)
        argPart = (,) <$> optionMaybe methodName <*> funArg

        prefixExp' :: Parser PrefixExp
        prefixExp' = choice [ Paren <$> parens exp
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
        parlist = parens (do exps <- exp `sepBy` (tok LTokComma)
                             return $ Args exps)

funBody :: Parser FunBody
funBody = do
    (params, vararg) <- parlist
    body <- block
    tok LTokEnd
    return $ FunBody params vararg body

  where parlist = parens $ do
          vars <- name `sepBy` (tok LTokComma)
          vararg <- optionMaybe $ (try $ tok LTokEllipsis) <|> (tok LTokComma)
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
tableField = choice [ expField
                    , try namedField
                    , field
                    ]
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

boolExp = (tok LTokTrue >> return (Bool "true")) <|>
            (tok LTokFalse >> return (Bool "false"))

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
binary op fun assoc = Infix (tok op >> return fun) assoc

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
           , try prefixexpExp -- stucks in a loop
           , try tableconstExp
           --, binopExp
           --, unopExp
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
emptyStat = optionMaybe (tok LTokSemic) >> return ()

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
    name' <- name
    body <- funBody
    return $ FunAssign name' body

localFunAssignStat = do
    tok LTokLocal
    tok LTokFunction
    name' <- name
    body <- funBody
    return $ LocalFunAssign name' body

localAssignStat = do
    tok LTokLocal
    names <- name `sepBy` tok LTokComma
    rest <- optionMaybe $ (tok LTokAssign) >> exp `sepBy` (tok LTokComma)
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