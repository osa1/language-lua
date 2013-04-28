{-# OPTIONS_GHC -Wall
                -fno-warn-hi-shadowing
                -fno-warn-name-shadowing
                -fno-warn-unused-do-bind #-}
module Language.Lua.Parser
  ( parseText
  , parseFile
  , stat
  , exp
  , chunk
  ) where

import Prelude hiding (exp, LT, GT, EQ, repeat)

import Language.Lua.Lexer
import Language.Lua.Token
import Language.Lua.Types

import Text.Parsec hiding (string)
import Text.Parsec.LTok
import Text.Parsec.Expr
import Control.Applicative ((<*), (<$>), (<*>))
import Control.Monad (void, liftM)

-- | Runs Lua lexer before parsing. Use @parseText stat@ to parse
-- statements, and @parseText exp@ to parse expressions.
parseText :: Parsec [LTok] () a -> String -> Either ParseError a
parseText p s = parse p "<string>" (llex s)

-- | Parse a Lua file. You can use @parseText chunk@ to parse a file from a string.
parseFile :: FilePath -> IO (Either ParseError Block)
parseFile = liftM (parseText chunk) . readFile
parseFile path = parse chunk path . llex <$> readFile path

parens :: Monad m => ParsecT [LTok] u m a -> ParsecT [LTok] u m a
parens = between (tok LTokLParen) (tok LTokRParen)

brackets :: Monad m => ParsecT [LTok] u m a -> ParsecT [LTok] u m a
brackets = between (tok LTokLBracket) (tok LTokRBracket)

name :: Parser String
name = tokenValue <$> anyIdent

number :: Parser String
number = tokenValue <$> anyNum


data PrimaryExp
    = PName Name
    | PParen Exp
    deriving (Show, Eq)

data SuffixedExp
    = SuffixedExp PrimaryExp [SuffixExp]
    deriving (Show, Eq)

data SuffixExp
    = SSelect Name
    | SSelectExp Exp
    | SSelectMethod Name FunArg
    | SFunCall FunArg
    deriving (Show, Eq)

primaryExp :: Parser PrimaryExp
primaryExp = (PName <$> name) <|> (liftM PParen $ parens exp)

suffixedExp :: Parser SuffixedExp
suffixedExp = SuffixedExp <$> primaryExp <*> many suffixExp

suffixExp :: Parser SuffixExp
suffixExp = selectName <|> selectExp <|> selectMethod <|> funarg
  where selectName   = SSelect <$> (tok LTokDot >> name)
        selectExp    = SSelectExp <$> brackets exp
        selectMethod = tok LTokColon >> (SSelectMethod <$> name <*> funArg)
        funarg       = SFunCall <$> funArg

sexpToPexp :: SuffixedExp -> PrefixExp
sexpToPexp (SuffixedExp t r) = case r of
    []                            -> t'
    (SSelect sname:xs)            -> iter xs (PEVar (SelectName t' sname))
    (SSelectExp sexp:xs)          -> iter xs (PEVar (Select t' sexp))
    (SSelectMethod mname args:xs) -> iter xs (PEFunCall (MethodCall t' mname args))
    (SFunCall args:xs)            -> iter xs (PEFunCall (NormalFunCall t' args))

  where t' :: PrefixExp
        t' = case t of
               PName name -> PEVar (Name name)
               PParen exp -> Paren exp

        iter :: [SuffixExp] -> PrefixExp -> PrefixExp
        iter [] pe                            = pe
        iter (SSelect sname:xs) pe            = iter xs (PEVar (SelectName pe sname))
        iter (SSelectExp sexp:xs) pe          = iter xs (PEVar (Select pe sexp))
        iter (SSelectMethod mname args:xs) pe = iter xs (PEFunCall (MethodCall pe mname args))
        iter (SFunCall args:xs) pe            = iter xs (PEFunCall (NormalFunCall pe args))

-- TODO: improve error messages.
sexpToVar :: SuffixedExp -> Parser Var
sexpToVar (SuffixedExp (PName name) []) = return (Name name)
sexpToVar (SuffixedExp _ []) = fail "syntax error"
sexpToVar sexp = case sexpToPexp sexp of
                   PEVar var -> return var
                   _ -> fail "syntax error"

sexpToFunCall :: SuffixedExp -> Parser FunCall
sexpToFunCall (SuffixedExp _ []) = fail "syntax error"
sexpToFunCall sexp = case sexpToPexp sexp of
                       PEFunCall funcall -> return funcall
                       _ -> fail "syntax error"

var :: Parser Var
var = suffixedExp >>= sexpToVar

funCall :: Parser FunCall
funCall = suffixedExp >>= sexpToFunCall

stringlit :: Parser String
stringlit = tokenValue <$> string

funArg :: Parser FunArg
funArg = tableArg <|> stringArg <|> parlist
  where tableArg  = TableArg <$> table
        stringArg = StringArg <$> stringlit
        parlist   = parens (do exps <- exp `sepBy` tok LTokComma
                               return $ Args exps)

funBody :: Parser FunBody
funBody = do
    (params, vararg) <- parlist
    body <- block
    tok LTokEnd
    return $ FunBody params vararg body

  where parlist = parens $ do
          vars <- name `sepEndBy` tok LTokComma
          vararg <- optionMaybe (tok LTokEllipsis <|> tok LTokComma)
          return $ case vararg of
                       Nothing -> (vars, False)
                       Just LTokEllipsis -> (vars, True)
                       _ -> (vars, False)

block :: Parser Block
block = do
  stats <- many stat
  ret <- optionMaybe retstat
  return $ Block stats ret

retstat :: Parser [Exp]
retstat = do
  tok LTokReturn
  exps <- exp `sepBy` tok LTokComma
  optional (tok LTokSemic)
  return exps

tableField :: Parser TableField
tableField = choice [ expField, try namedField, field ]
  where expField :: Parser TableField
        expField = do
            e1 <- brackets exp
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

prefixexpExp = PrefixExp <$> (liftM sexpToPexp suffixedExp)

tableconstExp = TableConst <$> table

binary :: Monad m => LToken -> (a -> a -> a) -> Assoc -> Operator [LTok] u m a
binary op fun = Infix (tok op >> return fun)

prefix :: Monad m => LToken -> (a -> a) -> Operator [LTok] u m a
prefix op fun = Prefix (tok op >> return fun)

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

exp' = choice [ nilExp, boolExp, numberExp, stringExp, varargExp,
                fundefExp, prefixexpExp, tableconstExp ]

-- | Expression parser.
exp = choice [ opExp, nilExp, boolExp, numberExp, stringExp, varargExp,
               fundefExp, prefixexpExp, tableconstExp ]

-----------------------------------------------------------------------
---- Statements

emptyStat, assignStat, funCallStat, labelStat, breakStat, gotoStat,
    doStat, whileStat, repeatStat, ifStat, forRangeStat, forInStat,
    funAssignStat, localFunAssignStat, localAssignStat, stat :: Parser Stat

emptyStat = void (tok LTokSemic) >> return EmptyStat

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
                          <*> many (tok LTokDot >> name)
                          <*> optionMaybe (tok LTokColon >> name)

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

-- | Statement parser.
stat =
  choice [ emptyStat
         , try assignStat
         , try funCallStat
         , labelStat
         , breakStat
         , gotoStat
         , doStat
         , whileStat
         , repeatStat
         , ifStat
         , try forRangeStat
         , forInStat
         , funAssignStat
         , try localFunAssignStat
         , localAssignStat
         ]

-- | Lua file parser.
chunk :: Parser Block
chunk = block <* tok LTokEof
