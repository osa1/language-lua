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
import Control.Monad (liftM)

-- | Runs Lua lexer before parsing. Use @parseText stat@ to parse
-- statements, and @parseText exp@ to parse expressions.
parseText :: Parsec [LTok] () a -> String -> Either ParseError a
parseText p s = parse p "<string>" (llex s)

-- | Parse a Lua file. You can use @parseText chunk@ to parse a file from a string.
parseFile :: FilePath -> IO (Either ParseError (Block SourcePos))
parseFile path = parse chunk path . llex <$> readFile path

parens :: Monad m => ParsecT [LTok] u m a -> ParsecT [LTok] u m a
parens = between (tok LTokLParen) (tok LTokRParen)

brackets :: Monad m => ParsecT [LTok] u m a -> ParsecT [LTok] u m a
brackets = between (tok LTokLBracket) (tok LTokRBracket)

name :: Parser (Name SourcePos)
name = do
    pos <- getPosition
    str <- tokenValue <$> anyIdent
    return $ Name pos str

number :: Parser String
number = tokenValue <$> anyNum


data PrimaryExp a
    = PName a (Name a)
    | PParen a (Exp a)
    deriving (Show, Eq)

data SuffixedExp a
    = SuffixedExp a (PrimaryExp a) [SuffixExp a]
    deriving (Show, Eq)

data SuffixExp a
    = SSelect a (Name a)
    | SSelectExp a (Exp a)
    | SSelectMethod a (Name a) (FunArg a)
    | SFunCall a (FunArg a)
    deriving (Show, Eq)

primaryExp :: Parser (PrimaryExp SourcePos)
primaryExp = do
    pos <- getPosition
    PName pos <$> name <|> PParen pos <$> parens exp

suffixedExp :: Parser (SuffixedExp SourcePos)
suffixedExp = SuffixedExp <$> getPosition <*> primaryExp <*> many suffixExp

suffixExp :: Parser (SuffixExp SourcePos)
suffixExp = selectName <|> selectExp <|> selectMethod <|> funarg
  where selectName   = SSelect <$> getPosition <*> (tok LTokDot >> name)
        selectExp    = SSelectExp <$> getPosition <*> brackets exp
        selectMethod = do
          pos <- getPosition
          tok LTokColon
          SSelectMethod pos <$> name <*> funArg
        funarg       = SFunCall <$> getPosition <*> funArg

sexpToPexp :: SuffixedExp SourcePos -> PrefixExp SourcePos
sexpToPexp (SuffixedExp _ t r) = case r of
    []                                -> t'
    (SSelect pos sname:xs)            -> iter xs (PEVar     pos (SelectName pos t' sname))
    (SSelectExp pos sexp:xs)          -> iter xs (PEVar     pos (Select pos t' sexp))
    (SSelectMethod pos mname args:xs) -> iter xs (PEFunCall pos (MethodCall pos t' mname args))
    (SFunCall pos args:xs)            -> iter xs (PEFunCall pos (NormalFunCall pos t' args))

  where t' :: PrefixExp SourcePos
        t' = case t of
               PName pos name -> PEVar pos (VarName pos name)
               PParen pos exp -> Paren pos exp

        iter :: [SuffixExp SourcePos] -> PrefixExp SourcePos -> PrefixExp SourcePos
        iter [] pe                                = pe
        iter (SSelect pos sname:xs) pe            = iter xs (PEVar pos (SelectName pos pe sname))
        iter (SSelectExp pos sexp:xs) pe          = iter xs (PEVar pos (Select pos pe sexp))
        iter (SSelectMethod pos mname args:xs) pe = iter xs (PEFunCall pos (MethodCall pos pe mname args))
        iter (SFunCall pos args:xs) pe            = iter xs (PEFunCall pos (NormalFunCall pos pe args))

-- TODO: improve error messages.
sexpToVar :: SuffixedExp SourcePos -> Parser (Var SourcePos)
sexpToVar (SuffixedExp pos (PName _ name) []) = return (VarName pos name)
sexpToVar (SuffixedExp _ _ []) = fail "syntax error"
sexpToVar sexp = case sexpToPexp sexp of
                   PEVar _ var -> return var
                   _ -> fail "syntax error"

sexpToFunCall :: SuffixedExp SourcePos -> Parser (FunCall SourcePos)
sexpToFunCall (SuffixedExp _ _ []) = fail "syntax error"
sexpToFunCall sexp = case sexpToPexp sexp of
                       PEFunCall _ funcall -> return funcall
                       _ -> fail "syntax error"

var :: Parser (Var SourcePos)
var = suffixedExp >>= sexpToVar

funCall :: Parser (FunCall SourcePos)
funCall = suffixedExp >>= sexpToFunCall

stringlit :: Parser String
stringlit = tokenValue <$> string

funArg :: Parser (FunArg SourcePos)
funArg = tableArg <|> stringArg <|> arglist
  where tableArg  = TableArg <$> getPosition <*> table
        stringArg = StringArg <$> getPosition <*> stringlit
        arglist   = do
          pos <- getPosition
          parens (do exps <- exp `sepBy` tok LTokComma
                     return $ Args pos exps)

funBody :: Parser (FunBody SourcePos)
funBody = do
    pos <- getPosition
    (params, vararg) <- arglist
    body <- block
    tok LTokEnd
    return $ FunBody pos params vararg body

  where lastarg = do
          pos <- getPosition
          arg <- optionMaybe (tok LTokEllipsis <|> tok LTokComma)
          case arg of
            Just LTokEllipsis -> return (Just pos)
            _ -> return Nothing

        arglist = parens $ do
          vars <- name `sepEndBy` tok LTokComma
          vararg <- lastarg
          return (vars, vararg)

block :: Parser (Block SourcePos)
block = do
  pos <- getPosition
  stats <- many stat
  ret <- optionMaybe retstat
  return $ Block pos stats ret

retstat :: Parser [Exp SourcePos]
retstat = do
  tok LTokReturn
  exps <- exp `sepBy` tok LTokComma
  optional (tok LTokSemic)
  return exps

tableField :: Parser (TableField SourcePos)
tableField = choice [ expField, try namedField, field ]
  where expField :: Parser (TableField SourcePos)
        expField = do
            pos <- getPosition
            e1 <- brackets exp
            tok LTokAssign
            e2 <- exp
            return $ ExpField pos e1 e2

        namedField :: Parser (TableField SourcePos)
        namedField = do
            pos <- getPosition
            name' <- name
            tok LTokAssign
            val <- exp
            return $ NamedField pos name' val

        field :: Parser (TableField SourcePos)
        field = Field <$> getPosition <*> exp

table :: Parser (Table SourcePos)
table = do
    pos <- getPosition
    between (tok LTokLBrace)
            (tok LTokRBrace)
            (do fields <- tableField `sepEndBy` fieldSep
                return $ Table pos fields)
  where fieldSep = tok LTokComma <|> tok LTokSemic

-----------------------------------------------------------------------
---- Expressions

nilExp, boolExp, numberExp, stringExp, varargExp, fundefExp,
  prefixexpExp, tableconstExp, exp, exp' :: Parser (Exp SourcePos)

nilExp = (Nil <$> getPosition) <* tok LTokNil

boolExp = do
    pos <- getPosition
    tOrF <- tok LTokTrue <|> tok LTokFalse
    return $ Bool pos (tOrF == LTokTrue)

numberExp = Number <$> getPosition <*> number

stringExp = String <$> getPosition <*> stringlit

varargExp = (Vararg <$> getPosition) <* tok LTokEllipsis

fundefExp = do
  pos <- getPosition
  tok LTokFunction
  body <- funBody
  return $ EFunDef pos (FunDef (ann body) body)

prefixexpExp = PrefixExp <$> getPosition <*> liftM sexpToPexp suffixedExp

tableconstExp = TableConst <$> getPosition <*> table

binary :: Monad m => LToken -> (SourcePos -> a -> a -> a) -> Assoc -> Operator [LTok] u m a
binary op fun = Infix (do pos <- getPosition; tok op; return $ fun pos)

prefix :: Monad m => LToken -> (SourcePos -> a -> a) -> Operator [LTok] u m a
prefix op fun = Prefix (do pos <- getPosition; tok op; return $ fun pos)

opTable :: Monad m => SourcePos -> [[Operator [LTok] u m (Exp SourcePos)]]
opTable pos = [ [ binary LTokExp       (Binop pos . Exp)    AssocRight ]
              , [ prefix LTokNot       (Unop pos . Not)
                , prefix LTokSh        (Unop pos . Len)
                , prefix LTokMinus     (Unop pos . Neg)
                ]
              , [ binary LTokStar      (Binop pos . Mul)    AssocLeft
                , binary LTokSlash     (Binop pos . Div)    AssocLeft
                , binary LTokPercent   (Binop pos . Mod)    AssocLeft
                ]
              , [ binary LTokPlus      (Binop pos . Add)    AssocLeft
                , binary LTokMinus     (Binop pos . Sub)    AssocLeft
                ]
              , [ binary LTokDDot      (Binop pos . Concat) AssocRight ]
              , [ binary LTokGT        (Binop pos . GT)     AssocLeft
                , binary LTokLT        (Binop pos . LT)     AssocLeft
                , binary LTokGEq       (Binop pos . GTE)    AssocLeft
                , binary LTokLEq       (Binop pos . LTE)    AssocLeft
                , binary LTokNotequal  (Binop pos . NEQ)    AssocLeft
                , binary LTokEqual     (Binop pos . EQ)     AssocLeft
                ]
              , [ binary LTokAnd       (Binop pos . And)    AssocLeft ]
              , [ binary LTokOr        (Binop pos . Or)     AssocLeft ]
              ]
opExp :: SourcePos -> Parser (Exp SourcePos)
opExp pos = buildExpressionParser (opTable pos) exp' <?> "opExp"

exp' = choice [ nilExp, boolExp, numberExp, stringExp, varargExp,
                fundefExp, prefixexpExp, tableconstExp ]

-- | Expression parser.
exp = choice [ opExp =<< getPosition, nilExp, boolExp, numberExp, stringExp, varargExp,
               fundefExp, prefixexpExp, tableconstExp ]

-----------------------------------------------------------------------
---- Statements

emptyStat, assignStat, funCallStat, labelStat, breakStat, gotoStat,
    doStat, whileStat, repeatStat, ifStat, forRangeStat, forInStat,
    funAssignStat, localFunAssignStat, localAssignStat, stat :: Parser (Stat SourcePos)

emptyStat = (EmptyStat <$> getPosition) <* tok LTokSemic

assignStat = do
  pos <- getPosition
  vars <- var `sepBy` tok LTokComma
  tok LTokAssign
  exps <- exp `sepBy` tok LTokComma
  return $ Assign pos vars exps

funCallStat = FunCall <$> getPosition <*> funCall

labelStat = Label <$> getPosition <*> label
  where label = between (tok LTokDColon) (tok LTokDColon) name

breakStat = (Break <$> getPosition) <* tok LTokBreak

gotoStat = Goto <$> getPosition <*> (tok LTokGoto >> name)

doStat = Do <$> getPosition <*> between (tok LTokDo) (tok LTokEnd) block

whileStat = do
  pos <- getPosition
  between (tok LTokWhile)
          (tok LTokEnd)
          (do cond <- exp
              tok LTokDo
              body <- block
              return $ While pos cond body)

repeatStat = do
  pos <- getPosition
  tok LTokRepeat
  body <- block
  tok LTokUntil
  cond <- exp
  return $ Repeat pos body cond

ifStat = do
    pos <- getPosition
    between (tok LTokIf)
            (tok LTokEnd)
            (do f <- ifPart
                conds <- many elseifPart
                l <- optionMaybe elsePart
                return $ If pos (f:conds) l)

  where ifPart :: Parser (Exp SourcePos, Block SourcePos)
        ifPart = cond

        elseifPart :: Parser (Exp SourcePos, Block SourcePos)
        elseifPart = tok LTokElseIf >> cond

        cond :: Parser (Exp SourcePos, Block SourcePos)
        cond = do
            cond <- exp
            tok LTokThen
            body <- block
            return (cond, body)

        elsePart :: Parser (Block SourcePos)
        elsePart = tok LTokElse >> block

forRangeStat = do
  pos <- getPosition
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
              return $ ForRange pos name' start end range body)

forInStat = do
  pos <- getPosition
  between (tok LTokFor)
          (tok LTokEnd)
          (do names <- name `sepBy` tok LTokComma
              tok LTokIn
              exps <- exp `sepBy` tok LTokComma
              tok LTokDo
              body <- block
              return $ ForIn pos names exps body)

funAssignStat = do
    pos <- getPosition
    tok LTokFunction
    name' <- funName
    body <- funBody
    return $ FunAssign pos name' body
  where funName :: Parser (FunName SourcePos)
        funName = FunName <$> getPosition
                          <*> name
                          <*> many (tok LTokDot >> name)
                          <*> optionMaybe (tok LTokColon >> name)

localFunAssignStat = do
  pos <- getPosition
  tok LTokLocal
  tok LTokFunction
  name' <- name
  body <- funBody
  return $ LocalFunAssign pos name' body

localAssignStat = do
  pos <- getPosition
  tok LTokLocal
  names <- name `sepBy` tok LTokComma
  rest <- optionMaybe $ tok LTokAssign >> exp `sepBy` tok LTokComma
  return $ LocalAssign pos names rest

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
chunk :: Parser (Block SourcePos)
chunk = block <* tok LTokEof
