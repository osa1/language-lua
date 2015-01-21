{-# LANGUAGE LambdaCase, TupleSections #-}

module Language.Lua.Annotated.Parser
  ( parseText
  , parseFile
  , stat
  , exp
  , chunk
  ) where

import           Control.Applicative           ((<$>), (<*), (<*>))
import           Control.Monad                 (liftM)
import           Prelude                       hiding (EQ, GT, LT, exp)
import           Text.Parsec                   hiding (string)
import           Text.Parsec.LTok

import           Language.Lua.Annotated.Lexer
import           Language.Lua.Annotated.Syntax
import           Language.Lua.Token

-- | Runs Lua lexer before parsing. Use @parseText stat@ to parse
-- statements, and @parseText exp@ to parse expressions.
parseText :: Parser a -> String -> Either ParseError a
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
    str <- anyIdent
    return $ Name pos str


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
  prefixexpExp, tableconstExp, exp :: Parser (Exp SourcePos)

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

type Binop' = Exp SourcePos -> Exp SourcePos -> Exp SourcePos
type Unop'  = Exp SourcePos -> Exp SourcePos

binop :: Parser (Binop', Int, Int)
binop = do
  pos <- getPosition
  choice
    [ tok LTokPlus >> return (Binop pos (Add pos), 10, 10)
    , tok LTokMinus >> return (Binop pos (Sub pos), 10, 10)
    , tok LTokStar >> return (Binop pos (Mul pos), 11, 11)
    , tok LTokSlash >> return (Binop pos (Div pos), 11, 11)
    , tok LTokExp >> return (Binop pos (Exp pos), 14, 13)
    , tok LTokPercent >> return (Binop pos (Mod pos), 11, 11)
    , tok LTokDDot >> return (Binop pos (Concat pos), 9, 8)
    , tok LTokLT >> return (Binop pos (LT pos), 3, 3)
    , tok LTokLEq >> return (Binop pos (LTE pos), 3, 3)
    , tok LTokGT >> return (Binop pos (GT pos), 3, 3)
    , tok LTokGEq >> return (Binop pos (GTE pos), 3, 3)
    , tok LTokEqual >> return (Binop pos (EQ pos), 3, 3)
    , tok LTokNotequal >> return (Binop pos (NEQ pos), 3, 3)
    , tok LTokAnd >> return (Binop pos (And pos), 2, 2)
    , tok LTokOr >> return (Binop pos (Or pos), 1, 1)
    ]

unop :: Parser (Unop', Int)
unop = do
    pos <- getPosition
    unopTok <- choice
      [ tok LTokMinus >> return Neg
      , tok LTokNot >> return Not
      , tok LTokSh >> return Len
      ]
    return (Unop pos (unopTok pos), 12)

subexp :: Int -> Parser (Exp SourcePos, Maybe (Binop', Int, Int))
subexp limit = do
    (e1, bop) <- optionMaybe unop >>=
                   \case Nothing -> (, Nothing) <$> simpleExp
                         Just (uop, uopPri) -> do
                           (e1, bop) <- subexp uopPri
                           return (uop e1, bop)
    maybe (optionMaybe binop) (return . Just) bop >>= loop e1
  where
    loop e1 Nothing = return (e1, Nothing)
    loop e1 (Just b@(bop, bopPriL, bopPriR))
      | bopPriL > limit = do
          (e2, nextOp) <- subexp bopPriR
          loop (bop e1 e2) nextOp
      | otherwise = return (e1, Just b)

simpleExp :: Parser (Exp SourcePos)
simpleExp = choice [ nilExp, boolExp, numberExp, stringExp, varargExp,
                     fundefExp, prefixexpExp, tableconstExp ]

-- | Expression parser.
exp = fst <$> subexp 0

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
