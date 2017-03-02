{-# LANGUAGE FlexibleInstances, CPP #-}

-- | Lua pretty-printer.
module Language.Lua.PrettyPrinter
  ( pprint
  , renderPretty
  , displayS
  , displayIO
  , LPretty
  ) where

#if __GLASGOW_HASKELL__ >= 710
import           Prelude                 hiding (EQ, GT, LT, (<$>))
#else
import           Prelude                 hiding (EQ, GT, LT)
#endif

import           Text.PrettyPrint.Leijen hiding ((<$>))

import           Language.Lua.Syntax

intercalate :: Doc -> [Doc] -> Doc
intercalate s elems = sep (punctuate s elems)

infixr 5 <$>
(<$>) :: Doc -> Doc -> Doc
x <$> y | isEmpty y = x
        | otherwise = x <> line <> y

type Precedence = Int

class LPretty a where
    pprint :: a -> Doc
    pprint = pprint' 0

    pprint' :: Precedence -> a -> Doc
    pprint' _ = pprint

instance LPretty [Char] where
    pprint = text

instance LPretty Bool where
    pprint True  = text "true"
    pprint False = text "false"

instance LPretty Exp where
    pprint' _ Nil            = text "nil"
    pprint' _ (Bool s)       = pprint s
    pprint' _ (Number n)     = text n
    pprint' _ (String s)     = text (show s)
    pprint' _ Vararg         = text "..."
    pprint' _ (EFunDef f)    = pprint f
    pprint' _ (PrefixExp pe) = pprint pe
    pprint' _ (TableConst t) = pprint t
    pprint' p (Binop op e1 e2) = ps (pprint' opPrecL e1 <+> pprint op
                                                        <+> case e2 of
                                                              Unop{} -> pprint e2
                                                              _ -> pprint' opPrecR e2)
      where
        (opPrecL, opPrecR) = getBinopPrec op
        ps = if min opPrecL opPrecR < p then parens else id

    -- We handle this as a special case: When we have a chain of negations, we
    -- should put a space between operators, otherwise we end up printing a
    -- comment.
    --
    -- One another solution would be to always put a space after negation, but I
    -- like to put negation just before the expression, without any spaces.
    pprint' p (Unop Neg (Unop Neg e)) =
        ps (pprint Neg <+> pprint' opPrec (Unop Neg e))
      where
        opPrec = getUnopPrec Neg
        ps = if opPrec < p then parens else id

    pprint' p (Unop op e)    = ps (pprint op <> pprint' opPrec e)
      where
        opPrec = getUnopPrec op
        ps = if opPrec < p then parens else id

instance LPretty Var where
    pprint (VarName n)          = pprint n
    pprint (Select pe e)        = pprint pe <> align (brackets (pprint e))
    pprint (SelectName pe name) = pprint pe <//> (char '.' <> pprint name)

instance LPretty Binop where
    pprint Add    = char '+'
    pprint Sub    = char '-'
    pprint Mul    = char '*'
    pprint Div    = char '/'
    pprint IDiv   = text "//"
    pprint Exp    = char '^'
    pprint Mod    = char '%'
    pprint Concat = text ".."
    pprint LT     = char '<'
    pprint LTE    = text "<="
    pprint GT     = char '>'
    pprint GTE    = text ">="
    pprint EQ     = text "=="
    pprint NEQ    = text "~="
    pprint And    = text "and"
    pprint Or     = text "or"
    pprint BAnd   = char '&'
    pprint BOr    = char '|'
    pprint BXor   = char '~'
    pprint ShiftL = text "<<"
    pprint ShiftR = text ">>"

instance LPretty Unop where
    pprint Neg = char '-'
    pprint Not = text "not "
    pprint Len = char '#'
    pprint Complement = char '~'

getBinopPrec :: Binop -> (Precedence, Precedence)
getBinopPrec op =
    case op of
      Add -> (10, 10)
      Sub -> (10, 10)
      Mul -> (11, 11)
      Div -> (11, 11)
      IDiv -> (11, 11)
      Exp -> (14, 13)
      Mod -> (11, 11)
      Concat -> (9, 8)
      ShiftL -> (7, 7)
      ShiftR -> (7, 7)
      BAnd -> (6, 6)
      BXor -> (5, 5)
      BOr -> (4, 4)
      LT -> (3, 3)
      LTE -> (3, 3)
      GT -> (3, 3)
      GTE -> (3, 3)
      EQ -> (3, 3)
      NEQ -> (3, 3)
      And -> (2, 2)
      Or -> (1, 1)

getUnopPrec :: Unop -> Precedence
getUnopPrec = const 12

instance LPretty PrefixExp where
    pprint (PEVar var)         = pprint var
    pprint (PEFunCall funcall) = pprint funcall
    pprint (Paren e)           = parens (pprint e)

instance LPretty [TableField] where
    pprint fields = braces (align (fillSep (punctuate comma (map pprint fields))))

instance LPretty TableField where
    pprint (ExpField e1 e2)    = brackets (pprint e1) <+> equals <+> pprint e2
    pprint (NamedField name e) = pprint name <+> equals <+> pprint e
    pprint (Field e)           = pprint e

instance LPretty Block where
    pprint (Block stats ret) =
      case stats of
        [] -> ret'
        _  -> vsep (map pprint stats) <$> ret'
      where ret' = case ret of
                     Nothing -> empty
                     Just [fun@EFunDef{}] -> text "return" <+> pprint fun
                     Just e  -> nest 2 (text "return" </> intercalate comma (map (align . pprint) e))

instance LPretty FunName where
    pprint (FunName name s methods) = cat (punctuate dot (map pprint $ name:s)) <> method'
      where method' = case methods of
                        Nothing -> empty
                        Just m' -> char ':' <> pprint m'

instance LPretty FunBody where
    pprint = pprintFunction Nothing

pprintFunction :: Maybe Doc -> FunBody -> Doc
pprintFunction funname (FunBody args vararg block) =
    group (nest 2 (header <$> body) <$> end)
  where
    header = case funname of
               Nothing -> text "function" <+> args'
               Just n  -> text "function" <+> n <> args'
    vararg' = if vararg then ["..."] else []
    args' = parens (align (cat (punctuate (comma <> space) (map pprint (args ++ vararg')))))
    body = pprint block
    end = text "end"

instance LPretty FunCall where
    pprint (NormalFunCall pe arg)     = pprint pe <> pprint arg
    pprint (MethodCall pe method arg) = pprint pe <//> colon <> pprint method <> pprint arg

instance LPretty FunArg where
    pprint (Args [fun@EFunDef{}]) = parens (pprint fun)
    pprint (Args exps)   = parens (align (fillSep (punctuate comma (map (align . pprint) exps))))
    pprint (TableArg t)  = pprint t
    pprint (StringArg s) = text s

instance LPretty Stat where
    pprint (Assign names vals)
        =   intercalate comma (map pprint names)
        <+> equals
        <+> intercalate comma (map pprint vals)
    pprint (FunCall funcall) = pprint funcall
    pprint (Label name)      = text "::" <> pprint name <> text "::"
    pprint Break             = text "break"
    pprint (Goto name)       = text "goto" <+> pprint name
    pprint (Do block)        = group (nest 2 (text "do" <$> pprint block) <$> text "end")
    pprint (While guard e)
        =  nest 2 (text "while" <+> pprint guard <+> text "do" <$> pprint e)
       <$> text "end"
    pprint (Repeat block guard)
        =   nest 2 (text "repeat" <$> pprint block)
        </> nest 2 (text "until" </> pprint guard)

    pprint (If cases elsePart) = group (printIf cases elsePart)
      where
        printIf ((guard, block) : xs) e =
          nest 2 (text "if" <+> pprint guard <+> text "then" <$> pprint block) <$> printIf' xs e
        printIf [] _ =
          error $ "pprint: Trying to print invalid syntax:\n\t" ++
                  "if statement should have at least one case"

        printIf' [] Nothing  = text "end"
        printIf' [] (Just b) = nest 2 (text "else" <$> pprint b) <$> text "end"
        printIf' ((guard, block) : xs) e =
          nest 2 (text "elseif" <+> pprint guard <+> text "then" <$> pprint block) <$> printIf' xs e

    pprint (ForRange name e1 e2 e3 block)
        =   nest 2 (text "for" <+> pprint name <> equals <> pprint e1
                      <> comma <> pprint e2 <> e3' <+> text "do"
                      <$> pprint block)
        <$> text "end"
      where e3' = case e3 of
                    Nothing -> empty
                    Just e  -> comma <> pprint e

    pprint (ForIn names exps block)
        =   nest 2 (text "for" <+> intercalate comma (map pprint names) <+> text "in"
                     <+> intercalate comma (map pprint exps) <+> text "do"
                     <$> pprint block)
        <$> text "end"

    pprint (FunAssign name body) = pprintFunction (Just (pprint name)) body
    pprint (LocalFunAssign name body) = text "local" <+> pprintFunction (Just (pprint name)) body
    pprint (LocalAssign names exps)
        = text "local" <+> intercalate comma (map pprint names) <+> exps'
      where exps' = case exps of
                      Nothing -> empty
                      Just es -> equals </> intercalate comma (map pprint es)
    pprint EmptyStat = text ";"
