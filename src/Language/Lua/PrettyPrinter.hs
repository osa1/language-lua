{-# OPTIONS_GHC -fno-warn-hi-shadowing
                -fno-warn-name-shadowing
                -fno-warn-unused-do-bind #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Lua pretty-printer.
module Language.Lua.PrettyPrinter
  ( pprint
  , renderPretty
  , displayS
  , displayIO
  , LPretty
  ) where

import Prelude hiding (EQ, GT, LT)
import Text.PrettyPrint.Leijen hiding ((<$>))

import Language.Lua.Syntax

intercalate :: Doc -> [Doc] -> Doc
intercalate s elems = sep (punctuate s elems)

infixr 5 <$>
(<$>) :: Doc -> Doc -> Doc
x <$> y | isEmpty y = x
        | otherwise = x <> line <> y

class LPretty a where
    pprint :: a -> Doc

instance LPretty [Char] where
    pprint = text

instance LPretty Bool where
    pprint True  = text "true"
    pprint False = text "false"

instance LPretty Exp where
    pprint Nil            = text "nil"
    pprint (Bool s)       = pprint s
    pprint (Number n)     = text n
    pprint (String s)     = dquotes (text s)
    pprint Vararg         = text "..."
    pprint (EFunDef f)    = pprint f
    pprint (PrefixExp pe) = pprint pe
    pprint (TableConst t) = pprint t
    pprint (Binop op e1 e2) = group (pprint e1 <+> pprint op <$> pprint e2)
    pprint (Unop op e)    = pprint op <> pprint e

instance LPretty Var where
    pprint (VarName n)          = pprint n
    pprint (Select pe e)        = pprint pe <> align (brackets (pprint e))
    pprint (SelectName pe name) = pprint pe <//> (char '.' <> pprint name)

instance LPretty Binop where
    pprint Add    = char '+'
    pprint Sub    = char '-'
    pprint Mul    = char '*'
    pprint Div    = char '/'
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

instance LPretty Unop where
    pprint Neg = char '-'
    pprint Not = text "not "
    pprint Len = char '#'

instance LPretty PrefixExp where
    pprint (PEVar var)         = pprint var
    pprint (PEFunCall funcall) = pprint funcall
    pprint (Paren e)           = parens (pprint e)

instance LPretty [TableField] where
    pprint fields = braces (align (fillSep (punctuate (comma <> space) (map pprint fields))))

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
                     Just e  -> nest 4 (text "return" </> intercalate comma (map (align . pprint) e))

instance LPretty FunName where
    pprint (FunName name s methods) = cat (punctuate dot (map pprint $ name:s)) <> method'
      where method' = case methods of
                        Nothing -> empty
                        Just m' -> char ':' <> pprint m'

instance LPretty FunBody where
    pprint = pprintFunction Nothing

pprintFunction :: Maybe Doc -> FunBody -> Doc
pprintFunction funname (FunBody args vararg block) =
    group (nest 4 (header <$> body) <$> end)
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
    pprint (Args exps)   = parens (align (fillSep (punctuate (comma <> space) (map (align . pprint) exps))))
    pprint (TableArg t)  = pprint t
    pprint (StringArg s) = dquotes (text s)

instance LPretty Stat where
    pprint (Assign names vals)
        =   intercalate comma (map pprint names)
        <+> equals
        <+> intercalate comma (map pprint vals)
    pprint (FunCall funcall) = pprint funcall
    pprint (Label name)      = text "::" <> pprint name <> text "::"
    pprint Break             = text "break"
    pprint (Goto name)       = text "goto" <+> pprint name
    pprint (Do block)        = group (nest 4 (text "do" <$> pprint block) <$> text "end")
    pprint (While guard e)
        =  text "while" <+> pprint guard <+> text "do"
       <$> indent 4 (pprint e)
       <$> text "end"
    pprint (Repeat block guard)
        =   (text "repeat" <$> indent 4 (pprint block))
        </> nest 4 (text "until" </> pprint guard)

    pprint (If cases elsePart) = group (printIf cases elsePart)
      where
        printIf ((guard, block) : xs) e =
          nest 4 (text "if" <+> pprint guard <+> text "then" <$> pprint block) <$> printIf' xs e

        printIf' [] Nothing  = text "end"
        printIf' [] (Just b) = nest 4 (text "else" <$> pprint b) <$> text "end"
        printIf' ((guard, block) : xs) e =
          nest 4 (text "elseif" <+> pprint guard <+> text "then" <$> pprint block) <$> printIf' xs e

    pprint (ForRange name e1 e2 e3 block)
        =   text "for" <+> pprint name <> equals <> pprint e1 <> comma <> pprint e2 <> e3' <+> text "do"
        <$> indent 4 (pprint block)
        <$> text "end"
      where e3' = case e3 of
                    Nothing -> empty
                    Just e  -> comma <> pprint e

    pprint (ForIn names exps block)
        =   text "for" <+> intercalate comma (map pprint names)
                <+> text "in" <+> intercalate comma (map pprint exps) <+> text "do"
        <$> indent 4 (pprint block)
        <$> text "end"

    pprint (FunAssign name body) = pprintFunction (Just (pprint name)) body
    pprint (LocalFunAssign name body) = text "local" <+> pprintFunction (Just (pprint name)) body
    pprint (LocalAssign names exps)
        = text "local" <+> intercalate comma (map pprint names) <+> exps'
      where exps' = case exps of
                      Nothing -> empty
                      Just es -> equals </> intercalate comma (map pprint es)
    pprint EmptyStat = empty
