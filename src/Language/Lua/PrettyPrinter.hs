{-# OPTIONS_GHC -Wall
                -fno-warn-hi-shadowing
                -fno-warn-name-shadowing
                -fno-warn-unused-do-bind #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Lua.PrettyPrinter where

import Prelude hiding (EQ, GT, LT)
import Text.PrettyPrint.HughesPJ

import Language.Lua.Types

data Printer = Printer { ident :: Int }

intercalate :: Doc -> [Doc] -> Doc
intercalate sep elems = hsep (punctuate sep elems)

class Pretty a where
    pprint :: Printer -> a -> Doc

instance Pretty [Char] where
    pprint _ s = text s

instance Pretty Exp where
    pprint _ Nil              = text "nil"
    pprint _ (Bool s)         = text s
    pprint _ (Number n)       = text n
    pprint _ (String s)       = doubleQuotes (text s)
    pprint _ Vararg           = text "..."
    pprint p (EFunDef f)      = pprint p f
    pprint p (PrefixExp pe)   = pprint p pe
    pprint p (TableConst t)   = pprint p t
    pprint p (Binop op e1 e2) = pprint p e1 <+> pprint p op <+> pprint p e2
    pprint p (Unop op e)      = pprint p op <> pprint p e

instance Pretty Var where
    pprint _ (Name n)             = text n
    pprint p (Select pe e)        = pprint p pe <> brackets (pprint p e)
    pprint p (SelectName pe name) = pprint p pe <> char '.' <> pprint p name

instance Pretty Binop where
    pprint _ Add    = char '+'
    pprint _ Sub    = char '-'
    pprint _ Mul    = char '*'
    pprint _ Div    = char '/'
    pprint _ Exp    = char '^'
    pprint _ Mod    = char '%'
    pprint _ Concat = text ".."
    pprint _ LT     = char '<'
    pprint _ LTE    = text "<="
    pprint _ GT     = char '>'
    pprint _ GTE    = text ">="
    pprint _ EQ     = text "=="
    pprint _ NEQ    = text "~="
    pprint _ And    = text "and"
    pprint _ Or     = text "or"

instance Pretty Unop where
    pprint _ Neg = char '-'
    pprint _ Not = text "not"
    pprint _ Len = char '#'

instance Pretty PrefixExp where
    pprint p (PEVar var)         = pprint p var
    pprint p (PEFunCall funcall) = pprint p funcall
    pprint p (Paren e)           = parens (pprint p e)

instance Pretty Table where
    pprint p (Table fields) = braces (intercalate comma (map (pprint p) fields))

instance Pretty TableField where
    pprint p (ExpField e1 e2)    = brackets (pprint p e1) <+> equals <+> pprint p e2
    pprint p (NamedField name e) = pprint p name <+> equals <+> pprint p e
    pprint p (Field e)           = pprint p e

instance Pretty Block where
    pprint p (Block stats ret)
        = (foldr ($+$) empty (map (pprint p) stats)) $+$ ret'
      where ret' = case ret of
                     Nothing -> empty
                     Just e -> text "return" <+> (intercalate comma (map (pprint p) e))

instance Pretty FunName where
    pprint p (FunName name s methods) = text name <> s' <> (intercalate colon (map (pprint p) methods))
      where s' = case s of
                   Nothing -> empty
                   Just s' -> char '.' <> text s'

instance Pretty FunDef where
    pprint p (FunDef body) = pprint p body

instance Pretty FunBody where
    pprint p (FunBody args vararg block)
        =   parens (intercalate comma (map (pprint p) args))
        <>  vararg'
        $+$ nest 4 (pprint p block)
        $+$ text "end"
      where vararg' = if vararg then comma <+> text "..." else empty

instance Pretty FunCall where
    pprint p (NormalFunCall pe arg)     = pprint p pe <> parens (pprint p arg)
    pprint p (MethodCall pe method arg) = pprint p pe <> colon <> text method <> parens (pprint p arg)

instance Pretty FunArg where
    pprint p (Args exps)   = parens (intercalate comma (map (pprint p) exps))
    pprint p (TableArg t)  = pprint p t
    pprint _ (StringArg s) = doubleQuotes (text s)

instance Pretty Stat where
    pprint p (Assign names vals)
        =   (intercalate comma (map (pprint p) names))
        <+> equals
        <+> (intercalate comma (map (pprint p) vals))
    pprint p (FunCall funcall) = pprint p funcall
    pprint p (Label name)      = text ":::" <> text name <> text ":::"
    pprint p Break             = text "break"
    pprint p (Goto name)       = text "goto" <+> text name
    pprint p (Do block)        = text "do" $+$ nest 4 (pprint p block) $+$ text "end"
    pprint p (While guard e)
        =   text "while" <+> pprint p guard
        $+$ nest 4 (pprint p e)
        $+$ text "end"
    pprint p (Repeat block guard)
        = text "repeat" <+> pprint p block <+> text "until" <+> pprint p guard

    pprint p (If cases elsePart) = printIf cases elsePart
      where printIf ((guard, block):xs) e
                =   text "if" <+> pprint p guard <+> text "then"
                $+$ pprint p block
                $+$ printIf' p xs e

            printIf' p [] Nothing  = nest (-4) (text "end")
            printIf' p [] (Just b) =   nest (-4) (text "else")
                                   $+$ pprint p b
                                   $+$ nest (-4) (text "end")
            printIf' p ((guard, block):xs) e
                =   nest (-4) (text "elseif") <+> pprint p guard <+> text "then"
                $+$ pprint p block
                $+$ printIf' p xs e

    pprint p (ForRange name e1 e2 e3 block)
        =   text "for" <+> text name <> equals <> pprint p e1 <> comma <> pprint p e2 <> e3' <+> text "do"
        $+$ nest 4 (pprint p block)
        $+$ text "end"
      where e3' = case e3 of
                    Nothing -> empty
                    Just e  -> comma <> pprint p e

    pprint p (ForIn names exps block)
        =   text "for" <+> (intercalate comma (map (pprint p) names))
                <+> text "in" <+> (intercalate comma (map (pprint p) exps)) <+> text "do"
        $+$ nest 4 (pprint p block)
        $+$ text "end"

    pprint p (FunAssign name body) = text "function" <+> pprint p name <+> pprint p body
    pprint p (LocalFunAssign name body) = text "local function" <+> text name <+> pprint p body
    pprint p (LocalAssign names exps) = undefined -- TODO
    pprint p EmptyStat = empty