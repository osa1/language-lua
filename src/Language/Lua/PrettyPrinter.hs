{-# OPTIONS_GHC -Wall
                -fno-warn-hi-shadowing
                -fno-warn-name-shadowing
                -fno-warn-unused-do-bind #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Lua pretty-printer.
module Language.Lua.PrettyPrinter (pprint, LPretty) where

import Prelude hiding (EQ, GT, LT)
import Text.PrettyPrint.Leijen hiding ((<$>))

import Language.Lua.Types

intercalate :: Doc -> [Doc] -> Doc
intercalate s elems = sep (punctuate s elems)

infixr 5 <$>
(<$>) :: Doc -> Doc -> Doc
x <$> y | isEmpty y = x
        | otherwise = x <> line <> y

class LPretty a where
    pprint :: a -> Doc

instance LPretty [Char] where
    pprint s = text s

instance LPretty Bool where
    pprint True  = text "true"
    pprint False = text "false"

instance LPretty (Name a) where
    pprint (Name _ s) = text s

instance LPretty (Exp a) where
    pprint (Nil _)            = text "nil"
    pprint (Bool _ s)         = pprint s
    pprint (Number _ n)       = text n
    pprint (String _ s)       = dquotes (text s)
    pprint (Vararg _)         = text "..."
    pprint (EFunDef _ f)      = pprint f
    pprint (PrefixExp _ pe)   = pprint pe
    pprint (TableConst _ t)   = pprint t
    pprint (Binop _ op e1 e2) = pprint e1 <+> pprint op <+> pprint e2
    pprint (Unop _ op e)      = pprint op <> pprint e

instance LPretty (Var a) where
    pprint (VarName _ n)          = pprint n
    pprint (Select _ pe e)        = pprint pe <> brackets (pprint e)
    pprint (SelectName _ pe name) = group (pprint pe <$$> (char '.' <> pprint name))

instance LPretty (Binop a) where
    pprint Add{}    = char '+'
    pprint Sub{}    = char '-'
    pprint Mul{}    = char '*'
    pprint Div{}    = char '/'
    pprint Exp{}    = char '^'
    pprint Mod{}    = char '%'
    pprint Concat{} = text ".."
    pprint LT{}     = char '<'
    pprint LTE{}    = text "<="
    pprint GT{}     = char '>'
    pprint GTE{}    = text ">="
    pprint EQ{}     = text "=="
    pprint NEQ{}    = text "~="
    pprint And{}    = text "and"
    pprint Or{}     = text "or"

instance LPretty (Unop a) where
    pprint Neg{} = char '-'
    pprint Not{} = text "not "
    pprint Len{} = char '#'

instance LPretty (PrefixExp a) where
    pprint (PEVar _ var)         = pprint var
    pprint (PEFunCall _ funcall) = pprint funcall
    pprint (Paren _ e)           = parens (pprint e)

instance LPretty (Table a) where
    pprint (Table _ fields) = braces (nest 4 (cat (punctuate comma (map pprint fields))))

instance LPretty (TableField a) where
    pprint (ExpField _ e1 e2)    = brackets (pprint e1) <+> equals <+> pprint e2
    pprint (NamedField _ name e) = pprint name <+> equals <+> pprint e
    pprint (Field _ e)           = pprint e

instance LPretty (Block a) where
    pprint (Block _ stats ret)
        = case stats of
            [] -> ret'
            _  -> (foldr (<$>) empty (map pprint stats)) <$> ret'
      where ret' = case ret of
                     Nothing -> empty
                     Just e  -> nest 2 (text "return" </> (intercalate comma (map pprint e)))

instance LPretty (FunName a) where
    pprint (FunName _ name s methods) = cat (punctuate dot (map pprint $ name:s)) <> method'
      where method' = case methods of
                        Nothing -> empty
                        Just m' -> char ':' <> pprint m'

instance LPretty (FunDef a) where
    pprint (FunDef _ body) = pprint body

instance LPretty (FunBody a) where
    pprint funbody = pprintFunction Nothing funbody

pprintFunction :: Maybe Doc -> FunBody a -> Doc
pprintFunction funname (FunBody _ args vararg block)
    = group (nest 4 (funhead <$> funbody) <$> end)
  where funhead = case funname of
                    Nothing -> nest 2 (text "function" </> args')
                    Just n  -> nest 2 (text "function" </> n </> args')
        args' = parens (align (cat (punctuate (comma <> space)
                                        (map pprint (args ++ if vararg then [Name undefined "..."] else []))))) -- FIXME:
        funbody = pprint block
        end = text "end"

instance LPretty (FunCall a) where
    pprint (NormalFunCall _ pe arg)     = group (nest 4 (pprint pe <$$> pprint arg))
    pprint (MethodCall _ pe method arg) = group (nest 4 (pprint pe <$$> (colon <> pprint method) <$$> pprint arg))

instance LPretty (FunArg a) where
    pprint (Args _ exps)   = parens (nest 4 (cat (punctuate (comma <> space) (map pprint exps))))
    pprint (TableArg _ t)  = pprint t
    pprint (StringArg _ s) = dquotes (text s)

instance LPretty (Stat a) where
    pprint (Assign _ names vals)
        =   (intercalate comma (map pprint names))
        <+> equals
        <+> (intercalate comma (map pprint vals))
    pprint (FunCall _ funcall) = pprint funcall
    pprint (Label _ name)      = text "::" <> pprint name <> text "::"
    pprint (Break _)           = text "break"
    pprint (Goto _ name)       = text "goto" <+> pprint name
    pprint (Do _ block)        = group (nest 4 (text "do" <$> pprint block) <$> text "end")
    pprint (While _ guard e)
        =  (nest 4 (text "while" <+> pprint guard <+> text "do"
                   </> indent 4 (pprint e)))
       </> text "end"
    pprint (Repeat _ block guard)
        = nest 4 (text "repeat" </> pprint block) </> (nest 4 (text "until" </> pprint guard))

    pprint (If _ cases elsePart) = group (printIf cases elsePart)
      where printIf ((guard, block):xs) e
                =   group (nest 4 (text "if" <+> pprint guard <+> text "then"
                        <$> pprint block))
                <$> printIf' xs e

            printIf' [] Nothing  = text "end"
            printIf' [] (Just b) = group (nest 4 (text "else" </> pprint b)
                                         <$> text "end")
            printIf' ((guard, block):xs) e
                =   group (nest 4 (text "elseif" <+> pprint guard <+> text "then"
                        <$> pprint block))
                <$> printIf' xs e

    pprint (ForRange _ name e1 e2 e3 block)
        =   text "for" <+> pprint name <> equals <> pprint e1 <> comma <> pprint e2 <> e3' <+> text "do"
        <$> indent 4 (pprint block)
        <$> text "end"
      where e3' = case e3 of
                    Nothing -> empty
                    Just e  -> comma <> pprint e

    pprint (ForIn _ names exps block)
        =   text "for" <+> (intercalate comma (map pprint names))
                <+> text "in" <+> (intercalate comma (map pprint exps)) <+> text "do"
        <$> indent 4 (pprint block)
        <$> text "end"

    pprint (FunAssign _ name body) = pprintFunction (Just (pprint name)) body
    pprint (LocalFunAssign _ name body) = text "local" <+> pprintFunction (Just (pprint name)) body
    pprint (LocalAssign _ names exps)
        = text "local" <+> (intercalate comma (map pprint names)) <+> equals <+> exps'
      where exps' = case exps of
                      Nothing -> empty
                      Just es -> intercalate comma (map pprint es)
    pprint EmptyStat{} = empty
