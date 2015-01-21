{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, DeriveGeneric #-}

-- | Lua 5.2 syntax tree, as specified in <http://www.lua.org/manual/5.2/manual.html#9>.
-- Annotation implementation is inspired by haskell-src-exts.
module Language.Lua.Annotated.Syntax where

import           Control.DeepSeq (NFData)
import           Data.Data       (Data, Typeable)
import           GHC.Generics    (Generic)
import           Prelude         hiding (EQ, GT, LT)

data Name a = Name a String deriving (Show, Eq, Functor, Data, Typeable, Generic)

data Stat a
    = Assign a [Var a] [Exp a] -- ^var1, var2 .. = exp1, exp2 ..
    | FunCall a (FunCall a) -- ^function call
    | Label a (Name a) -- ^label for goto
    | Break a -- ^break
    | Goto a (Name a) -- ^goto label
    | Do a (Block a) -- ^do .. end
    | While a (Exp a) (Block a) -- ^while .. do .. end
    | Repeat a (Block a) (Exp a) -- ^repeat .. until ..
    | If a [(Exp a, Block a)] (Maybe (Block a)) -- ^if .. then .. [elseif ..] [else ..] end
    | ForRange a (Name a) (Exp a) (Exp a) (Maybe (Exp a)) (Block a) -- ^for x=start, end [, step] do .. end
    | ForIn a [Name a] [Exp a] (Block a) -- ^for x in .. do .. end
    | FunAssign a (FunName a) (FunBody a) -- ^function \<var\> (..) .. end
    | LocalFunAssign a (Name a) (FunBody a) -- ^local function \<var\> (..) .. end
    | LocalAssign a [Name a] (Maybe [Exp a]) -- ^local var1, var2 .. = exp1, exp2 ..
    | EmptyStat a -- ^/;/
    deriving (Show, Eq, Functor, Data, Typeable, Generic)

data Exp a
    = Nil a
    | Bool a Bool
    | Number a String
    | String a String
    | Vararg a -- ^/.../
    | EFunDef a (FunDef a) -- ^/function (..) .. end/
    | PrefixExp a (PrefixExp a)
    | TableConst a (Table a) -- ^table constructor
    | Binop a (Binop a) (Exp a) (Exp a) -- ^binary operators, /+ - * ^ % .. < <= > >= == ~= and or/
    | Unop a (Unop a) (Exp a) -- ^unary operators, /- not #/
    deriving (Show, Eq, Functor, Data, Typeable, Generic)

data Var a
    = VarName a (Name a) -- ^variable
    | Select a (PrefixExp a) (Exp a) -- ^/table[exp]/
    | SelectName a (PrefixExp a) (Name a) -- ^/table.variable/
    deriving (Show, Eq, Functor, Data, Typeable, Generic)

data Binop a = Add a | Sub a | Mul a | Div a | Exp a | Mod a | Concat a
    | LT a | LTE a | GT a | GTE a | EQ a | NEQ a | And a | Or a
    deriving (Show, Eq, Functor, Data, Typeable, Generic)

data Unop a = Neg a | Not a | Len a
    deriving (Show, Eq, Functor, Data, Typeable, Generic)

data PrefixExp a
    = PEVar a (Var a)
    | PEFunCall a (FunCall a)
    | Paren a (Exp a)
    deriving (Show, Eq, Functor, Data, Typeable, Generic)

data Table a = Table a [TableField a] -- ^list of table fields
    deriving (Show, Eq, Functor, Data, Typeable, Generic)

data TableField a
    = ExpField a (Exp a) (Exp a) -- ^/[exp] = exp/
    | NamedField a (Name a) (Exp a) -- ^/name = exp/
    | Field a (Exp a)
    deriving (Show, Eq, Functor, Data, Typeable, Generic)

-- | A block is list of statements with optional return statement.
data Block a = Block a [Stat a] (Maybe [Exp a])
    deriving (Show, Eq, Functor, Data, Typeable, Generic)

data FunName a = FunName a (Name a) [Name a] (Maybe (Name a))
    deriving (Show, Eq, Functor, Data, Typeable, Generic)

data FunDef a = FunDef a (FunBody a)
    deriving (Show, Eq, Functor, Data, Typeable, Generic)

data FunBody a = FunBody a [Name a] (Maybe a) (Block a) -- ^(args, vararg, block)
    deriving (Show, Eq, Functor, Data, Typeable, Generic)

data FunCall a
    = NormalFunCall a (PrefixExp a) (FunArg a) -- ^/prefixexp ( funarg )/
    | MethodCall a (PrefixExp a) (Name a) (FunArg a) -- ^/prefixexp : name ( funarg )/
    deriving (Show, Eq, Functor, Data, Typeable, Generic)

data FunArg a
    = Args a [Exp a] -- ^list of args
    | TableArg a (Table a) -- ^table constructor
    | StringArg a String -- ^string
    deriving (Show, Eq, Functor, Data, Typeable, Generic)


class Functor ast => Annotated ast where
    -- | Retrieve the annotation of an AST node.
    ann :: ast l -> l
    -- | Change the annotation of an AST node. Note that only the annotation of
    --   the node itself is affected, and not the annotations of any child nodes.
    --   if all nodes in the AST tree are to be affected, use 'fmap'.
    amap :: (l -> l) -> ast l -> ast l

instance Annotated Stat where
    ann (Assign a _ _) = a
    ann (FunCall a _) = a
    ann (Label a _) = a
    ann (Break a) = a
    ann (Goto a _) = a
    ann (Do a _) = a
    ann (While a _ _) = a
    ann (Repeat a _ _) = a
    ann (If a _ _) = a
    ann (ForRange a _ _ _ _ _) = a
    ann (ForIn a _ _ _) = a
    ann (FunAssign a _ _) = a
    ann (LocalFunAssign a _ _) = a
    ann (LocalAssign a _ _) = a
    ann (EmptyStat a) = a

    amap f (Assign a x1 x2) = Assign (f a) x1 x2
    amap f (FunCall a x1) = FunCall (f a) x1
    amap f (Label a x1) = Label (f a) x1
    amap f (Break a) = Break (f a)
    amap f (Goto a x1) = Goto (f a) x1
    amap f (Do a x1) = Do (f a) x1
    amap f (While a x1 x2) = While (f a) x1 x2
    amap f (Repeat a x1 x2) = Repeat (f a) x1 x2
    amap f (If a x1 x2) = If (f a) x1 x2
    amap f (ForRange a x1 x2 x3 x4 x5) = ForRange (f a) x1 x2 x3 x4 x5
    amap f (ForIn a x1 x2 x3) = ForIn (f a) x1 x2 x3
    amap f (FunAssign a x1 x2) = FunAssign (f a) x1 x2
    amap f (LocalFunAssign a x1 x2) = LocalFunAssign (f a) x1 x2
    amap f (LocalAssign a x1 x2) = LocalAssign (f a) x1 x2
    amap f (EmptyStat a) = EmptyStat (f a)

instance Annotated Exp where
    ann (Nil a) = a
    ann (Bool a _) = a
    ann (Number a _) = a
    ann (String a _) = a
    ann (Vararg a) = a
    ann (EFunDef a _) = a
    ann (PrefixExp a _) = a
    ann (TableConst a _) = a
    ann (Binop a _ _ _) = a
    ann (Unop a _ _) = a

    amap f (Nil a) = Nil (f a)
    amap f (Bool a x1) = Bool (f a) x1
    amap f (Number a x1) = Number (f a) x1
    amap f (String a x1) = String (f a) x1
    amap f (Vararg a) = Vararg (f a)
    amap f (EFunDef a x1) = EFunDef (f a) x1
    amap f (PrefixExp a x1) = PrefixExp (f a) x1
    amap f (TableConst a x1) = TableConst (f a) x1
    amap f (Binop a x1 x2 x3) = Binop (f a) x1 x2 x3
    amap f (Unop a x1 x2) = Unop (f a) x1 x2

instance Annotated Var where
    ann (VarName a _) = a
    ann (Select a _ _) = a
    ann (SelectName a _ _) = a

    amap f (VarName a x1) = VarName (f a) x1
    amap f (Select a x1 x2) = Select (f a) x1 x2
    amap f (SelectName a x1 x2) = SelectName (f a) x1 x2

instance Annotated Binop where
    ann (Add a) = a
    ann (Sub a) = a
    ann (Mul a) = a
    ann (Div a) = a
    ann (Exp a) = a
    ann (Mod a) = a
    ann (Concat a) = a
    ann (LT a) = a
    ann (LTE a) = a
    ann (GT a) = a
    ann (GTE a) = a
    ann (EQ a) = a
    ann (NEQ a) = a
    ann (And a) = a
    ann (Or a) = a

    amap f (Add a) = Add (f a)
    amap f (Sub a) = Sub (f a)
    amap f (Mul a) = Mul (f a)
    amap f (Div a) = Div (f a)
    amap f (Exp a) = Exp (f a)
    amap f (Mod a) = Mod (f a)
    amap f (Concat a) = Concat (f a)
    amap f (LT a) = LT (f a)
    amap f (LTE a) = LTE (f a)
    amap f (GT a) = GT (f a)
    amap f (GTE a) = GTE (f a)
    amap f (EQ a) = EQ (f a)
    amap f (NEQ a) = NEQ (f a)
    amap f (And a) = And (f a)
    amap f (Or a) = Or (f a)

instance Annotated Unop where
    ann (Neg a) = a
    ann (Not a) = a
    ann (Len a) = a

    amap f (Neg a) = Neg (f a)
    amap f (Not a) = Not (f a)
    amap f (Len a) = Len (f a)

instance Annotated PrefixExp where
    ann (PEVar a _) = a
    ann (PEFunCall a _) = a
    ann (Paren a _) = a

    amap f (PEVar a x1) = PEVar (f a) x1
    amap f (PEFunCall a x1) = PEFunCall (f a) x1
    amap f (Paren a x1) = Paren (f a) x1

instance Annotated Table where
    ann (Table a _) = a
    amap f (Table a x1) = Table (f a) x1

instance Annotated TableField where
    ann (ExpField a _ _) = a
    ann (NamedField a _ _) = a
    ann (Field a _) = a

    amap f (ExpField a x1 x2) = ExpField (f a) x1 x2
    amap f (NamedField a x1 x2) = NamedField (f a) x1 x2
    amap f (Field a x1) = Field (f a) x1

instance Annotated Block where
    ann (Block a _ _) = a
    amap f (Block a x1 x2) = Block (f a) x1 x2

instance Annotated FunName where
    ann (FunName a _ _ _) = a
    amap f (FunName a x1 x2 x3) = FunName (f a) x1 x2 x3

instance Annotated FunDef where
    ann (FunDef a _) = a
    amap f (FunDef a x1) = FunDef (f a) x1

instance Annotated FunBody where
    ann (FunBody a _ _ _) = a
    amap f (FunBody a x1 x2 x3) = FunBody (f a) x1 x2 x3

instance Annotated FunCall where
    ann (NormalFunCall a _ _) = a
    ann (MethodCall a _ _ _) = a

    amap f (NormalFunCall a x1 x2) = NormalFunCall (f a) x1 x2
    amap f (MethodCall a x1 x2 x3) = MethodCall (f a) x1 x2 x3

instance Annotated FunArg where
    ann (Args a _) = a
    ann (TableArg a _) = a
    ann (StringArg a _) = a

    amap f (Args a x1) = Args (f a) x1
    amap f (TableArg a x1) = TableArg (f a) x1
    amap f (StringArg a x1) = StringArg (f a) x1

instance NFData a => NFData (Name a)
instance NFData a => NFData (Stat a)
instance NFData a => NFData (Exp a)
instance NFData a => NFData (Var a)
instance NFData a => NFData (Binop a)
instance NFData a => NFData (Unop a)
instance NFData a => NFData (PrefixExp a)
instance NFData a => NFData (Table a)
instance NFData a => NFData (TableField a)
instance NFData a => NFData (Block a)
instance NFData a => NFData (FunName a)
instance NFData a => NFData (FunDef a)
instance NFData a => NFData (FunBody a)
instance NFData a => NFData (FunCall a)
instance NFData a => NFData (FunArg a)
