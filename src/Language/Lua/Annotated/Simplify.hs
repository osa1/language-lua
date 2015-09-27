-- | Remove annotations.
module Language.Lua.Annotated.Simplify where

import           Data.Maybe                    (isJust)
import           Prelude                       hiding (EQ, GT, LT)

import qualified Language.Lua.Annotated.Syntax as A
import           Language.Lua.Syntax

sName :: A.Name a -> Name
sName (A.Name _ s) = s

sStat :: A.Stat a -> Stat
sStat (A.Assign _ vs es) = Assign (map sVar vs) (map sExp es)
sStat (A.FunCall _ fc) = FunCall (sFunCall fc)
sStat (A.Label _ n) = Label (sName n)
sStat (A.Break _) = Break
sStat (A.Goto _ n) = Goto (sName n)
sStat (A.Do _ b) = Do (sBlock b)
sStat (A.While _ e b) = While (sExp e) (sBlock b)
sStat (A.Repeat _ b e) = Repeat (sBlock b) (sExp e)
sStat (A.If _ conds else_) = If (map sCond conds) (fmap sBlock else_)
  where sCond (e, b) = (sExp e, sBlock b)
sStat (A.ForRange _ n e1 e2 oe3 b) =
    ForRange (sName n) (sExp e1) (sExp e2) (fmap sExp oe3) (sBlock b)
sStat (A.ForIn _ ns es b) = ForIn (map sName ns) (map sExp es) (sBlock b)
sStat (A.FunAssign _ fn fb) = FunAssign (sFunName fn) (sFunBody fb)
sStat (A.LocalFunAssign _ n fb) = LocalFunAssign (sName n) (sFunBody fb)
sStat (A.LocalAssign _ ns es) = LocalAssign (map sName ns) (fmap (map sExp) es)
sStat (A.EmptyStat _) = EmptyStat

sExp :: A.Exp a -> Exp
sExp (A.Nil _) = Nil
sExp (A.Bool _ b) = Bool b
sExp (A.Number _ n) = Number n
sExp (A.String _ s) = String s
sExp (A.Vararg _) = Vararg
sExp (A.EFunDef _ fd) = EFunDef (sFunDef fd)
sExp (A.PrefixExp _ pe) = PrefixExp (sPrefixExp pe)
sExp (A.TableConst _ t) = TableConst (sTable t)
sExp (A.Binop _ bop e1 e2) = Binop (sBinop bop) (sExp e1) (sExp e2)
sExp (A.Unop _ uop e) = Unop (sUnop uop) (sExp e)

sBlock :: A.Block a -> Block
sBlock (A.Block _ ss oes) = Block (map sStat ss) (fmap (map sExp) oes)

sVar :: A.Var a -> Var
sVar (A.VarName _ n) = VarName (sName n)
sVar (A.Select _ pe e) = Select (sPrefixExp pe) (sExp e)
sVar (A.SelectName _ pe n) = SelectName (sPrefixExp pe) (sName n)

sFunCall :: A.FunCall a -> FunCall
sFunCall (A.NormalFunCall _ pe a) = NormalFunCall (sPrefixExp pe) (sFunArg a)
sFunCall (A.MethodCall _ pe n a) = MethodCall (sPrefixExp pe) (sName n) (sFunArg a)

sFunName :: A.FunName a -> FunName
sFunName (A.FunName _ n ns on) = FunName (sName n) (map sName ns) (fmap sName on)

sFunBody :: A.FunBody a -> FunBody
sFunBody (A.FunBody _ ns vararg b) =
    FunBody (map sName ns) (isJust vararg) (sBlock b)

sFunDef :: A.FunDef a -> FunBody
sFunDef (A.FunDef _ fb) = sFunBody fb

sPrefixExp :: A.PrefixExp a -> PrefixExp
sPrefixExp (A.PEVar _ v) = PEVar (sVar v)
sPrefixExp (A.PEFunCall _ fc) = PEFunCall (sFunCall fc)
sPrefixExp (A.Paren _ e) = Paren (sExp e)

sTable :: A.Table a -> [TableField]
sTable (A.Table _ fs) = map sTableField fs

sBinop :: A.Binop a -> Binop
sBinop A.Add{} = Add
sBinop A.Sub{} = Sub
sBinop A.Mul{} = Mul
sBinop A.Div{} = Div
sBinop A.IDiv{} = IDiv
sBinop A.Exp{} = Exp
sBinop A.Mod{} = Mod
sBinop A.Concat{} = Concat
sBinop A.LT{} = LT
sBinop A.LTE{} = LTE
sBinop A.GT{} = GT
sBinop A.GTE{} = GTE
sBinop A.EQ{} = EQ
sBinop A.NEQ{} = NEQ
sBinop A.And{} = And
sBinop A.Or{} = Or
sBinop A.BAnd{} = BAnd
sBinop A.BOr{} = BOr
sBinop A.BXor{} = BXor
sBinop A.ShiftL{} = ShiftL
sBinop A.ShiftR{} = ShiftR

sUnop :: A.Unop a -> Unop
sUnop A.Neg{} = Neg
sUnop A.Not{} = Not
sUnop A.Len{} = Len
sUnop A.Complement{} = Complement

sFunArg :: A.FunArg a -> FunArg
sFunArg (A.Args _ es) = Args (map sExp es)
sFunArg (A.TableArg _ t) = TableArg (sTable t)
sFunArg (A.StringArg _ s) = StringArg s

sTableField :: A.TableField a -> TableField
sTableField (A.ExpField _ e1 e2) = ExpField (sExp e1) (sExp e2)
sTableField (A.NamedField _ n e) = NamedField (sName n) (sExp e)
sTableField (A.Field _ e) = Field (sExp e)
