module Langauge.Lua.Test where

import Prelude hiding (EQ, LT, GT)

import Test.QuickCheck hiding (Args)
import Control.Applicative

import qualified Language.Lua.Parser as P
import qualified Language.Lua.PrettyPrinter as PP
import qualified Language.Lua.Lexer as L
import Language.Lua.Types

-- sample (liftA (P.parseText P.block) (liftA show (liftA (PP.pprint undefined) (arbitrary :: Gen Block))))

newtype LuaString = LuaString { unwrapLuaString :: String }

-- FIXME: either fix this or implement separate lexer tests
instance Arbitrary LuaString where
    arbitrary = LuaString <$> listOf1 (elements ['a'..'z'])

arbitraryLuaStringList :: Gen [String]
arbitraryLuaStringList = liftA unwrapLuaString <$> listOf1 arbitrary

arbitraryLuaString :: Gen String
arbitraryLuaString = unwrapLuaString <$> arbitrary

arbitraryLuaStringMaybe :: Gen (Maybe String)
arbitraryLuaStringMaybe = liftA (liftA unwrapLuaString) (arbitrary :: Gen (Maybe LuaString))

instance Arbitrary Stat where
    arbitrary = oneof [ Assign <$> arbitrary <*> arbitrary
                      , FunCall <$> arbitrary
                      , Label <$> arbitraryLuaString
                      , return Break
                      , Goto <$> arbitraryLuaString
                      , Do <$> arbitrary
                      , While <$> arbitrary <*> arbitrary
                      , Repeat <$> arbitrary <*> arbitrary
                      , If <$> listOf1 arbitrary <*> arbitrary
                      , ForRange <$> arbitraryLuaString <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                      , ForIn <$> arbitraryLuaStringList <*> arbitrary <*> arbitrary
                      , FunAssign <$> arbitrary <*> arbitrary
                      , LocalFunAssign <$> arbitraryLuaString <*> arbitrary
                      , LocalAssign <$> arbitraryLuaStringList <*> arbitrary
                      -- , return EmptyStat
                      ]

instance Arbitrary Exp where
    arbitrary = oneof [ return Nil
                      , Bool <$> arbitrary
                      , Number <$> listOf1 (elements ['0'..'9']) -- TODO: implement number lexer tests
                      , String <$> liftA ("\"" ++) (liftA ((:) '"') arbitraryLuaString)
                      , return Vararg
                      , EFunDef <$> arbitrary
                      , PrefixExp <$> arbitrary
                      , TableConst <$> arbitrary
                      , Binop <$> arbitrary <*> arbitrary <*> arbitrary
                      , Unop <$> arbitrary <*> arbitrary
                      ]

instance Arbitrary Var where
    arbitrary = oneof [ Name <$> arbitraryLuaString
                      , Select <$> arbitrary <*> arbitrary
                      , SelectName <$> arbitrary <*> arbitraryLuaString
                      ]

instance Arbitrary Binop where
    arbitrary = oneof (return <$> [Add, Sub, Mul, Div, Exp, Mod, Concat, LT, LTE, GT, GTE, EQ, NEQ, And, Or])

instance Arbitrary Unop where
    arbitrary = oneof [ return Neg
                      , return Not
                      , return Len
                      ]

instance Arbitrary PrefixExp where
    arbitrary = oneof [ PEVar <$> arbitrary
                      , PEFunCall <$> arbitrary
                      , Paren <$> arbitrary
                      ]

instance Arbitrary Table where
    arbitrary = Table <$> arbitrary

instance Arbitrary TableField where
    arbitrary = oneof [ ExpField <$> arbitrary <*> arbitrary
                      , NamedField <$> arbitraryLuaString <*> arbitrary
                      , Field <$> arbitrary
                      ]

instance Arbitrary Block where
    arbitrary = Block <$> arbitrary <*> suchThat arbitrary (\a -> case a of Nothing -> True
                                                                            Just l -> not (null l))

instance Arbitrary FunName where
    arbitrary = FunName <$> arbitraryLuaString <*> arbitraryLuaStringMaybe <*> arbitraryLuaStringList

instance Arbitrary FunDef where
    arbitrary = FunDef <$> arbitrary

instance Arbitrary FunBody where
    arbitrary = FunBody <$> arbitraryLuaStringList <*> arbitrary <*> arbitrary

instance Arbitrary FunCall where
    arbitrary = oneof [ NormalFunCall <$> arbitrary <*> arbitrary
                      , MethodCall <$> arbitrary <*> arbitraryLuaString <*> arbitrary
                      ]

instance Arbitrary FunArg where
    arbitrary = oneof [ Args <$> arbitrary
                      , TableArg <$> arbitrary
                      , StringArg <$> arbitrary
                      ]