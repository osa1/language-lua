{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric, FlexibleInstances, StandaloneDeriving, ViewPatterns
             #-}

module Main where

import           Language.Lua.Annotated
import           Language.Lua.Annotated.Lexer
import qualified Language.Lua.Annotated.Simplify as S
import qualified Language.Lua.Parser             as P
import           Language.Lua.PrettyPrinter      (pprint)
import qualified Language.Lua.Syntax             as S

import qualified Text.Parsec                     as P

import           Test.QuickCheck                 hiding (Args)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import           Control.Applicative
import           Control.Monad                   (forM_)
import           GHC.Generics
import           Prelude                         hiding (Ordering (..), exp)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [stringTests, numberTests{-, genPrintParse-}]

parseExps :: String -> String -> Either P.ParseError [Exp P.SourcePos]
parseExps file contents = P.runParser (many exp) () file (llex contents)

stringTests :: TestTree
stringTests = testGroup "String tests"
    [ testCase
        "Strings from 5.2.2 reference manual"
        (do let file = "tests/strings"
            contents <- readFile file
            case parseExps file contents of
              Left parseErr -> assertFailure (show parseErr)
              Right exps -> do
                assertBool "Wrong number of strings parsed" (length exps == 5)
                assertEqTrans $ map (fmap (const ())) exps)
    ]
  where
    assertEqTrans :: [Exp ()] -> Assertion
    assertEqTrans [] = return ()
    assertEqTrans [_] = return ()
    assertEqTrans (a : b : rest) = do
      assertEqual "Strings are not same" a b
      assertEqTrans (b : rest)

numberTests :: TestTree
numberTests = testGroup "Number tests"
    [ testCase
        "Numbers from 5.2.2 reference manual"
        (do let file = "tests/numbers"
            contents <- readFile file
            case parseExps file contents of
              Left parseErr -> assertFailure (show parseErr)
              Right exps -> do
                assertBool "Wrong number of numbers parsed" (length exps == 9)
                forM_ exps assertNumber)
    ]
  where
    assertNumber :: Show a => Exp a -> Assertion
    assertNumber Number{} = return ()
    assertNumber nan      = assertFailure ("Not a number: " ++ show nan)

genPrintParse :: TestTree
genPrintParse =
    localOption (QuickCheckTests 10)
  . localOption (mkTimeout 1000)
  . localOption (QuickCheckMaxSize 2)
  $ testGroup "Generate-Print-Parse" [ testProperty "forall l, (parse . pprint) l = l" prop ]
  where
    prop :: Property
    -- TODO: use forallShrink
    prop = forAll arbitrary printAndParseEq

    printAndParseEq :: Block () -> Property
    printAndParseEq (S.sBlock -> b) = (P.parseText P.chunk . show . pprint) b === Right b

instance Eq P.ParseError where
    _ == _ = False

-- * Arbitrary instances

newtype LuaString = LuaString { unwrapLuaString :: String } deriving (Generic)

-- FIXME: either fix this or implement separate lexer tests
instance Arbitrary LuaString where
  arbitrary = LuaString <$> listOf1 (elements ['a'..'z'])
  shrink = recursivelyShrink

arbitraryLuaStringList :: Gen [String]
arbitraryLuaStringList = liftA unwrapLuaString <$> listOf1 arbitrary

arbitraryLuaString :: Gen String
arbitraryLuaString = unwrapLuaString <$> arbitrary

instance Arbitrary (Name ()) where
  arbitrary = Name () <$> arbitraryLuaString
  shrink = recursivelyShrink

instance Arbitrary (Stat ()) where
  arbitrary = oneof
    [ Assign () <$> arbitrary <*> arbitrary
    , FunCall () <$> arbitrary
    , Label () <$> arbitrary
    , return $ Break ()
    , Goto () <$> arbitrary
    , Do () <$> arbitrary
    , While () <$> arbitrary <*> arbitrary
    , Repeat () <$> arbitrary <*> arbitrary
    , If () <$> listOf1 arbitrary <*> arbitrary
    , ForRange () <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    , ForIn () <$> listOf1 arbitrary <*> arbitrary <*> arbitrary
    , FunAssign () <$> arbitrary <*> arbitrary
    , LocalFunAssign () <$> arbitrary <*> arbitrary
    , LocalAssign () <$> listOf1 arbitrary <*> arbitrary
    -- Don't generate EmptyState - it's not printed by pretty-printer
    -- , return $ EmptyStat ()
    ]
  shrink = recursivelyShrink

instance Arbitrary (Exp ()) where
  arbitrary = oneof
    [ return $ Nil ()
    , Bool () <$> arbitrary
    , Number () <$> listOf1 (elements ['0'..'9']) -- TODO: implement number lexer tests
    , String () <$> arbitraryLuaString
    , return $ Vararg ()
    , EFunDef () <$> arbitrary
    , PrefixExp () <$> arbitrary
    , TableConst () <$> arbitrary
    , Binop () <$> arbitrary <*> arbitrary <*> arbitrary
    , Unop () <$> arbitrary <*> expNotUnop
    ]
  shrink = recursivelyShrink

-- | Any expression except Unop. (see #2)
expNotUnop :: Gen (Exp ())
expNotUnop = suchThat arbitrary notUnop
  where
    notUnop :: Exp () -> Bool
    notUnop Unop{} = False
    notUnop _      = True

instance Arbitrary (Var ()) where
  arbitrary = oneof
    [ VarName () <$> arbitrary
    , Select () <$> arbitrary <*> arbitrary
    , SelectName () <$> arbitrary <*> arbitrary
    ]
  shrink = recursivelyShrink

instance Arbitrary (Binop ()) where
  arbitrary = oneof $ (return <$>
    map ($ ()) [Add, Sub, Mul, Div, Exp, Mod, Concat, LT, LTE, GT, GTE, EQ, NEQ, And, Or])
  shrink = recursivelyShrink

instance Arbitrary (Unop ()) where
  arbitrary = oneof
    [ return $ Neg ()
    , return $ Not ()
    , return $ Len ()
    ]
  shrink = recursivelyShrink

instance Arbitrary (PrefixExp ()) where
  arbitrary = oneof
    [ PEVar () <$> arbitrary
    , PEFunCall () <$> arbitrary
    , Paren () <$> arbitrary
    ]
  shrink = recursivelyShrink

instance Arbitrary (Table ()) where
  arbitrary = Table () <$> arbitrary
  shrink = recursivelyShrink

instance Arbitrary (TableField ()) where
  arbitrary = oneof
    [ ExpField () <$> arbitrary <*> arbitrary
    , NamedField () <$> arbitrary <*> arbitrary
    , Field () <$> arbitrary
    ]
  shrink = recursivelyShrink

instance Arbitrary (Block ()) where
  arbitrary = Block () <$> arbitrary
                       <*> suchThat arbitrary (\a -> case a of
                                                       Nothing -> True
                                                       Just l -> not (null l))
  shrink = recursivelyShrink

instance Arbitrary (FunName ()) where
  arbitrary = FunName () <$> arbitrary <*> listOf arbitrary <*> arbitrary
  shrink = recursivelyShrink

instance Arbitrary (FunDef ()) where
  arbitrary = FunDef () <$> arbitrary
  shrink = recursivelyShrink

instance Arbitrary (FunBody ()) where
  arbitrary = FunBody () <$> listOf1 arbitrary <*> arbitrary <*> arbitrary
  shrink = recursivelyShrink

instance Arbitrary (FunCall ()) where
  arbitrary = oneof
    [ NormalFunCall () <$> arbitrary <*> arbitrary
    , MethodCall () <$> arbitrary <*> arbitrary <*> arbitrary
    ]
  shrink = recursivelyShrink

instance Arbitrary (FunArg ()) where
  arbitrary = oneof
    [ Args () <$> arbitrary
    , TableArg () <$> arbitrary
    , StringArg () <$> arbitrary
    ]
  shrink = recursivelyShrink

-- * Generic instances

deriving instance Generic (Name a)
deriving instance Generic (Stat a)
deriving instance Generic (Exp a)
deriving instance Generic (Var a)
deriving instance Generic (Binop a)
deriving instance Generic (Unop a)
deriving instance Generic (PrefixExp a)
deriving instance Generic (Table a)
deriving instance Generic (TableField a)
deriving instance Generic (Block a)
deriving instance Generic (FunName a)
deriving instance Generic (FunDef a)
deriving instance Generic (FunBody a)
deriving instance Generic (FunCall a)
deriving instance Generic (FunArg a)

