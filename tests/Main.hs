{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric, FlexibleInstances, ScopedTypeVariables,
             StandaloneDeriving #-}

module Main where

import qualified Language.Lua.Annotated          as A
import qualified Language.Lua.Annotated.Lexer    as L
import qualified Language.Lua.Annotated.Simplify as S
import qualified Language.Lua.Parser             as P
import           Language.Lua.PrettyPrinter      (pprint)
import           Language.Lua.Syntax
import qualified Language.Lua.Token              as T

import qualified Text.Parsec                     as P

import           Test.QuickCheck                 hiding (Args)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import           Control.Applicative
import           Control.DeepSeq                 (deepseq)
import           Control.Monad                   (forM_)
import           Data.Char                       (isSpace)
import           GHC.Generics
import           Prelude                         hiding (Ordering (..), exp)

import           System.Directory                (getDirectoryContents)
import           System.FilePath

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, propertyTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests" [stringTests, numberTests, regressions, lua522Tests]
  where
    lua522Tests = parseFilesTest "Parsing Lua files from Lua 5.2.2 test suite" "lua-5.2.2-tests"

propertyTests :: TestTree
propertyTests = testGroup "Property tests" [{-genPrintParse-}]

parseExps :: String -> String -> Either P.ParseError [A.Exp P.SourcePos]
parseExps file contents = P.runParser (many A.exp) () file (L.llex contents)

stringTests :: TestTree
stringTests = testGroup "String tests"
    [ testCase
        "Equal strings from 5.2.2 reference manual"
        (do let file = "tests/strings"
            contents <- readFile file
            case parseExps file contents of
              Left parseErr -> assertFailure (show parseErr)
              Right exps -> do
                assertBool "Wrong number of strings parsed" (length exps == 5)
                assertEqTrans $ map S.sExp exps)
    ]
  where
    assertEqTrans :: [Exp] -> Assertion
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
                forM_ exps (assertNumber . S.sExp))
    ]
  where
    assertNumber :: Exp -> Assertion
    assertNumber Number{} = return ()
    assertNumber nan      = assertFailure ("Not a number: " ++ show nan)

regressions :: TestTree
regressions = testGroup "Regression tests"
    [ testCase "Lexing comment with text \"EOF\" in it" $ do
        assertEqual "Lexing is wrong" [(T.LTokEof, L.AlexPn (-1) (-1) (-1))] (L.llex "--EOF")
    , testCase "Binary/unary operator parsing/printing" $ do
        pp "2^3^2 == 2^(3^2)"
        pp "2^3*4 == (2^3)*4"
        pp "2^-2 == 1/4 and -2^- -2 == - - -4"
        pp "not nil and 2 and not(2>3 or 3<2)"
        pp "-3-1-5 == 0+0-9"
        pp "-2^2 == -4 and (-2)^2 == 4 and 2*2-3-1 == 0"
        pp "2*1+3/3 == 3 and 1+2 .. 3*1 == \"33\""
        pp "not(2+1 > 3*1) and \"a\"..\"b\" > \"a\""
        pp "not ((true or false) and nil)"
        pp "true or false  and nil"
        pp "(((1 or false) and true) or false) == true"
        pp "(((nil and true) or false) and true) == false"
    , testCase "Lexing unnecessarily escaped quotes" $ do
        show (L.llex "'\\\"'") `deepseq` return ()
        show (L.llex "\"\\\'\"") `deepseq` return ()
    ]
  where
    pp :: String -> Assertion
    pp expr =
      case P.parseText P.exp expr of
        Left err -> assertFailure $ "Parsing failed: " ++ show err
        Right expr' ->
          assertEqual "Printed string is not equal to original one modulo whitespace"
            (filter (not . isSpace) expr) (filter (not . isSpace) (show $ pprint expr'))


parseFilesTest :: String -> FilePath -> TestTree
parseFilesTest msg root = testCase msg $ do
  luaFiles <- map (root </>) . filter ((==) ".lua" . takeExtension) <$> getDirectoryContents root
  putStrLn $ "Trying to parse " ++ show (length luaFiles) ++ " Lua files."
  forM_ luaFiles $ \luaFile -> do
    putStrLn $ "Parsing file: " ++ luaFile
    ret <- P.parseFile luaFile
    case ret of
      Left err -> assertFailure ("Parser error in " ++ luaFile ++ ": " ++ show err)
      Right _  -> return ()

genPrintParse :: TestTree
genPrintParse =
    localOption (QuickCheckTests 10)
  . localOption (mkTimeout 100000)
  . localOption (QuickCheckMaxSize 2)
  $ testGroup "Generate-Print-Parse" [ testProperty "forall l, (parse . pprint) l = l" prop ]
  where
    prop :: Property
    prop = forAll arbitrary printAndParseEq

    printAndParseEq :: Block -> Property
    printAndParseEq b = Right b === (P.parseText P.chunk . show . pprint) b

instance Eq P.ParseError where
    _ == _ = False

-- * Arbitrary instances

newtype LuaString = LuaString { unwrapLuaString :: String } -- deriving (Generic)

-- FIXME: either fix this or implement separate lexer tests
instance Arbitrary LuaString where
  arbitrary = LuaString <$> listOf1 (elements ['a'..'z'])
  shrink = recursivelyShrink

arbitraryLuaStringList :: Gen [String]
arbitraryLuaStringList = liftA unwrapLuaString <$> listOf1 arbitrary

arbitraryLuaString :: Gen String
arbitraryLuaString = unwrapLuaString <$> arbitrary

instance Arbitrary Stat where
  arbitrary = oneof
    [ Assign <$> arbitrary <*> arbitrary
    , FunCall <$> arbitrary
    , Label <$> arbitrary
    , return Break
    , Goto <$> arbitrary
    , Do <$> arbitrary
    , While <$> arbitrary <*> arbitrary
    , Repeat <$> arbitrary <*> arbitrary
    , If <$> listOf1 arbitrary <*> arbitrary
    , ForRange <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    , ForIn <$> listOf1 arbitrary <*> arbitrary <*> arbitrary
    , FunAssign <$> arbitrary <*> arbitrary
    , LocalFunAssign <$> arbitrary <*> arbitrary
    , LocalAssign <$> listOf1 arbitrary <*> arbitrary
    -- Don't generate EmptyState - it's not printed by pretty-printer
    -- , return $ EmptyStat ()
    ]
  shrink = recursivelyShrink

instance Arbitrary Exp where
  arbitrary = oneof
    [ return Nil
    , Bool <$> arbitrary
    , Number <$> listOf1 (elements ['0'..'9']) -- TODO: implement number lexer tests
    , String <$> arbitraryLuaString
    , return Vararg
    , EFunDef <$> arbitrary
    , PrefixExp <$> arbitrary
    , TableConst <$> arbitrary
    , Binop <$> arbitrary <*> arbitrary <*> arbitrary
    , Unop <$> arbitrary <*> expNotUnop
    ]
  shrink = recursivelyShrink

-- | Any expression except Unop. (see #2)
expNotUnop :: Gen Exp
expNotUnop = suchThat arbitrary notUnop
  where
    notUnop :: Exp -> Bool
    notUnop Unop{} = False
    notUnop _      = True

instance Arbitrary Var where
  arbitrary = oneof
    [ VarName <$> arbitrary
    , Select <$> arbitrary <*> arbitrary
    , SelectName <$> arbitrary <*> arbitrary
    ]
  shrink = recursivelyShrink

instance Arbitrary Binop where
  arbitrary = oneof $
    map return [Add, Sub, Mul, Div, Exp, Mod, Concat, LT, LTE, GT, GTE, EQ, NEQ, And, Or]
  shrink = recursivelyShrink

instance Arbitrary Unop where
  arbitrary = oneof
    [ return Neg
    , return Not
    , return Len
    ]
  shrink = recursivelyShrink

instance Arbitrary PrefixExp where
  arbitrary = oneof
    [ PEVar <$> arbitrary
    , PEFunCall <$> arbitrary
    , Paren <$> arbitrary
    ]
  shrink = recursivelyShrink

instance Arbitrary TableField where
  arbitrary = oneof
    [ ExpField <$> arbitrary <*> arbitrary
    , NamedField <$> arbitrary <*> arbitrary
    , Field <$> arbitrary
    ]
  shrink = recursivelyShrink

instance Arbitrary Block where
  arbitrary = Block <$> arbitrary
                    <*> suchThat arbitrary (maybe True (not . null))
  shrink = recursivelyShrink

instance Arbitrary FunName where
  arbitrary = FunName <$> arbitrary <*> listOf arbitrary <*> arbitrary
  shrink = recursivelyShrink

instance Arbitrary FunBody where
  arbitrary = FunBody <$> listOf1 arbitrary <*> arbitrary <*> arbitrary
  shrink = recursivelyShrink

instance Arbitrary FunCall where
  arbitrary = oneof
    [ NormalFunCall <$> arbitrary <*> arbitrary
    , MethodCall <$> arbitrary <*> arbitrary <*> arbitrary
    ]
  shrink = recursivelyShrink

instance Arbitrary FunArg where
  arbitrary = oneof
    [ Args <$> arbitrary
    , TableArg <$> arbitrary
    , StringArg <$> arbitrary
    ]
  shrink = recursivelyShrink

-- * Generic instances

deriving instance Generic LuaString
deriving instance Generic Stat
deriving instance Generic Exp
deriving instance Generic Var
deriving instance Generic Binop
deriving instance Generic Unop
deriving instance Generic PrefixExp
deriving instance Generic TableField
deriving instance Generic Block
deriving instance Generic FunName
deriving instance Generic FunBody
deriving instance Generic FunCall
deriving instance Generic FunArg
