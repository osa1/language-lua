{-# LANGUAGE DeriveGeneric, FlexibleInstances, OverloadedStrings,
             ScopedTypeVariables #-}

module Main where

import qualified Language.Lua.Annotated          as A
import qualified Language.Lua.Annotated.Lexer    as L
import qualified Language.Lua.Annotated.Simplify as S
import qualified Language.Lua.Parser             as P
import           Language.Lua.PrettyPrinter      (pprint)
import           Language.Lua.StringLiteral
import           Language.Lua.Syntax
import qualified Language.Lua.Token              as T

import qualified Text.Parsec                     as P

import           Test.QuickCheck                 hiding (Args)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import           Control.Applicative
import           Control.DeepSeq                 (deepseq, force)
import           Control.Monad                   (forM_)
import qualified Data.ByteString.Lazy            as B
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
unitTests = testGroup "Unit tests"
  [stringTests, numberTests, regressions, lua531Tests, literalDecodingTests]
  where
    lua531Tests = parseFilesTest "Parsing Lua files from Lua 5.3.1 test suite" "lua-5.3.1-tests"

propertyTests :: TestTree
propertyTests = testGroup "Property tests" [{-genPrintParse-}]

parseExps :: String -> String -> Either P.ParseError [A.Exp P.SourcePos]
parseExps file contents =
  case L.llex contents of
    Left (msg,pos) -> P.runParser (reportLexError msg pos) () file []
    Right xs -> P.runParser (many A.exp) () file xs

reportLexError :: Monad m => String -> L.AlexPosn -> P.ParsecT s u m a
reportLexError msg (L.AlexPn _ line column) =
  do pos <- P.getPosition
     P.setPosition (pos `P.setSourceLine` line `P.setSourceColumn` column)
     fail ("lexical error: " ++ msg)

literalDecodingTests :: TestTree
literalDecodingTests = testGroup "Literal codec tests"
  [ testCase "decoding"
      (do assertEqual "C escapes wrong"
              (Just "\a\b\f\n\r\t\v\\\"'")
            $ interpretStringLiteral "\"\\a\\b\\f\\n\\r\\t\\v\\\\\\\"'\""
          assertEqual "C escapes wrong"
              (Just "\a \b \f \n \r \t \v \\ \" '")
            $ interpretStringLiteral "\"\\a \\b \\f \\n \\r \\t \\v \\\\ \\\" '\""
          assertEqual "ASCII characters wrong"
              (Just "the quick brown fox jumps over the lazy dog")
            $ interpretStringLiteral "'the quick brown fox jumps over the lazy dog'"
          assertEqual "Test decimal escapes"
              (Just "\0\1\2\3\4\60\127\255")
            $ interpretStringLiteral "'\\0\\1\\2\\3\\4\\60\\127\\255'"
          assertEqual "Test hexadecimal escapes"
              (Just "\0\1\2\3\4\127\255")
            $ interpretStringLiteral "\"\\x00\\x01\\x02\\x03\\x04\\x7f\\xff\""
          assertEqual "Test UTF-8 encoding"
              (Just "\230\177\137\229\173\151")
            $ interpretStringLiteral "'汉字'"
          assertEqual "Test unicode escape"
              (Just "\0 \16 \230\177\137\229\173\151")
            $ interpretStringLiteral "'\\u{0} \\u{10} \\u{6c49}\\u{5b57}'"
          assertEqual "Test continued line"
              (Just "hello\nworld")
            $ interpretStringLiteral "\"hello\\\nworld\""
          assertEqual "Test skipped whitespace"
              (Just "helloworld")
            $ interpretStringLiteral "'hello\\z  \n \f \t \r \v   world'"
          assertEqual "Long-quote leading newline"
              (Just "line1\nline2\n")
            $ interpretStringLiteral "[===[\nline1\nline2\n]===]"
          assertEqual "Long-quote without leading newline"
              (Just "line1\nline2\n")
            $ interpretStringLiteral "[===[line1\nline2\n]===]"
          assertEqual "Long-quote no escapes"
              (Just "\\0\\x00\\u{000}")
            $ interpretStringLiteral "[===[\\0\\x00\\u{000}]===]"
          assertEqual "Empty single quoted"
              (Just "")
            $ interpretStringLiteral "''"
          assertEqual "Empty double quoted"
              (Just "")
            $ interpretStringLiteral "\"\""
          assertEqual "Empty long quoted"
              (Just "")
            $ interpretStringLiteral "[[]]"
      )
  , testCase "encoding"
      (do assertEqual "Empty string"
              "\"\""
            $ constructStringLiteral ""
          assertEqual "Normal escapes"
              "\"\\a\\b\\f\\n\\r\\t\\v\\\\\\\"\""
            $ constructStringLiteral "\a\b\f\n\r\t\v\\\""
          assertEqual "Exhaustive test"
              "\"\\x00\\x01\\x02\\x03\\x04\\x05\\x06\\a\
              \\\b\\t\\n\\v\\f\\r\\x0e\\x0f\
              \\\x10\\x11\\x12\\x13\\x14\\x15\\x16\\x17\
              \\\x18\\x19\\x1a\\x1b\\x1c\\x1d\\x1e\\x1f\
              \ !\\\"#$%&'()*+,-./0123456789:;<=>?@\
              \ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\\\]^_`\
              \abcdefghijklmnopqrstuvwxyz{|}~\\x7f\
              \\\x80\\x81\\x82\\x83\\x84\\x85\\x86\\x87\
              \\\x88\\x89\\x8a\\x8b\\x8c\\x8d\\x8e\\x8f\
              \\\x90\\x91\\x92\\x93\\x94\\x95\\x96\\x97\
              \\\x98\\x99\\x9a\\x9b\\x9c\\x9d\\x9e\\x9f\
              \\\xa0\\xa1\\xa2\\xa3\\xa4\\xa5\\xa6\\xa7\
              \\\xa8\\xa9\\xaa\\xab\\xac\\xad\\xae\\xaf\
              \\\xb0\\xb1\\xb2\\xb3\\xb4\\xb5\\xb6\\xb7\
              \\\xb8\\xb9\\xba\\xbb\\xbc\\xbd\\xbe\\xbf\
              \\\xc0\\xc1\\xc2\\xc3\\xc4\\xc5\\xc6\\xc7\
              \\\xc8\\xc9\\xca\\xcb\\xcc\\xcd\\xce\\xcf\
              \\\xd0\\xd1\\xd2\\xd3\\xd4\\xd5\\xd6\\xd7\
              \\\xd8\\xd9\\xda\\xdb\\xdc\\xdd\\xde\\xdf\
              \\\xe0\\xe1\\xe2\\xe3\\xe4\\xe5\\xe6\\xe7\
              \\\xe8\\xe9\\xea\\xeb\\xec\\xed\\xee\\xef\
              \\\xf0\\xf1\\xf2\\xf3\\xf4\\xf5\\xf6\\xf7\
              \\\xf8\\xf9\\xfa\\xfb\\xfc\\xfd\\xfe\\xff\""
             (constructStringLiteral (B.pack [0..255]))

      )
  ]

stringTests :: TestTree
stringTests = testGroup "String tests"
    [ testCase
        "Equal strings from 5.3.1 reference manual"
        (do let file = "tests/strings"
            contents <- readFile file
            case parseExps file contents of
              Left parseErr -> assertFailure (show parseErr)
              Right exps -> do
                assertBool "Wrong number of strings parsed" (length exps == 5)
                case asStrings exps of
                  Nothing -> assertFailure "Not all strings were strings"
                  Just strs ->
                    forM_ strs $ \str ->
                        assertEqual "String not same"
                                expected $ interpretStringLiteral str)
    , testCase
        "Round-trip through the pretty-printer"
       (do let file = "tests/string-literal-roundtrip.lua"
           contents <- readFile file
           case P.parseText P.chunk contents of
             Left parseErr -> assertFailure (show parseErr)
             Right x -> assertEqual
                          "pretty printer didn't preserve"
                          contents
                          (show (pprint x) ++ "\n"))
                        -- text file lines always end in a newline
                        -- but the pretty printer doesn't know this
    ]
  where
    expected = Just "alo\n123\""
    asString (String s) = Just s
    asString _          = Nothing

    asStrings = mapM (asString . S.sExp)

numberTests :: TestTree
numberTests = testGroup "Number tests"
    [ testCase
        "Numbers from 5.3.1 reference manual"
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
    [ testCase "Lexing comment with text \"EOF\" in it" $
        assertEqual "Lexing is wrong" (Right [(T.LTokEof, L.AlexPn (-1) (-1) (-1))]) (L.llex "--EOF")
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
    , testCase "Lexing Lua string: '\\\\\"'" $ do
        assertEqual "String lexed wrong"
          (Right [T.LTokSLit "'\\\\\"'", T.LTokEof]) (fmap (map fst) $ L.llex "'\\\\\"'")
    , testCase "Lexing long literal `[====[ ... ]====]`" $
        show (L.llex "[=[]]=]") `deepseq` return ()
    , testCase "Handling \\z" $
        show (L.llex "\"\\z\n  \"") `deepseq` return ()
    , testCase "varlist parser shouldn't accept empty list of variables in local declarations" $
        assertParseFailure (P.parseText P.stat "local = test")
    , testCase "explist parser shouldn't accept empty list of expressions in local declarations" $
        assertParseFailure (P.parseText P.stat "local x =")
    , testCase "empty varlist and explist in single assignment, should be rejected" $
        assertParseFailure (P.parseText P.stat "local =")
    , testCase "varlist parser shouldn't accept empty list of expressions in global declarations" $
        assertParseFailure (P.parseText P.stat "= test")
    , testCase "explist parsers shouldn't accept empty list of expressions in global declarations" $
        assertParseFailure (P.parseText P.stat "x =")
    , testCase "empty list of return values should be accpeted" $
        assertEqual "Parsed wrong" (Right $ Block [] (Just [])) (P.parseText P.chunk "return")
    , testCase "Long comments should start immediately after --" $ do
        assertEqual "Parsed wrong" (Right $ Block [] Nothing)
          (P.parseText P.chunk "--[[ line1\nline2 ]]")
        assertParseFailure (P.parseText P.chunk "-- [[ line1\nline2 ]]")
    , testCase "Print EmptyStat for disambiguation" $ ppChunk "f();(f)()"
    , testCase "printing negation chains" $
        let exp = Unop Neg (Unop Neg (Unop Neg (Number "120")))
         in assertEqual "Parsed and/or printed wrong"
                        (Right exp)
                        (P.parseText P.exp (show (pprint exp)))
    ]
  where
    pp :: String -> Assertion
    pp = ppTest (P.parseText P.exp) (show . pprint)

    ppChunk :: String -> Assertion
    ppChunk = ppTest (P.parseText P.chunk) (show . pprint)

    ppTest :: Show err => (String -> Either err ret) -> (ret -> String) -> String -> Assertion
    ppTest parser printer str =
      case parser str of
        Left err -> assertFailure $ "Parsing failed: " ++ show err
        Right expr' ->
          assertEqual "Printed string is not equal to original one modulo whitespace"
            (filter (not . isSpace) str) (filter (not . isSpace) (printer expr'))

    assertParseFailure (Left _parseError) = return ()
    assertParseFailure (Right ret) = assertFailure $ "Unexpected parse: " ++ show ret

parseFilesTest :: String -> FilePath -> TestTree
parseFilesTest msg root = testCase msg $ do
  luaFiles <- map (root </>) . filter ((==) ".lua" . takeExtension) <$> getDirectoryContents root
  putStrLn $ "Trying to parse " ++ show (length luaFiles) ++ " Lua files."
  forM_ luaFiles $ \luaFile -> do
    putStrLn $ "Parsing file: " ++ luaFile
    ret <- P.parseFile luaFile
    case ret of
      Left err -> assertFailure ("Parser error in " ++ luaFile ++ ": " ++ show err)
      Right st -> -- force st `seq` return ()
        let printed = show (pprint st)
         in case P.parseText P.chunk printed of
              Left err ->
                assertFailure ("Parser error while parsing printed version of "
                               ++ luaFile ++ ": " ++ show err ++ "\nPrinted file:\n"
                               ++ printed)
              Right st' ->
                force st' `seq` return ()

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
    , return EmptyStat
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
